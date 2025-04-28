;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Capture Jira issues using Org capture templates.

;;; Code:
(require 'ediff)
(require 'org)
(require 'org-attach)
(require 'org-capture)

(require 'aio)

(require 'jirassic-parser)
(require 'jirassic-serializer)
(require 'jirassic-client)


(declare-function jirassic--expand-roam-template-for-diff
                  "jirassic-org-roam"
                  (issue template-keys level))

(defcustom jirassic-org-add-attachments t
  "Whether to download and attach files.

If nil, the media files will be linked to the issue. If t, the media
files will be downloaded and attached to the org file via `org-attach'."
  :type 'boolean
  :group 'jirassic)

(defcustom jirassic-restore-windows-after-diff t
  "Whether to restore windows after viewing issue changes."
  :type 'boolean
  :group 'jirassic)

(defcustom jirassic-org-capture-templates
  `(("t" "Todo" entry
     (file org-default-notes-file)
     ,(concat "* %(issue-todo-state) %(issue-summary)\n"
              "%(issue-org-properties)\n"
              "%(issue-description)")))
  "Org capture templates for Jira issues."
  :type (get 'org-capture-templates 'custom-type)
  :group 'jirassic)

(define-error 'jirassic-capture-error
              "An error occurred while capturing a Jira issue.")

;;; TODO: This belongs in client
(defvar jirassic--max-parallel-downloads 5
  "Maximum number of parallel downloads.")

(defvar jirassic-last-inserted-issue nil
  "Most recently inserted Jira issue for use in hooks.

Stores a Jirassic issue object.")

(defvar jirassic--ediff-buffers-to-cleanup nil
  "List of buffers to clean up after jirassic ediff exits.")

(defvar jirassic--initial-window-configuration nil
  "Initial window configuration before ediff session.")

(defvar jirassic--issue-location-for-diff nil
  "Marker for the location of the issue being diffed.")

(defvar jirassic--template-key-property "TEMPLATE_KEYS"
  "The org property name for the template key.")

(defun jirassic-org-issue-entry-p (&optional epom)
  "Check if org entry at EPOM is a jira issue.

EPOM is an element, marker, or buffer position."
  (and
   (not (null (org-entry-get epom "issue-key")))
   (not (null (org-entry-get epom "issue-id")))))

(defun jirassic--min-heading-level-in-buffer ()
  "Find the minimum heading level in the current buffer.

Returns the minimum level found, or nil if no headings exist."
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (let (min-level)
      (org-map-entries
       (lambda ()
         (if-let ((current-level (org-current-level)))
             (setq min-level
                   (cond
                    ((null min-level) current-level)
                    (t (min min-level
                            current-level))))))
       t nil)
      min-level)))

(defun jirassic--download-attachments (attachments attachment-dir)
  "Download ATTACHMENTS to ATTACHMENT-DIR."
  (let ((sem (aio-sem jirassic--max-parallel-downloads)))
    (aio-wait-for
     (jirassic-aio-all
      (seq-map
       (lambda (a)
         (let* ((filename (alist-get 'filename a))
                (id (alist-get 'id a))
                (mimetype (alist-get 'mimeType a))
                (filetype (alist-get mimetype
                                     '(("image/png" . "png")
                                       ("image/jpeg" . "jpg")
                                       ("image/gif" . "gif")
                                       ("application/pdf" . "pdf")
                                       ("application/zip" . "zip"))
                                     nil nil #'string=))
                (attachment-filename
                 (if (and filetype
                          (not (string= (f-ext filename) filetype)))
                     (f-swap-ext filename filetype)
                   filename))
                (attachment-path
                 (expand-file-name attachment-filename
                                   attachment-dir)))
           ;; Wait until there is room in the download queue
           (aio-with-async
             (aio-await (aio-sem-wait sem))
             (prog1 (aio-await (jirassic-download-attachment
                                id attachment-path))
               (aio-sem-post sem)))))
       attachments)))))

(aio-defun jirassic--maybe-download-org-attachments ()
  (when (and jirassic-org-add-attachments
             (jirassic-org-issue-entry-p)
             buffer-file-name)
    (let ((issue-key (org-entry-get nil "issue-key"))
          (attachment-dir (org-attach-dir 'get-create)))
      (condition-case err
          (jirassic-bind-restore
              (
               ;; TODO: Cache the issue object just fetched to avoid
               ;; multiple requests.
               (issue (aio-await (jirassic-get-issue issue-key)))
               (attachment-paths
                (aio-await (jirassic--download-attachments
                            (jirassic-issue-attachments
                             issue)
                            attachment-dir))))
            (when attachment-paths
              (org-attach-tag)))
        (jirassic-client-error
         (message "Error adding attachments %s: %s"
                  (propertize issue-key 'face 'bold)
                  (jirassic-http-error-message (cdr err))))))))

(aio-defun jirassic--org-capture-finalize ()
  "Perform finalization after org capture of jira issues."
  (when (and (not org-note-abort)
             org-capture-last-stored-marker
             (buffer-live-p (marker-buffer org-capture-last-stored-marker)))
    (condition-case err
        (aio-await
         (with-current-buffer (marker-buffer org-capture-last-stored-marker)
           (goto-char (marker-position org-capture-last-stored-marker))
           (when (jirassic-org-issue-entry-p)
             (jirassic--maybe-download-org-attachments))))
      (jirassic-client-error
       (message "Error downloading attachments: %s"
                (jirassic-http-error-message (cdr err))))
      (error
       (message "Error downloading attachments: %s"
                (error-message-string err))))))

(defun jirassic--issue-ediff-cleanup ()
  "Cleanup buffers created by jirassic ediff."
  (unwind-protect
      (progn (mapc #'kill-buffer jirassic--ediff-buffers-to-cleanup)
             (when ediff-buffer-C
               (kill-buffer ediff-buffer-C))
             (when ediff-control-buffer
               (kill-buffer ediff-control-buffer))
             (when jirassic-restore-windows-after-diff
               (unless jirassic--initial-window-configuration
                 (error "Initial window configuration not saved"))
               (set-window-configuration jirassic--initial-window-configuration)))

    (setq jirassic--ediff-buffers-to-cleanup nil)
    (setq jirassic--initial-window-configuration nil)
    (setq jirassic--issue-location-for-diff nil)))

(defun jirassic--after-issue-merge ()
  "After merging the issue, save the merged issue."
  (unwind-protect
      (when (y-or-n-p "Save changes to the merged issue?")
        (unless (buffer-live-p (marker-buffer jirassic--issue-location-for-diff))
          (error "Buffer for issue no longer exists"))
        (let ((updated-issue
               (with-current-buffer ediff-buffer-C
                 (buffer-substring-no-properties (point-min) (point-max)))))
          (with-current-buffer (marker-buffer jirassic--issue-location-for-diff)
            (save-restriction
              (save-excursion
                (widen)
                (cond
                 ((org-before-first-heading-p)
                  ;; If the issue is at the file level, we need to
                  ;; replace the entire buffer.
                  (delete-region (point-min) (point-max))
                  (insert updated-issue))
                 (t
                  ;; Otherwise, we need to replace the subtree.
                  (org-back-to-heading t)
                  (org-narrow-to-subtree)
                  (let ((inhibit-read-only t))
                    (delete-region (point-min) (point-max))
                    (insert updated-issue)))))))))
    (jirassic--issue-ediff-cleanup)))

(defmacro jirassic--with-issue-context-funcs (issue &rest body)
  "Dynamically bind functions for ISSUE properties and execute BODY."
  (declare (indent 1)
           (debug (form body)))
  (let ((bindings
         `((issue-key            . (jirassic-issue-key issue))
           (issue-id             . (jirassic-issue-id issue))
           (issue-summary        . (jirassic-issue-summary issue))
           (issue-description    . (lambda (&optional level)
                                     (jirassic--serialize-doc
                                      (jirassic-issue-description issue)
                                      ;; In templates, the heading will
                                      ;; normally be at level 1 so the
                                      ;; description defaults to start
                                      ;; at level 2.
                                      (or level 2))))
           (issue-type           . (jirassic-issue-type issue))
           (issue-priority       . (jirassic-issue-priority issue))
           (issue-status         . (jirassic-issue-status issue))
           (issue-todo-state     . (jirassic--jira-to-org-status
                                    (jirassic-issue-status issue)))
           (issue-creator-name   . (jirassic-user-display-name
                                    (jirassic-issue-creator issue)))
           (issue-creator-email  . (jirassic-user-email-address
                                    (jirassic-issue-creator issue)))
           (issue-project        . (jirassic-issue-project issue))
           (issue-link           . (jirassic-issue-link issue))
           (issue-summary-slug   . (replace-regexp-in-string
                                    "[^a-zA-Z0-9_]+" "_"
                                    (downcase (jirassic-issue-summary issue))))
           (issue-org-properties
            . (lambda (&optional extra-properties)
                (jirassic--serialize-properties
                 ,issue
                 (append
                  extra-properties
                  (list (cons jirassic--template-key-property (org-capture-get :key))))))))))
    `(cl-letf ,(mapcar (lambda (binding)
                         (let ((symbol (car binding))
                               (value (cdr binding)))
                           `((symbol-function ',symbol)
                             (lambda (&rest args)
                               (if (functionp ,value)
                                   (apply ,value args)
                                 ,value)))))
                       bindings)
       ,@body)))

(defun jirassic--expand-template-for-diff (issue template-keys level)
  "Expand the specified Jira issue template for diffing.

ISSUE is the `jirassic-issue' struct with the latest data.
TEMPLATE-KEYS is the string key (e.g., \"t\", \"i\") identifying the template.

Returns the expanded template content as a string with headings
normalized to LEVEL."
  (let* ((template-defs jirassic-org-capture-templates)
         (template-found (assoc template-keys template-defs)))
    (unless template-found
      (error "Template with key '%s' not found" template-keys))

    (with-temp-buffer
      ;; Set up the context needed for template expansion, similar to capture
      (jirassic--with-issue-context-funcs issue
        (let* ((template-definition (nth 4 template-found)) ; Get the template content part
               ;; Load the template in case it's a file or function
               (template-text
                (let ((org-capture-plist `(:template ,template-definition)))
                  (ignore)
                  (org-capture-get-template)
                  (org-capture-get :template)))
               (org-capture-plist
                (plist-put org-capture-plist :key template-keys)))
          (org-mode)
          (insert (org-capture-fill-template template-text))
          ;; We are expanding the template but we want to have the
          ;; lowest level heading in the buffer to be at `level'.
          (if-let ((min-level (jirassic--min-heading-level-in-buffer))
                   (diff (- level
                            min-level)))
              (dotimes (_ diff)
                (org-map-entries
                 (if (> diff 0)
                     #'org-demote
                   #'org-promote)
                 t nil)))
          ;; There might be an `:ATTACH:' tag in the template
          (unless (or (seq-empty-p (jirassic-issue-attachments issue))
                      (null jirassic-org-add-attachments))
            (point-min)
            (org-map-entries
             (lambda ()
               (when (string= (org-entry-get nil "issue-key")
                              (jirassic-issue-key issue))
                 (org-attach-tag)))
             t nil))
          (buffer-string))))))

;;;###autoload
(defun jirassic-update-org-issue ()
  "Ediff the current Jira Org entry against the latest version.

Supports issues captured via `jirassic-org-capture' or
`jirassic-org-roam-capture'.

This function relies on the presence of `:TEMPLATE_KEYS:' or
`:ROAM_TEMPLATE_KEYS:' (when using Org-roam and `jirassic-org-roam')
which is included in the `issue-org-properties' function that's used in
Jirassic capture templates.

When one of these properties cannot be found, the function will
fall back to the default serializer."
  (interactive)
  (aio-with-async
    ;; Handle these user errors and message explicitly because
    ;; these `user-error's that happen in async interactive
    ;; functions are not reported to the user.
    (condition-case err
        (progn
          (when jirassic--issue-location-for-diff
            (user-error "Already requesting issue changes"))
          (unless (derived-mode-p 'org-mode)
            (user-error "Not in Org-mode buffer"))
          (unless (jirassic-org-issue-entry-p)
            (user-error "Not in a Jira issue entry"))
          ;; Store the window configuration before we start the ediff session
          ;; so that we can restore it later.
          (setq jirassic--initial-window-configuration (current-window-configuration)))

      (user-error
       (message (error-message-string err))
       (setq jirassic--initial-window-configuration nil)
       (signal (car err) (cdr err))))

    (setq jirassic--issue-location-for-diff (point-marker))

    (condition-case err
        (jirassic-bind-restore
            ((issue-level (or (org-current-level) 1))
             (id   (org-entry-get nil "issue-id"))
             (key  (org-entry-get nil "issue-key"))
             (template-key (or (org-entry-get nil "TEMPLATE_KEYS")
                               (org-entry-get nil "ROAM_TEMPLATE_KEYS")))
             (buf-current
              (get-buffer-create (format "*jira-%s-current*" id)))
             (buf-latest
              (get-buffer-create (format "*jira-%s-latest*" id)))
             (issue-latest (progn
                             (message "Fetching latest data for issue %s..." key)
                             (aio-await (jirassic-get-issue key))))
             (current-issue-content
              (save-restriction
                (save-excursion
                  (widen)
                  ;; The issue is either a normal org entry or a file
                  ;; level entry
                  (if (org-before-first-heading-p)
                      (buffer-string)
                    (org-narrow-to-subtree)
                    (buffer-substring-no-properties (point-min) (point-max))))))
             (latest-issue-content
              (if template-key
                  (progn
                    (message "Generating latest version using template '%s'..."
                             template-key)
                    (if (and (featurep 'jirassic-org-roam)
                             (org-entry-get nil "ROAM_TEMPLATE_KEYS"))
                        (progn
                          (jirassic--expand-roam-template-for-diff issue-latest
                                                                   template-key
                                                                   issue-level))
                      (jirassic--expand-template-for-diff issue-latest
                                                          template-key
                                                          issue-level)))
                ;; Fallback: Use the original serializer if no key found
                (progn
                  (message "No template key found, using default serializer...")
                  (jirassic--serialize-issue-entry issue-latest issue-level)))))

          (with-current-buffer buf-latest
            (let ((inhibit-read-only t))
              (erase-buffer)
              (org-mode)
              ;; Make sure that the issue is serialized at the same level as
              ;; the current issue.
              (insert latest-issue-content)))

          (with-current-buffer buf-current
            (let ((inhibit-read-only t))
              (erase-buffer)
              (org-mode)
              (insert current-issue-content)))

          (push buf-current jirassic--ediff-buffers-to-cleanup)
          (push buf-latest jirassic--ediff-buffers-to-cleanup)

          (ediff-merge-buffers buf-current buf-latest
                               (list
                                (lambda ()
                                  (setq-local ediff-quit-merge-hook
                                              (cons #'jirassic--after-issue-merge
                                                    ediff-quit-merge-hook))))))

      ;; Clean up even if there's an error.
      (jirassic-client-error
       (let ((client-error (cdr err)))
         (message "Error fetching latest issue update: %s"
                  (jirassic-http-error-message client-error))
         (jirassic--issue-ediff-cleanup)
         (signal (car err) (cdr err))))

      (error (progn
               ;; Notify the user of the error and re-raise
               (message (error-message-string err))
               (jirassic--issue-ediff-cleanup)
               (signal (car err) (cdr err)))))))

;;;###autoload
(defun jirassic-org-capture (issue-key &optional goto keys)
  "Capture a Jira issue using Org capture templates."
  (interactive
   (list (read-string "Issue key: ")
         current-prefix-arg))
  (aio-with-async
    (condition-case err
        (let* ((issue (aio-await (jirassic-get-issue issue-key)))
               (org-capture-templates jirassic-org-capture-templates))
          (jirassic--with-issue-context-funcs issue
            (org-capture goto keys)))

      (jirassic-client-error
       (message "Error fetching issue '%s': %s"
                issue-key
                (jirassic-http-error-message (cdr err)))
       (signal (car err) (cdr err)))
      (error
       (message "Error capturing issue %s: %s"
                issue-key
                (error-message-string err))
       (signal (car err) (cdr err))))))

(add-hook 'org-capture-after-finalize-hook #'jirassic--org-capture-finalize)

(provide 'jirassic-org)
;;; jirassic-org.el ends here
