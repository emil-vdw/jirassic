;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides Org Mode integration for the Jirassic package.

;;; Code:

(require 'org)
(require 'ediff)

(require 'aio)

(require 'jirassic-parser)
(require 'jirassic-serializer)
(require 'jirassic-client)


(defcustom jirassic-org-add-attachments t
  "Whether to download and attach files.

If nil, the media files will be linked to the issue. If t, the media
files will be downloaded and attached to the org file via `org-attach'.")

(defcustom jirassic-org-after-insert-hook nil
  "Hook run after inserting a Jira issue into an Org buffer."
  :type 'hook
  :group 'jirassic)

(defcustom jirassic-restore-windows-after-diff t
  "Whether to restore windows after viewing issue changes."
  :type 'boolean
  :group 'jirassic)

(defcustom jirassic-org-capture-templates
  `(("t" "Todo" entry
     (file org-default-notes-file)
     ,(concat "* %(issue-todo-state) %(issue-summary)\n"
              ":PROPERTIES:\n"
              ":issue-key: %(issue-key)\n"
              ":issue-link: %(issue-link)\n"
              ":issue-id: %(issue-id)\n"
              ":issue-type: %(issue-type)\n"
              ":issue-creator: %(issue-creator-name)\n"
              ":issue-project: %(issue-project)\n"
              ":END:\n\n%(issue-description)")))
  "Org capture templates for Jira issues."
  :type (get 'org-capture-templates 'custom-type)
  :group 'jirassic)

(define-error 'jirassic-capture-error
              "An error occurred while capturing a Jira issue.")

(defvar jirassic-last-inserted-issue nil
  "Most recently inserted Jira issue for use in hooks.

Stores a Jirassic issue object.")

(defvar jirassic-last-inserted-issue-location nil
  "Last location of the inserted issue.

This is a marker pointing to the start location of the last inserted
issue.")

(defvar jirassic--ediff-buffers-to-cleanup nil
  "List of buffers to clean up after jirassic ediff exits.")

(defvar jirassic--initial-window-configuration nil
  "Initial window configuration before ediff session.")

(defvar jirassic--issue-location-for-diff nil
  "Marker for the location of the issue being diffed.")

(defun jirassic-org-issue-entry-p (&optional epom)
  "Check if org entry at EPOM is a jira issue.

EPOM is an element, marker, or buffer position."
  (and
   (not (null (org-entry-get epom "issue-key")))
   (not (null (org-entry-get epom "issue-id")))))

(defun jirassic--maybe-download-org-attachments ()
  (when (and jirassic-org-add-attachments
             (jirassic-org-issue-entry-p)
             buffer-file-name)
    (jirassic--serialize-attachments
     (jirassic-issue-attachments jirassic-last-inserted-issue))))

(defun jirassic--org-capture-finalize ()
  "Perform finalization after org capture of jira issues."
  (when (and org-capture-last-stored-marker
             (buffer-live-p (marker-buffer org-capture-last-stored-marker)))
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (goto-char (marker-position org-capture-last-stored-marker))
      (when (jirassic-org-issue-entry-p)
        (condition-case err
            (jirassic--maybe-download-org-attachments)
          (jirassic-client-error
           (message "Error downloading attachments: %s"
                    (jirassic-http-error-message (cdr err))))
          (error
           (message "Error downloading attachments: %s"
                    (error-message-string err))))))))

(defun jirassic--issue-ediff-cleanup ()
  "Cleanup buffers created by jirassic ediff."
  (mapc #'kill-buffer jirassic--ediff-buffers-to-cleanup)
  (setq jirassic--ediff-buffers-to-cleanup nil)
  (when ediff-buffer-C
    (kill-buffer ediff-buffer-C))
  (when ediff-control-buffer
    (kill-buffer ediff-control-buffer))
  (when jirassic-restore-windows-after-diff
    (unless jirassic--initial-window-configuration
      (error "Initial window configuration not saved"))
    (set-window-configuration jirassic--initial-window-configuration))
  (setq jirassic--issue-location-for-diff nil))

(defun jirassic--after-issue-merge ()
  "After merging the issue, save the changes to the original buffer."
  (unwind-protect
      (when (y-or-n-p "Save changes to the merged issue?")
        (unless (buffer-live-p (marker-buffer jirassic--issue-location-for-diff))
          (error "Buffer for issue no longer exists"))
        (let ((updated-issue
               (with-current-buffer ediff-buffer-C
                 (buffer-substring-no-properties (point-min) (point-max)))))
          ;; XXX: What if the buffer has been closed?
          (with-current-buffer (marker-buffer jirassic--issue-location-for-diff)
            (save-restriction
              (save-excursion
                (widen)
                (org-back-to-heading t)
                (org-narrow-to-subtree)
                (let ((inhibit-read-only t))
                  (delete-region (point-min) (point-max))
                  (insert updated-issue)))))))
    (jirassic--issue-ediff-cleanup)))

;;;###autoload
(aio-defun jirassic-org-insert-issue (key &optional level)
  "Insert a Jira issue into the current buffer."
  (interactive
   (list (read-string "Enter issue key: ")
         (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (org-current-level))))

  (unless (derived-mode-p 'org-mode)
    (error "Cannot insert issue in non-org buffer"))

  (message "Fetching issue %s..." key)
  (condition-case err
      (jirassic-bind-restore ((issue (aio-await (jirassic-get-issue key))))

        ;; Make sure to return to the start of where we insert the issue
        ;; before running the hooks.
        (goto-char
         (marker-position (jirassic--serialize-issue-entry issue level)))
        (setq jirassic-last-inserted-issue issue)
        (run-hooks 'jirassic-org-after-insert-hook))
    (jirassic-client-error
     (let ((client-error (cdr err)))
       (message "Error fetching issue %s: %s"
                (propertize key 'face 'bold)
                (jirassic-http-error-message
                 client-error))))))

(defun jirassic--expand-template-for-diff (issue template-keys)
  "Expand the specified Jira issue template for diffing.

ISSUE is the `jirassic-issue' struct with the latest data.
TEMPLATE-KEYS is the string key (e.g., \"t\", \"i\") identifying the template.

Returns the expanded template content as a string."
  (let* ((template-defs jirassic-org-capture-templates)
         (template-found (assoc template-keys template-defs)))
    (unless template-found
      (error "Template key '%s' not found for type '%s'" template-keys template-type))

    (with-temp-buffer
      ;; Set up the context needed for template expansion, similar to capture
      (jirassic--with-issue-context-funcs issue
        (let* ((template-definition (nth 4 template-found)) ; Get the template content part
               (template-string (if (stringp template-definition)
                                    template-definition
                                  ;; Handle function/file templates if necessary (more complex)
                                  (error "Only string templates supported for diff currently"))))
          (insert (org-capture-fill-template template-string))
          ;; XXX: Align all headings with source buffer heading levels
          (buffer-string))))))

;;;###autoload
(aio-defun jirassic-org-update-issue-entry ()
  "Ediff the current Org-mode Jira entry against the latest version.

1. Grab `issue-id` and `issue-key` from the current Org entry properties.
2. Copy the current subtree into buffer `*jira-<ID>-current*`.
3. Fetch the issue from Jira, parse and serialize it into `*jira-<ID>-latest*`.
4. Call `ediff-buffers` on the two temp buffers."
  (interactive)

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

  (condition-case err
      (jirassic-bind-restore
          ((issue-level (org-current-level))
           (id   (org-entry-get nil "issue-id"))
           (key  (org-entry-get nil "issue-key"))
           (template-key (org-entry-get nil "TEMPLATE_KEYS"))
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
                (org-narrow-to-subtree)
                (buffer-substring-no-properties (point-min) (point-max)))))
           (latest-issue-content
            (if template-key
                (progn
                  (message "Generating latest version using template '%s'..."
                           template-key)
                  (jirassic--expand-template-for-diff issue-latest template-key))
              ;; Fallback: Use the original serializer if no key found
              (progn
                (message "No template key found, using default serializer...")
                (with-temp-buffer
                  (save-match-data
                    (org-mode) ; Ensure org context for serializer if needed
                    (jirassic--serialize-issue-entry issue-latest issue-level)
                    (buffer-string)))))))

        (setq jirassic--issue-location-for-diff (point-marker))
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
            ;; Make sure that the issue is serialized at the same level as
            ;; the current issue.
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
       (message "Error fetching issue %s: %s"
                (propertize key 'face 'bold)
                (jirassic-http-error-message client-error))
       (jirassic--issue-ediff-cleanup)
       (signal (car err) (cdr err))))

    (error (progn
             ;; Notify the user of the error and re-raise
             (message (error-message-string err))
             (jirassic--issue-ediff-cleanup)
             (signal (car err) (cdr err))))))

(defmacro jirassic--with-issue-context-funcs (issue &rest body)
  "Dynamically bind functions for ISSUE properties and execute body."
  (declare (indent 1))
  (let ((bindings
         `((issue-key           . (jirassic-issue-key issue))
           (issue-id            . (jirassic-issue-id issue))
           (issue-summary       . (jirassic-issue-summary issue))
           (issue-description   . (lambda (&optional level)
                                    (jirassic--doc-string
                                     (jirassic-issue-description issue)
                                     ;; In templates, the heading will
                                     ;; normally be at level 1 so the
                                     ;; description defaults to start
                                     ;; at level 2.
                                     (or level 2))))
           (issue-type          . (jirassic-issue-type issue))
           (issue-priority      . (jirassic-issue-priority issue))
           (issue-status        . (jirassic-issue-status issue))
           (issue-todo-state    . (jirassic--jira-to-org-status
                                   (jirassic-issue-status issue)))
           (issue-creator-name  . (jirassic-user-display-name
                                   (jirassic-issue-creator issue)))
           (issue-creator-email . (jirassic-user-email-address
                                   (jirassic-issue-creator issue)))
           (issue-project       . (jirassic-issue-project issue))
           (issue-link          . (jirassic-issue-link issue))
           (issue-summary-slug  . (replace-regexp-in-string
                                   "[^a-zA-Z0-9_]+" "_"
                                   (downcase (jirassic-issue-summary issue)))))))
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

;;;###autoload
(aio-defun jirassic-org-capture (issue-key &optional goto keys)
  "Capture a Jira issue using Org capture templates."
  (interactive "sIssue key: ")
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
     (signal (car err) (cdr err)))))

(add-hook 'jirassic-org-after-insert-hook #'jirassic--maybe-download-org-attachments)
(add-hook 'org-capture-after-finalize-hook #'jirassic--org-capture-finalize)

(provide 'jirassic-org)
;;; jirassic-org.el ends here
