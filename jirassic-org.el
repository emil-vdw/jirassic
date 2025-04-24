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

(defvar jirassic--diff-issue-original-narrowing nil
  "Marker for the location of the issue being diffed.")

(defun jirassic--is-jira-issue (&optional epom)
  "Check if org entry at EPOM is a jira issue.

EPOM is an element, marker, or buffer position."
  (not (null (org-entry-get epom "issue-id"))))

(defun jirassic--maybe-download-org-attachments ()
  (when (and jirassic-org-add-attachments
             (jirassic--is-jira-issue)
             buffer-file-name)
    (jirassic--serialize-attachments
     (jirassic-issue-attachments jirassic-last-inserted-issue))))

(defun jirassic--org-capture-finalize ()
  "Perform finalization after org capture of jira issues."
  (with-current-buffer (marker-buffer org-capture-last-stored-marker)
    (goto-char (marker-position org-capture-last-stored-marker))
    (when (jirassic--is-jira-issue)
      (jirassic--maybe-download-org-attachments))))

(defun jirassic--issue-ediff-cleanup ()
  "Cleanup buffers created by jirassic ediff."
  (mapc #'kill-buffer jirassic--ediff-buffers-to-cleanup)
  (setq jirassic--ediff-buffers-to-cleanup nil)
  (when ediff-control-buffer
    (kill-buffer ediff-control-buffer))
  (when ediff-buffer-C
    (kill-buffer ediff-buffer-C))
  (when jirassic-restore-windows-after-diff
    (unless jirassic--initial-window-configuration
      (error "Initial window configuration not saved"))
    (set-window-configuration jirassic--initial-window-configuration))
  (with-current-buffer (marker-buffer jirassic--issue-location-for-diff)
    (if jirassic--diff-issue-original-narrowing
        (narrow-to-region
         (car jirassic--diff-issue-original-narrowing)
         (cadr jirassic--diff-issue-original-narrowing))
      (widen))
    (read-only-mode -1))
  (setq jirassic--issue-location-for-diff nil))

(defun jirassic--after-issue-merge ()
  "After merging the issue, save the changes to the original buffer."
  (unwind-protect
      (when (y-or-n-p "Save changes to the merged issue?")
        (let ((updated-issue
               (with-current-buffer ediff-buffer-C
                 (buffer-substring-no-properties (point-min) (point-max)))))
          ;; XXX: What if the buffer has been closed?
          (with-current-buffer (marker-buffer jirassic--issue-location-for-diff)
            (save-excursion
              (org-back-to-heading t)
              (org-narrow-to-subtree)
              (let ((inhibit-read-only t))
                (delete-region (point-min) (point-max))
                (insert updated-issue))))))
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

;;;###autoload
(aio-defun jirassic-org-view-issue-changes ()
  "Ediff the current Org-mode Jira entry against the latest version.

1. Grab `issue-id` and `issue-key` from the current Org entry properties.
2. Copy the current subtree into buffer `*jira-<ID>-current*`.
3. Fetch the issue from Jira, parse and serialize it into `*jira-<ID>-latest*`.
4. Call `ediff-buffers` on the two temp buffers."
  (interactive)
  (when jirassic--issue-location-for-diff
    (error "Already requesting issue changes"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org-mode buffer"))
  (setq jirassic--initial-window-configuration (current-window-configuration))
  (condition-case err
      (jirassic-bind-restore
          ((issue-level (org-current-level))
           (id   (org-entry-get nil "issue-id"))
           (key  (org-entry-get nil "issue-key"))
           (buf-latest
            (get-buffer-create (format "*jira-%s-latest*" id)))
           (issue-latest (progn
                           (message "Fetching latest data for issue %s..." key)
                           (aio-await (jirassic-get-issue key)))))

        (setq jirassic--issue-location-for-diff (point-marker))
        (with-current-buffer buf-latest
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            ;; Make sure that the issue is serialized at the same level as
            ;; the current issue.
            (jirassic--serialize-issue-entry issue-latest issue-level)))

        (push buf-latest jirassic--ediff-buffers-to-cleanup)
        ;; Store the current narrowing so we can restore it for the
        ;; user after the ediff session concludes.
        (setq jirassic--diff-issue-original-narrowing
              (when (buffer-narrowed-p)
                (list (point-min) (point-max))))
        (when (buffer-narrowed-p)
          (widen))

        (org-narrow-to-subtree)
        (ediff-merge-buffers (current-buffer) buf-latest
                             (list
                              (lambda ()
                                (setq-local ediff-quit-merge-hook
                                            (cons jirassic--after-issue-merge ediff-quit-merge-hook))))))

    ;; Clean up even if there's an error.
    (jirassic-client-error
     (let ((client-error (cdr err)))
       (message "Error fetching issue %s: %s"
                (propertize key 'face 'bold)
                (jirassic-http-error-message client-error))
       (jirassic--issue-ediff-cleanup)))

    (error (progn
             (jirassic--issue-ediff-cleanup)
             (signal (car err) (cdr err))))))

(add-hook 'jirassic-org-after-insert-hook #'jirassic--maybe-download-org-attachments)
(add-hook 'org-capture-after-finalize-hook #'jirassic--org-capture-finalize)

(provide 'jirassic-org)
;;; jirassic-org.el ends here
