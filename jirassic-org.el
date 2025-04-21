;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides Org Mode integration for the Jirassic package.

;;; Code:

(require 'org)
(require 'ediff)

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

(defvar jirassic-issue-last-inserted nil
  "Most recently inserted Jira issue for use in hooks.")

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
     (jirassic-issue-attachments jirassic-issue-last-inserted))))

(defun jirassic--org-insert-issue-at (buf pos issue &optional level)
  "Insert a Jira issue into the current buffer."
  (unless (buffer-live-p buf)
    (error "Buffer no longer live"))
  (save-excursion
    (with-current-buffer buf
      (goto-char pos)
      (jirassic--serialize-issue issue level))))

(defun jirassic--org-capture-finalize ()
  "Perform finalization after org capture of jira issues."
  (with-current-buffer (marker-buffer org-capture-last-stored-marker)
    (goto-char (marker-position org-capture-last-stored-marker))
    (when (jirassic--is-jira-issue)
      (jirassic--maybe-download-org-attachments))))

(defun jirassic--ediff-cleanup ()
  "Cleanup buffers created by jirassic ediff."
  (mapc #'kill-buffer jirassic--ediff-buffers-to-cleanup)
  (setq jirassic--ediff-buffers-to-cleanup nil)
  (when jirassic-restore-windows-after-diff
    (unless jirassic--initial-window-configuration
      (error "Initial window configuration not saved"))
    (set-window-configuration jirassic--initial-window-configuration))
  (with-current-buffer (marker-buffer jirassic--issue-location-for-diff)
    (if jirassic--diff-issue-original-narrowing
        (narrow-to-region
         (car jirassic--diff-issue-original-narrowing)
         (cadr jirassic--diff-issue-original-narrowing))
      (widen)))
  (setq jirassic--issue-location-for-diff nil))

;;;###autoload
(defun jirassic-org-insert-issue (key &optional level)
  "Insert a Jira issue into the current buffer."
  (interactive
   (list (read-string "Enter issue key: ")
         (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (org-current-level))))

  (unless (derived-mode-p 'org-mode)
    (error "Cannot insert issue in non-org buffer"))
  (let (
        ;; Store the current point and buffer so we can return to it
        ;; later when inserting the issue.
        (where-to-insert (point-marker)))
    (message "Fetching issue %s..." key)
    (jirassic-get-issue key
                        :then
                        (lambda (data)
                          (let ((issue (jirassic--parse-issue data)))
                            (setq jirassic-issue-last-inserted issue)
                            (jirassic--org-insert-issue-at
                             (marker-buffer where-to-insert)
                             (marker-position where-to-insert)
                             issue
                             level)
                            ;; We need to run the hooks after inserting the issue,
                            ;; making sure to go to the start of the entry before
                            ;; running the hooks.
                            (save-excursion
                              (with-current-buffer (marker-buffer jirassic--serialized-entry-start)
                                (goto-char (marker-position jirassic--serialized-entry-start))
                                (run-hooks 'jirassic-org-after-insert-hook))))))))


;;;###autoload
(defun jirassic-org-view-issue-changes ()
  "Ediff the current Org-mode Jira entry against the latest version.

1. Grab `issue-id` and `issue-key` from the current Org entry properties.
2. Copy the current subtree into buffer `*jira-<ID>-current*`.
3. Fetch the issue from Jira, parse and serialize it into `*jira-<ID>-latest*`.
4. Call `ediff-buffers` on the two temp buffers."
  ;; XXX: What if we fail to fetch the issue? Clean up the buffer.
  (interactive)
  (when jirassic--issue-location-for-diff
    (error "Already requesting issue changes"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org-mode buffer"))
  (setq jirassic--initial-window-configuration (current-window-configuration))
  ;; Narrow to the current entry
  (save-excursion
    (org-back-to-heading t)
    (let* ((issue-level (org-current-level))
           (id   (org-entry-get nil "issue-id"))
           (key  (org-entry-get nil "issue-key"))
           (buf-latest
            (get-buffer-create (format "*jira-%s-latest*" id))))
      (setq jirassic--issue-location-for-diff
            (point-marker))
      ;; Fetch and serialize latest issue
      (message "Fetching latest data for issue %s..." key)
      (jirassic-get-issue
       key
       :then
       (lambda (data)
         (let ((issue (jirassic--parse-issue data)))
           (with-current-buffer buf-latest
             (let ((inhibit-read-only t))
               (erase-buffer)
               (org-mode)
               ;; Make sure that the issue is serialized at the same level as
               ;; the current issue.
               (jirassic--serialize-issue issue issue-level)))
           ;; Launch ediff
           (push buf-latest jirassic--ediff-buffers-to-cleanup)
           (switch-to-buffer (marker-buffer jirassic--issue-location-for-diff))
           ;; Store the current narrowing so we can restore it for the
           ;; user after the ediff session concludes.
           (setq jirassic--diff-issue-original-narrowing
                 (when (buffer-narrowed-p)
                   (list (point-min) (point-max))))
           (when (buffer-narrowed-p)
             (widen))

           (goto-char (marker-position jirassic--issue-location-for-diff))
           (org-narrow-to-subtree)
           (ediff-buffers (current-buffer) buf-latest)))
       :else
       ;; Something went wrong when fetching the issue. Still clean up.
       (lambda (&rest args)
         (jirassic--ediff-cleanup))))))

(add-hook 'ediff-quit-hook #'jirassic--ediff-cleanup)
(add-hook 'jirassic-org-after-insert-hook #'jirassic--maybe-download-org-attachments)
(add-hook 'org-capture-after-finalize-hook #'jirassic--org-capture-finalize)

(provide 'jirassic-org)
;;; jirassic-org.el ends here
