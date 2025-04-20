;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides Org Mode integration for the Jirassic package.

;;; Code:

(require 'jirassic-serializer)


(defcustom jirassic-org-add-attachments t
  "Whether to download and attach files.

If nil, the media files will be linked to the issue. If t, the media
files will be downloaded and attached to the org file via `org-attach'.")

(defvar jirassic-issue-last-inserted nil
  "Most recently inserted Jira issue for use in hooks.")

(defcustom jirassic-org-after-insert-hook nil
  "Hook run after inserting a Jira issue into an Org buffer."
  :type 'hook
  :group 'jirassic)

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

(add-hook 'jirassic-org-after-insert-hook #'jirassic--maybe-download-org-attachments)
(add-hook 'org-capture-after-finalize-hook #'jirassic--org-capture-finalize)

(provide 'jirassic-org)
;;; jirassic-org.el ends here
