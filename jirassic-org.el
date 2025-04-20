;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides Org Mode integration for the Jirassic package.

;;; Code:

(require 'jirassic-serializer)


(defcustom jirassic-org-add-attachments t
  "Whether to download and attach files.

If nil, the media files will be linked to the issue. If t, the media
files will be downloaded and attached to the org file via `org-attach'.")

(defvar jirassic-issue nil
  "Most recently inserted Jira issue for post-insert hooks.")

(defcustom jirassic-org-after-insert-hook nil
  "Hook run after inserting a Jira issue into an Org buffer."
  :type 'hook
  :group 'jirassic)

(defun jirassic-org--maybe-download-attachments ()
  (when (and jirassic-org-add-attachments
             buffer-file-name)
    (jirassic--serialize-attachments
     (jirassic-issue-attachments jirassic-issue))))

(defun jirassic--org-insert-issue-at (buf pos issue &optional level)
  "Insert a Jira issue into the current buffer."
  (unless (buffer-live-p buf)
    (error "Buffer no longer live"))
  (save-excursion
    (with-current-buffer buf
      (goto-char pos)
      (jirassic--serialize-issue issue level))))

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
        (pos (point))
        (buf (current-buffer)))
    (message "Fetching issue %s..." key)
    (jirassic-get-issue key
                        :then
                        (lambda (data)
                          (let ((jirassic-issue (jirassic--parse-issue data)))
                            (jirassic--org-insert-issue-at
                             buf pos
                             jirassic-issue
                             level)
                            (run-hooks 'jirassic-org-after-insert-hook))))))

(add-hook 'jirassic-org-after-insert-hook #'jirassic-org--maybe-download-attachments)


(provide 'jirassic-org)
;;; jirassic-org.el ends here
