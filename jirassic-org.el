;;; jirassic-org.el --- Jira Org Mode integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides Org Mode integration for the Jirassic package.

;;; Code:

(require 'jirassic-serializer)


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
         (org-current-level)))

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
                          (jirassic--org-insert-issue-at
                           buf pos
                           (jirassic--parse-issue data)
                           level)))))

(provide 'jirassic-org)
;;; jirassic-org.el ends here
