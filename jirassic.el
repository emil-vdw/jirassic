;;; jirassic.el --- A Jira client for Emacs -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created: 19 April 2025
;; Version: 0.1
;; Package-Requires: ((emacs "29.3") (dash "2.0.0") (s "1.12.0") (org "9.5"))
;; Homepage: https://github.com/emil-vdw/jirassic
;; Keywords: tools, convenience, jira

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a Jira client for Emacs. It allows you to interact with
;; Jira's REST API to create, update, and delete issues, as well as
;; search for issues and retrieve issue details.

;;; Code:

(require 'jirassic-serializer)


(defun jirassic--org-insert-issue-at (buf pos issue &optional level)
  "Insert a Jira issue into the current buffer."
  (unless (buffer-live-p buf)
    (error "Buffer no longer live"))
  (save-excursion
    (save-restriction
      (with-current-buffer buf
        (widen)
        (goto-char pos)
        (jirassic--serialize-issue issue level)))))


;;;###autoload
(defun jirassic-org-insert-issue (key &optional level)
  "Insert a Jira issue into the current buffer."
  (interactive
   (list (read-string "Enter issue key: ")
         (org-current-level)))
  (let (
        ;; Store the current point and buffer so we can return to it
        ;; later when inserting the issue.
        (pos (point))
        (buf (current-buffer)))
    (jirassic-get-issue key
                        :then
                        (lambda (data)
                          (jirassic--org-insert-issue-at
                           buf pos
                           (jirassic--parse-issue data)
                           level)))))


(provide 'jirassic)
;;; jirassic.el ends here
