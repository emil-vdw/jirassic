;;; jirassic-serializer.el --- Serializers for Jira objects -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Parses jira structs to org format.

;;; Code:

(defun jirassic--serialize-status (jira-status)
  "Serialize a JIRA status to an org string."
  ;; XXX: This is a placeholder. The actual implementation should
  ;; convert the JIRA status to an appropriate org string.
  "TODO")

(defun jirassic--serialize-doc (doc)
  "Serialize ADF objects to org strings.")

(defun jirassic--serialize-properties (issue)
  "Set org entry properties for the given ISSUE."
  (mapc (lambda (property)
         (org-entry-put nil (car property) (cadr property)))
        `(("issue-link" ,(jirassic-issue-link issue))
          ("issue-id" ,(jirassic-issue-id issue))
          ("issue-key" ,(jirassic-issue-key issue))
          ("issue-type" ,(jirassic-issue-type issue))
          ("issue-creator" ,(jirassic-user-display-name
                             (jirassic-issue-creator issue)))
          ("issue-project" ,(jirassic-issue-project issue)))))

(defun jirassic--serialize-issue (issue &optional level)
  "Serialize a JIRA issue to an org entry."
  (let ((level (or level 1)))
    (org-insert-heading nil nil level)
    (insert (jirassic--serialize-status (jirassic-issue-status issue))
            " "
            (jirassic-issue-summary issue))
    (newline)
    (jirassic--serialize-properties issue)
    (newline)
    ;; (jirassic--serialize-doc (jirassic-issue-description issue))
    ))


(provide 'jirassic-serializer)
;;; jirassic-serializer.el ends here
