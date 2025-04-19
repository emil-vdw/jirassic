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

(defcustom jirassic-list-item-bullet "+"
  "The character to use for list items."
  :type 'string)

(defun jirassic--serialize-status (jira-status)
  "Serialize a JIRA status to an org string."
  ;; XXX: This is a placeholder. The actual implementation should
  ;; convert the JIRA status to an appropriate org string.
  "TODO")

(defun jirassic--serialize-text (data)
  "Serialize ADF objects to org strings."
  (let ((text (alist-get 'text data))
        (marks
         ;; Convert the vector of marks to a list
         (seq-into (alist-get 'marks data) 'list)))
    (insert
     (-reduce-from (lambda (formatted-text mark)
                     (let ((type (alist-get 'type mark))
                           (attrs (alist-get 'attrs mark)))
                       (cond
                        ((string= type "strong")
                         (format "*%s*" formatted-text))
                        ((string= type "em")
                         (format "/%s/" formatted-text))
                        ((string= type "underline")
                         (format "_%s_" formatted-text))
                        ((string= type "strike")
                         (format "+%s+" formatted-text))
                        ((string= type "code")
                         (format "~%s~" formatted-text))
                        ((string= type "link")
                         (format "[[%s][%s]]" (alist-get 'href attrs) text)))))

                   text marks))))

(defun jirassic--serialize-rule (data)
  "Serialize ADF objects to org strings."
  (newline)
  (insert "-----")
  (newline))

(defun jirassic--serialize-bullet-list (data)
  (newline)
  (setq jirassic-list-depth (+ jirassic-list-depth 1))
  (let ((list-items
         ;; Convert the vector of list items to a list
         (seq-into (alist-get 'content data) 'list)))

    (-map (lambda (list-item)
            (insert
             (s-repeat (* jirassic-list-depth 2) " ")
             (format "%s " jirassic-list-item-bullet))
            (jirassic--serialize-doc-node list-item))
          list-items))
  (setq jirassic-list-depth (- jirassic-list-depth 1)))

(defun jirassic--serialize-doc-node-content (data)
  (let ((content (seq-into (alist-get 'content data) 'list)))
   (-map (lambda (node)
           (jirassic--serialize-doc-node node))
         content)))

(defun jirassic--serialize-paragraph (data)
  (jirassic--serialize-doc-node-content data))

(defun jirassic--serialize-list-item (data)
  (jirassic--serialize-doc-node-content data)
  (newline))

(defun jirassic--serialize-doc (doc)
  (org-dlet ((jirassic-list-depth 0) ; Keep track of the current list depth
             )
    (jirassic--serialize-doc-node doc)))

(defun jirassic--serialize-doc-node (node)
  "Serialize ADF objects to org strings."
  (let ((type (alist-get 'type node)))
    (cond
     ((string= type "text")
      (jirassic--serialize-text node))
     ((string= type "rule")
      (jirassic--serialize-rule node))
     ((string= type "bulletList")
      (jirassic--serialize-bullet-list node))
     ((string= type "listItem")
      (jirassic--serialize-list-item node))
     ((string= type "hardBreak")
      (newline))
     ((string= type "paragraph")
      (jirassic--serialize-doc-node-content node))
     ((string= type "doc")
      (jirassic--serialize-doc-node-content node))
     ;; (t
     ;;  (error "Unknown type: %s" type))
     )))

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
    (jirassic--serialize-doc (jirassic-issue-description issue))
    ))


(provide 'jirassic-serializer)
;;; jirassic-serializer.el ends here
