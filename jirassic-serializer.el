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
(require 'org)

(require 'f)
(require 's)

(require 'jirassic-core)
(require 'jirassic-issue)


(defcustom jirassic-list-item-bullet "+"
  "The character to use for list items."
  :type 'string
  :group 'jirassic)

(defcustom jirassic-new-line-before-heading t
  "Whether to insert a newline before headings."
  :type 'boolean
  :group 'jirassic)

(defcustom jirassic-org-todo-state-alist
  '(("To Do" . "TODO")
    ("In Progress" . "IN PROGRESS")
    ("Done" . "DONE"))
  "Map Jira status to org todo states."
  :type '(alist :key-type string :value-type string)
  :group 'jirassic)

(defcustom jirassic-normalize-heading-levels t
  "If non-nil, normalize heading levels from Jira.

In Jira issue descriptions, one often uses heading 3 and greater instead
of using 1 and 2 for visual reasons. This does not look great when
translated to org format.

This reduces the level all heading levels by the amount of smallest
heading level."
  :type 'boolean
  :group 'jirassic)

(defvar jirassic--list-depth -1
  "The current depth of the list. Used to indent list items.")
(defvar jirassic--entry-level 0
  "The current level of the entry. Used to set the heading level.")
(defvar jirassic--normalized-heading-offset 0
  "The offset to use for normalizing heading levels.

See `jirassic--find-min-heading-level' for more
information.")
(defvar jirassic--inside-table nil
  "Whether we are serializing nodes inside a table.")

(defun jirassic--string-repeat (num s)
  "Repeat string S NUM times."
  (apply #'concat (make-list num s)))

(defun jirassic--serialize-org-properties (props)
  "Serialize org PROPS alist to a string."
  (concat
   ":PROPERTIES:\n"
   (mapconcat
    (lambda (prop)
      (format ":%s: %s" (car prop) (cdr prop)))
    props
    "\n")
   "\n:END:\n"))

(defun jirassic--jira-to-org-status (jira-status)
  "Serialize a JIRA-STATUS to an org string."
  (alist-get "To Do" jirassic-org-todo-state-alist
             jira-status nil #'string-equal))

(defun jirassic--serialize-table (data)
  "Serialize ADF table object DATA to org strings."
  (setq jirassic--inside-table t)
  (prog1
      (with-temp-buffer
        (org-mode)
        (save-excursion
          (insert
           (jirassic--serialize-doc-node-content data)))
        (org-table-align)
        (buffer-substring-no-properties
         (point-min)
         (point-max)))
    (setq jirassic--inside-table nil)))

(defun jirassic--serialize-table-row (data)
  "Serialize ADF table object DATA to org strings."
  (concat
   "| "
   (let ((content (alist-get 'content data)))
     (mapconcat (lambda (node)
                  (jirassic--serialize-doc-node node))
                content
                " | "))
   " |\n"))

(defun jirassic--serialize-table-header (data)
  "Serialize ADF table object DATA to org strings."
  (jirassic--serialize-doc-node-content data))

(defun jirassic--serialize-table-cell (data)
  "Serialize ADF table cell DATA to org strings."
  (jirassic--serialize-doc-node-content data))

(defun jirassic--serialize-text (data)
  "Serialize ADF text object DATA to org strings."
  (let* ((text
          ;; There is an edge case here where the text is a checkbox.
          ;; Jira uses a lowercase x for filled checkboxes, but we need to
          ;; convert it to an uppercase X for org mode.
          (save-match-data
            (replace-regexp-in-string "^\\[x\\]"
                                      "[X]"
                                      (alist-get 'text data))))
         (marks (alist-get 'marks data)))
    (seq-reduce (lambda (formatted-text mark)
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
                      (format "[[%s][%s]]" (alist-get 'href attrs) text))
                     ((and (string= type "subsup")
                           (string=
                            (alist-get 'type attrs) "sup"))
                      (format "^{%s}" formatted-text))
                     ((and (string= type "subsup")
                           (string=
                            (alist-get 'type attrs) "sub"))
                      (format "_{%s}" formatted-text)))))

                marks text)))

(defun jirassic--serialize-heading (data)
  "Serialize ADF heading objects DATA to org strings."
  (let* ((attrs (alist-get 'attrs data))
         (level (- (+ jirassic--entry-level
                      (alist-get 'level attrs))
                   jirassic--normalized-heading-offset))
         (content (alist-get 'content data)))
    (concat
     ;; Insert a newline before every heading
     (when jirassic-new-line-before-heading
       "\n")
     (jirassic--string-repeat level "*")
     " "
     (mapconcat #'jirassic--serialize-doc-node content)
     "\n")))

(defun jirassic--serialize-rule (&rest _)
  "Serialize ADF rule objects to org strings."
  "-----\n")

(defun jirassic--serialize-emoji (data)
  "Serialize ADF emoji DATA to org strings."
  (alist-get 'text (alist-get 'attrs data)))

(defun jirassic--serialize-mention (data)
  "Serialize ADF mention DATA to org strings."
  (format "=%s=" (alist-get 'text (alist-get 'attrs data))))

(defun jirassic--serialize-inline-card (data)
  "Serialize ADF inline card DATA to org strings."
  (format "[[%s]]" (alist-get 'url (alist-get 'attrs data))))

(defun jirassic--serialize-expand (data)
  "Serialize ADF expand DATA to org strings."
  (let ((title (alist-get 'title (alist-get 'attrs data))))
    (concat
     "#+begin_expand"
     (when (and title
                (not (string= title "")))
       (format " *%s*" title))
     "\n"
     (jirassic--serialize-doc-node-content data)
     "#+end_expand\n\n")))

(defun jirassic--serialize-blockquote (data)
  "Serialize ADF blockquote DATA to org strings."
  (concat
   "#+BEGIN_QUOTE\n"
   (jirassic--serialize-doc-node-content data)
   "\n"
   "#+END_QUOTE\n"))

(defun jirassic--serialize-codeblock (data)
  "Serialize ADF codeblock DATA to org strings."
  (let* ((attrs (alist-get 'attrs data))
         (language (alist-get 'language attrs)))
    (concat
     "#+BEGIN_SRC"
     (when language
       (concat " " language))
     "\n"
     (jirassic--serialize-doc-node-content data)
     "\n"
     "#+END_SRC\n")))

(defun jirassic--serialize-bullet-list (data)
  "Serialize jira ADF bullet list DATA to org bullet lists."
  (setq jirassic--list-depth (1+ jirassic--list-depth))
  (prog1
      (concat
       (when (> jirassic--list-depth 1)
         ;; Insert a newline before serializing nested lists
         "\n")
       (let ((list-items (alist-get 'content data)))
         (apply
          #'concat
          (seq-map-indexed
           (lambda (list-item index)
             (concat
              (jirassic--string-repeat (* (1- jirassic--list-depth)
                                          (+ 2 org-list-indent-offset))
                                       " ")
              (format "%s " jirassic-list-item-bullet)
              (jirassic--serialize-doc-node list-item)
              (when (< index (1- (length list-items)))
                "\n")))
           list-items)))
       (when (= jirassic--list-depth 1)
         "\n"))
    (setq jirassic--list-depth (1- jirassic--list-depth))))

(defun jirassic--serialize-ordered-list (data)
  "Serialize jira ADF ordered list DATA to org ordered lists."
  (setq jirassic--list-depth (1+ jirassic--list-depth))
  (prog1
      (concat
       (when (> jirassic--list-depth 1)
         ;; Insert a newline before serializing nested lists
         "\n")
       (let ((list-items (alist-get 'content data)))
         (seq-map-indexed
          (lambda (list-item index)
            (concat
             (jirassic--string-repeat
              (* (1- jirassic--list-depth)
                 (+ 2 org-list-indent-offset))
              " ")
             (format "%s. " (1+ index))
             (jirassic--serialize-doc-node list-item)
             (when (< index (1- (length list-items)))
               ;; Insert a newline after each list item except
               ;; the last
               "\n")))
          list-items))
       (when (= jirassic--list-depth 1)
         "\n"))
   (setq jirassic--list-depth (1- jirassic--list-depth))))

(defun jirassic--serialize-doc-node-content (data)
  "Serialize the content of a doc node DATA to org strings."
  (let ((content (alist-get 'content data)))
    (mapconcat (lambda (node)
                 (jirassic--serialize-doc-node node))
               content)))

(defun jirassic--serialize-paragraph (data)
  "Serialize ADF paragraph DATA to org strings."
  (concat
   (jirassic--serialize-doc-node-content data)
   (when (and (= 0 jirassic--list-depth)
              (not jirassic--inside-table))
     "\n")))

(defun jirassic--serialize-list-item (data)
  "Serialize ADF list item DATA to org strings."
  (jirassic--serialize-doc-node-content data))

(defun jirassic--serialize-date (data)
  "Serialize ADF date DATA to org strings."
  (let ((timestamp
         (string-to-number
          (alist-get 'timestamp (alist-get 'attrs data)))))
    (format-time-string "<%Y-%m-%d %a>"
                        ;; Convert milliseconds to seconds
                        (seconds-to-time (/ timestamp 1000)))))

(defun jirassic--find-min-heading-level (data)
  "Find the minimum heading level in the ADF DATA."
  (let ((min-level nil))
    (cl-labels
        ((walk (n)
           (cond
            ;; If this is an alist with (type . \"heading\"), extract its level:
            ((and (listp n)
                  (string-equal (assoc-default 'type n) "heading"))
             (let* ((attrs (assoc-default 'attrs n))
                    (lvl   (assoc-default 'level attrs)))
               (when (numberp lvl)
                 (setq min-level
                       (if min-level
                           (min min-level lvl)
                         lvl))))
             ;; continue into its content subtree, if any:
             (seq-map #'walk (assoc-default 'content n)))
            ;; Otherwise, if it has content:
            ((if-let ((content (assoc-default 'content n)))
                 (seq-map #'walk content))))))
      (walk data))

    (or min-level
        0)))

(defun jirassic--serialize-doc (doc &optional level)
  "Serialize ADF doc object DOC to org strings at LEVEL."
  (setq jirassic--list-depth 0)
  (setq jirassic--entry-level (or level 0))
  ;; Normalize heading levels if needed, see
  ;; `jirassic-normalize-heading-levels' for more info.
  (setq jirassic--normalized-heading-offset
        (or (and jirassic-normalize-heading-levels
                 (jirassic--find-min-heading-level doc))
            0))
  (jirassic--serialize-doc-node doc))

(defun jirassic--serialize-doc-node (node)
  "Serialize ADF NODE to org string."
  (let ((type (alist-get 'type node)))
    (cond
     ((string= type "text")
      (jirassic--serialize-text node))
     ((string= type "rule")
      (jirassic--serialize-rule node))
     ((string= type "bulletList")
      (jirassic--serialize-bullet-list node))
     ((string= type "orderedList")
      (jirassic--serialize-ordered-list node))
     ((string= type "listItem")
      (jirassic--serialize-list-item node))
     ((string= type "hardBreak")
      "\n")
     ((string= type "paragraph")
      (jirassic--serialize-paragraph node))
     ((string= type "doc")
      (jirassic--serialize-doc-node-content node))
     ((string= type "heading")
      (jirassic--serialize-heading node))
     ((string= type "blockquote")
      (jirassic--serialize-blockquote node))
     ((string= type "codeBlock")
      (jirassic--serialize-codeblock node))
     ((string= type "date")
      (jirassic--serialize-date node))
     ((string= type "expand")
      (jirassic--serialize-expand node))
     ((string= type "emoji")
      (jirassic--serialize-emoji node))
     ((string= type "mention")
      (jirassic--serialize-mention node))
     ((string= type "inlineCard")
      (jirassic--serialize-inline-card node))
     ((string= type "table")
      (jirassic--serialize-table node))
     ((string= type "tableRow")
      (jirassic--serialize-table-row node))
     ((string= type "tableCell")
      (jirassic--serialize-table-cell node))
     ((string= type "tableHeader")
      (jirassic--serialize-table-header node))
     (t
      (message "Unknown type: %s" type)))))

(defun jirassic--serialize-properties (issue &optional extra-properties)
  "Set org entry properties for the given ISSUE.

EXTRA-PROPERTIES is an alist of extra properties to add to the
properties list. This is useful for adding custom properties
that are not part of the standard Jira issue properties."
  (jirassic--serialize-org-properties
   (append extra-properties
           `(("issue-key"     . ,(jirassic-issue-key issue))
             ("issue-link"    . ,(jirassic-issue-link issue))
             ("issue-id"      . ,(jirassic-issue-id issue))
             ("issue-type"    . ,(jirassic-issue-type issue))
             ("issue-creator" . ,(jirassic-user-display-name
                                  (jirassic-issue-creator issue)))
             ("issue-project" . ,(jirassic-issue-project issue))))))

(defun jirassic--serialize-issue-entry (issue &optional level)
  "Serialize a Jira ISSUE to an org entry at LEVEL.

This function will insert the issue into the current buffer with
the same behavior as `org-insert-heading'. After inserting the
issue, it will return a marker to the start of the entry."
  (let ((level (or level 1)))
    (concat
     (jirassic--string-repeat level "*") " "
     (jirassic--jira-to-org-status (jirassic-issue-status issue)) " "
     (jirassic-issue-summary issue) "\n"
     (jirassic--serialize-properties issue)
     (jirassic--serialize-doc (jirassic-issue-description issue) (+ level 1)))))


(provide 'jirassic-serializer)
;;; jirassic-serializer.el ends here
