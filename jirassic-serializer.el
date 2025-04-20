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
(require 'jirassic-core)


(defcustom jirassic-list-item-bullet "+"
  "The character to use for list items."
  :type 'string)

(defcustom jirassic-org-todo-state-alist
  '(("To Do" . "TODO")
    ("In Progress" . "IN PROGRESS")
    ("Done" . "DONE"))
  "Map Jira status to org todo states.")

(defcustom jirassic-normalize-heading-levels t
  "If non-nil, normalize heading levels from Jira.

In Jira issue descriptions, one often uses heading 3 and greater instead
of using 1 and 2 for visual reasons. This does not look great when
translated to org format.

This reduces all heading levels by the
difference between the largest heading level and 1.")

(defvar jirassic--list-depth -1
  "The current depth of the list. Used to indent list items.")
(defvar jirassic--entry-level 0
  "The current level of the entry. Used to set the heading level.")
(defvar jirassic--normalized-heading-offset 0
  "The offset to use for normalizing heading levels.

See `jirassic--determine-normalized-heading-offset' for more
information.")

(defun jirassic--serialize-status (jira-status)
  "Serialize a JIRA status to an org string."
  (alist-get "To Do" jirassic-org-todo-state-alist
             nil nil #'string-equal))

(defun jirassic--serialize-text (data)
  "Serialize ADF text objects to org strings."
  (let* ((text
          ;; There is an edge case here where the text is a checkbox.
          ;; Jira uses a lowercase x for filled checkboxes, but we need to
          ;; convert it to an uppercase X for org mode.
          (s-replace-regexp "^\\[x\\]"
                            "[X]"
                            (alist-get 'text data)))
         (marks (alist-get 'marks data)))
    (insert
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
                       (format "[[%s][%s]]" (alist-get 'href attrs) text)))))

                 marks text))))

(defun jirassic--serialize-heading (data)
  (let* ((attrs (alist-get 'attrs data))
         (level (- (+ jirassic--entry-level
                      (alist-get 'level attrs))
                   jirassic--normalized-heading-offset))
         (content (seq-into (alist-get 'content data) 'list)))
    (org-insert-heading nil nil level)
    (mapc #'jirassic--serialize-doc-node content)
    (newline)))

(defun jirassic--serialize-rule (data)
  (insert "-----")
  (newline))

(defun jirassic--serialize-emoji (data)
  (insert (alist-get 'text (alist-get 'attrs data))))

(defun jirassic--serialize-mention (data)
  (insert (format "=%s=" (alist-get 'text (alist-get 'attrs data)))))

(defun jirassic--serialize-inline-card (data)
  (insert (format "[[%s]]" (alist-get 'url (alist-get 'attrs data)))))

(defun jirassic--serialize-expand (data)
  (let ((title (alist-get 'title (alist-get 'attrs data))))
    (unless (or (null title)
                (string-empty-p title))
      (insert (format "*%s*\n" title)))
    (insert ":EXPAND:\n")
    (jirassic--serialize-doc-node-content data)
    ;; Include an extra newline after the content
    ;; to improve readability.
    (insert ":END:\n\n")))

(defun jirassic--serialize-blockquote (data)
  (insert "#+BEGIN_QUOTE")
  (newline)
  (jirassic--serialize-doc-node-content data)
  (newline)
  (insert "#+END_QUOTE")
  (newline))

(defun jirassic--serialize-codeblock (data)
  (let* ((attrs (alist-get 'attrs data))
         (language (alist-get 'language attrs))
         (content (alist-get 'content data)))
    (insert "#+BEGIN_SRC"
            (when language
              (concat " " language)))
    (newline)
    (jirassic--serialize-doc-node-content data)
    (newline)
    (insert "#+END_SRC")
    (newline)))

(defun jirassic--serialize-bullet-list (data)
  (setq jirassic--list-depth (1+ jirassic--list-depth))
  (when (> jirassic--list-depth 1)
    ;; Insert a newline before serializing nested lists
    (newline))
  (let ((list-items (alist-get 'content data)))
    (seq-map-indexed (lambda (list-item index)
                       (insert
                        (s-repeat (* (1- jirassic--list-depth)
                                     (+ 2 org-list-indent-offset))
                                  " ")
                        (format "%s " jirassic-list-item-bullet))
                       (jirassic--serialize-doc-node list-item)
                       (when (< index (1- (length list-items)))
                         (newline)))
                     list-items))
  (setq jirassic--list-depth (1- jirassic--list-depth))
  (when (= jirassic--list-depth 0)
    (newline)))

(defun jirassic--serialize-ordered-list (data)
  "Serialize jira ADF ordered lists to org ordered lists.
"
  (setq jirassic--list-depth (1+ jirassic--list-depth))
  (when (> jirassic--list-depth 1)
    ;; Insert a newline before serializing nested lists
    (newline))
  (let ((list-items (alist-get 'content data)))

    (seq-map-indexed (lambda (list-item index)
                    (insert
                     (s-repeat (* (1- jirassic--list-depth)
                                  (+ 2 org-list-indent-offset))
                               " ")
                     (format "%s. " (1+ index)))
                    (jirassic--serialize-doc-node list-item)
                    (when (< index (1- (length list-items)))
                      ;; Insert a newline after each list item except
                      ;; the last
                      (newline)))
                  list-items))

  (setq jirassic--list-depth (1- jirassic--list-depth))
  (when (= jirassic--list-depth 0)
    (newline)))

(defun jirassic--serialize-doc-node-content (data)
  (let ((content (alist-get 'content data)))
    (seq-map-indexed (lambda (node index)
                       (jirassic--serialize-doc-node node))
                     content)))

(defun jirassic--serialize-paragraph (data)
  (jirassic--serialize-doc-node-content data)
  (when (= jirassic--list-depth 0)
    (newline)))

(defun jirassic--serialize-list-item (data)
  (jirassic--serialize-doc-node-content data))

(defun jirassic--serialize-date (data)
  (let ((timestamp
         (string-to-number
          (alist-get 'timestamp (alist-get 'attrs data)))))
    (insert (format-time-string "<%Y-%m-%d %a>"
                                ;; Convert milliseconds to seconds
                                (seconds-to-time (/ timestamp 1000))))))

(defun jirassic--determine-normalized-heading-offset (data)
  "Find the normalized heading offset from the headings in DATA.

Basically we need to determine how much to demote the headings that will
be created from DATA so that the heading levels under the main heading
start at the level one below the main heading.

Keep in mind that the headings, when serialized, will be demoted by the
entry's starting level, so the offset we are calculating will be relative
to the entry's level.

For example, if your DATA contains headings at levels 4 and 6, then the
minimum is 4 and we want new headings to start at level 2, so we demote
everything by 2. Now if we're inserting at a starting level of 3, then
normalized heading offset of a heading with level 4 in data will be:
'4 + 3 - 2 = 5' where 2 is the offset we are calculating.
"
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

    (max (- min-level 1)
         0)))

(defun jirassic--serialize-doc (doc &optional level)
  (setq jirassic--list-depth 0)
  (setq jirassic--entry-level (or level 0))
  ;; Normalize heading levels if needed, see
  ;; `jirassic-normalize-heading-levels' for more info.
  (setq jirassic--normalized-heading-offset
        (or (and jirassic-normalize-heading-levels
                 (jirassic--determine-normalized-heading-offset doc))
            0))
  (jirassic--serialize-doc-node doc))

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
     ((string= type "orderedList")
      (jirassic--serialize-ordered-list node))
     ((string= type "listItem")
      (jirassic--serialize-list-item node))
     ((string= type "hardBreak")
      (newline))
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
     (t
      (message "Unknown type: %s" type)))))

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
    (if (and (<= (count-lines (point-min) (point-max)) 1)
             (save-excursion
               (goto-char (line-beginning-position))
               (looking-at-p "^\\s-*$")))
        (progn
          (goto-char (line-beginning-position))
          (delete-line)
          (insert
           (s-repeat level "*") " "))
      (org-insert-heading nil nil level))

    (insert (jirassic--serialize-status (jirassic-issue-status issue))
            " "
            (jirassic-issue-summary issue))
    (newline)
    (jirassic--serialize-properties issue)
    (newline)
    (jirassic--serialize-doc (jirassic-issue-description issue) level)))


(provide 'jirassic-serializer)
;;; jirassic-serializer.el ends here
