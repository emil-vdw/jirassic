;;; jirassic-org-serializer.el --- Serialize ADF objects into Org strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'org)

(require 'jirassic-jira)

(defvar jirassic-level-indent 2
  "Number of whitespace characters of indentation per level.")

(defcustom jirassic-org-bullet-char "-"
  "Character used by default for org bullet lists.

Value may be one of the supported org bullet characters or a function
that takes the indentation level as an argument and returns one of those characters."
  :type '(choice (const :tag "-" "-")
                 (const :tag "+" "+")
                 (const :tag "*" "*")
                 (function :tag "Function (level -> char)"))
  :group 'jirassic)

(defcustom jirassic-blank-line-between-headings
  (not (null (alist-get 'heading
                        org-blank-before-new-entry)))
  "Whether sibling headings are serialized with a blank line in between them.

Defaults to the value of heading in `org-blank-before-new-entry', t if
set to auto or t.")

(defvar jirassic-serializer--supported-marks
  '(code em link strike strong subsup underline)
  "The type of text marks supported by `jirassic--serialize-to-org'.")

(cl-defgeneric jirassic--serialize-to-org (node &optional level)
  "Serialize NODE to an `org-mode' string at heading LEVEL.")

(cl-defmethod jirassic--serialize-to-org :around (node &optional level)
  "Default level to 0."
  (cl-call-next-method node (or level 0)))

(defun jirassic--string-repeat (num s)
  "Repeat string S NUM times."
  (declare (pure t) (side-effect-free t))
  (apply #'concat (make-list num s)))

;;; Default serializer when there isn't one specific to the node type.
(cl-defmethod jirassic--serialize-to-org ((node t) &optional _level)
  "Warn the user and return a placeholder of unsupported NODE."
  ;; This is a fallback serializer that is only meant to be dispatched
  ;; when no specific serializer is defined for the given node type.
  (declare (pure t) (side-effect-free t))
  (let ((node-type (cl-type-of node)))
    (warn "Jirassic serializer doesn't support serializing %s" node-type)
    (format "###unsupported ADF node: %s###" node-type)))

;;; `jira-issue'
(cl-defmethod jirassic--serialize-to-org ((issue jira-issue) &optional _level)
  "Serialise Jira ISSUE to an org mode task."
  (declare (pure t) (side-effect-free t))
  (format "* %s\n%s"
          (jira-issue-summary issue)
          (jirassic--serialize-to-org
           (jirassic-adjust-heading-level
            ;; Promote all headings in the description by 1 before
            ;; serializing because we want to have the issue summary
            ;; as the level 1 heading and all headings in the
            ;; description as children.
            (jira-issue-description issue) 1))))

;;; `adf-doc'
(cl-defmethod jirassic--serialize-to-org ((doc adf-doc) &optional _level)
  "Serialize DOC."
  (declare (pure t) (side-effect-free t))
  (jirassic-serializer--serialize-content-list (adf-doc-content doc)))

;;; `adf-heading'
(cl-defmethod jirassic--serialize-to-org ((heading adf-heading) &optional level)
  "Convert HEADING to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (let ((stars (adf-heading-level heading)))
    (format "%s %s"
            (jirassic--string-repeat stars "*")
            (mapconcat (lambda (heading-part)
                         (jirassic--serialize-to-org heading-part level))
                       (adf-heading-content heading)))))

;;; `adf-paragraph'
(cl-defmethod jirassic--serialize-to-org ((paragraph adf-paragraph) &optional _level)
  "Serialize PARAGRAPH."
  (declare (pure t) (side-effect-free t))
  (jirassic-serializer--serialize-content-list (adf-paragraph-content paragraph)))

;;; `adf-text'
(cl-defmethod jirassic--serialize-to-org ((text adf-text) &optional _level)
  "Return TEXT serialized to org, LEVEL is ignored.

LEVEL is ignored because `adf-text' is an inline node, so only its
parent container will consider indentation.

Only applies the first supported mark because of org syntax limitations."
  (declare (pure t))
  (let (;; Get the first mark that is supported by the serializer (if any).
        (mark (car (seq-filter
                    (lambda (mark) (member (adf-mark-type mark)
                                           jirassic-serializer--supported-marks))
                    (adf-text-marks text))))
        (full-text (adf-text-text text)))
    (if mark
        ;; The text we need to format w.r.t. the given mark may start or end
        ;; with some whitespace, e.g. " string ". Since org-mode syntax for these
        ;; emphasis markers, we take out only the center text, apply the formatting
        ;; and then put the leading and trailing whitespace back.
        (cl-destructuring-bind (leading-space center-text trailing-space)
            (jirassic-serializer--split-whitespace full-text)
          (concat
           ;; Put the leading spaces back.
           leading-space
           ;; Format the center text.
           (pcase (adf-mark-type mark)
             ('code      (format "~%s~" center-text))
             ('em        (format "/%s/" center-text))
             ('strike    (format "+%s+" center-text))
             ('strong    (format "*%s*" center-text))
             ('underline (format "_%s_" center-text))
             ('subsup    (format
                          (if (string= (alist-get 'attrs (adf-mark-attrs mark)) "sub")
                              "_{%s}"   ; subscript
                            "^{%s}")    ;superscript
                          center-text))
             ('link      (format "[[%s][%s]]" (alist-get 'href (adf-mark-attrs mark)) center-text))
             (type (warn "Unsupported ADF text mark %s" type)))
           ;; Put the trailing spaces back.
           trailing-space))
      full-text)))

;;; `adf-rule'
(cl-defmethod jirassic--serialize-to-org ((_rule adf-rule) &optional _level)
  "Serialize a rule to an org horizontal rule string, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  "-----")

;;; `adf-emoji'
(cl-defmethod jirassic--serialize-to-org ((emoji adf-emoji) &optional _level)
  "Serialize EMOJI to an org mode string, LEVEL is ignored.

LEVEL is ignored because `adf-emoji' is an inline node, so only its
parent container will consider indentation."
  (declare (pure t) (side-effect-free t))
  (adf-emoji-text emoji))

;;; `adf-inline-card'
(cl-defmethod jirassic--serialize-to-org ((card adf-inline-card) &optional _level)
  "Serialize CARD to an org link, LEVEL is ignored.

LEVEL is ignored because `adf-inline-card' is an inline node, so only
its parent container will consider indentation."
  ;; The card will only ever contain a URL or DATA but never both.
  (declare (pure t) (side-effect-free t))
  (if-let ((url (adf-inline-card-url card)))
      ;; Simple link if we only have the URL
      (format "[[%s]]" url)
    ;; Otherwise, format to a named link using the JSONLD data.
    (let* ((data (adf-inline-card-data card))
           (link (or (alist-get 'url data) (alist-get '@id data)))
           (name (alist-get 'name data)))
      (if name
          (format "[[%s][%s]]" link name)
        (format "[[%s]]" link)))))

;;; `adf-bullet-list'
(cl-defmethod jirassic--serialize-to-org ((bullet-list adf-bullet-list) &optional level)
  "Convert BULLET-LIST to an org mode string at LEVEL."
  ;; We serialize `adf-list-item' children directly in this function
  ;; instead of creating a dedicated serializer function because then
  ;; we don't have to worry about how to know what type of list the
  ;; parent is, which we need to know to determine the type of marker
  ;; (e.g. "1." vs "-").
  (declare (pure t) (side-effect-free t))
  (let ((bullet-char (if (functionp jirassic-org-bullet-char)
                         (funcall jirassic-org-bullet-char level)
                       jirassic-org-bullet-char)))
    (mapconcat (lambda (list-item-text)
                 (format "%s%s %s"
                         ;; Indent to LEVEL
                         (jirassic--string-repeat (jirassic-serializer--level-spaces level)
                                                  " ")
                         bullet-char
                         list-item-text))
               ;; Serialize the content of each `adf-list-item' into an org string
               (mapcar (lambda (list-item)
                         (jirassic-serializer--serialize-content-list
                          (adf-list-item-content list-item)
                          ;; Since this is inside a list item, we
                          ;; need to increment the indentation level.
                          (1+ level)))
                       (adf-bullet-list-content bullet-list))
               ;; Join all serialized bullets with newline characters.
               "\n")))

;;; `adf-ordered-list'
(cl-defmethod jirassic--serialize-to-org ((ordered-list adf-ordered-list) &optional level)
  "Convert ORDERED-LIST to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (jirassic--s-join
   (seq-map-indexed
    (lambda (list-item index)
      (format "%s%d. %s"
              ;; Indent at LEVEL.
              (jirassic--string-repeat (jirassic-serializer--level-spaces level) " ")
              ;; List item number
              (1+ index)
              (jirassic-serializer--serialize-content-list
               (adf-list-item-content list-item) (1+ level))))
    (adf-ordered-list-content ordered-list))
   ;; Join all list items with newline characters
   "\n"))

;;; `adf-date'
(cl-defmethod jirassic--serialize-to-org ((date adf-date) &optional _level)
  "Serialize DATE to an org timestamp, LEVEL is ignored.

LEVEL is ignored because `adf-date' is an inline node, so only its
parent container will consider indentation.

Formats to a date without time components."
  (declare (pure t) (side-effect-free t))
  (format-time-string "<%Y-%m-%d %a>"
                      (seconds-to-time (string-to-number (adf-date-timestamp date)))))

;;; `adf-hard-break'
(cl-defmethod jirassic--serialize-to-org ((_hard-break adf-hard-break) &optional _level)
  "Serialize a hard-break as a newline character, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  "\n")

;;; `adf-code-block'
(cl-defmethod jirassic--serialize-to-org ((code-block adf-code-block) &optional _level)
  "Serialize CODE-BLOCK as an org source block, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  (let ((language (adf-code-block-language code-block))
        (content (adf-code-block-content code-block)))
    (concat
     "#+BEGIN_SRC"
     (when (and language
                (not (string-empty-p language))
                ;; "none" is also an option in Jira and we want to ignore it.
                (not (string= language "none")))
       (format " %s" language))
     "\n"
     (jirassic-serializer--serialize-content-list content)
     "\n#+END_SRC")))

;;; `adf-blockquote'
(cl-defmethod jirassic--serialize-to-org ((blockquote adf-blockquote) &optional _level)
  "Serialise BLOCKQUOTE to an `org-mode' quote block."
  (declare (pure t) (side-effect-free t))
  (format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
          (jirassic-serializer--serialize-content-list (adf-blockquote-content blockquote))))

;;; `adf-table'
(cl-defmethod jirassic--serialize-to-org ((table adf-table) &optional _level)
  "Serialize TABLE into an `org-mode' table.

Because of the limitations of `org-mode' tables compared to Atlassian
tables, like cells that span multiple columns or rows, or cells that
contain nested expands. If the table is an unsupported configuration,
return a placeholder."
  (declare (pure t))
  (if-let ((reason-unsupported (jirassic-serializer--table-unsupported table)))
      (progn (warn "Cannot serialize table, it has a cell that %s" reason-unsupported)
             ;; Fall back to the default handler that will return a placeholder.
             (cl-call-next-method))
    (let* (;; We get a two dimensional list of serialized cell
           ;; contents first so we can determine the column widths.
           (serialized-rows (jirassic--table-serialize-row-contents (adf-table-content table)))
           (serialized-columns (apply #'cl-mapcar #'list serialized-rows))
           (column-widths
            (mapcar #'jirassic--column-min-width serialized-columns)))
      (concat
       ;; Header content
       (jirassic--format-table-row (nth 0 serialized-rows) column-widths)
       ;; Header seperator
       "\n" (jirassic--table-header-seperator column-widths) "\n"
       ;; Table content
       (mapconcat (lambda (row) (jirassic--format-table-row row column-widths))
                  (cdr serialized-rows) "\n")))))

(defun jirassic--table-serialize-row-contents (rows)
  "Serialize each cell of each row in ROWS to an org string."
  (mapcar
   (lambda (row)
     (mapcar
      (lambda (cell)
        (mapconcat #'jirassic--serialize-to-org
                   (jirassic--table-cell-content cell)))
      (adf-table-row-content row)))
   rows))

(defun jirassic--table-header-seperator (column-widths)
  "Create a table header separator for header columns with COLUMN-WIDTHS."
  (concat
   "|"
   (mapconcat (lambda (width)
                (jirassic--string-repeat
                 ;; The width of the cell does not include the space
                 ;; before and after the cell contents that separate
                 ;; the cell from the column divider.
                 (+ width 2)
                 "-"))
              column-widths
              "+")
   "|"))

(defun jirassic--format-table-row (row-content column-widths)
  "Format serialized ROW-CONTENT int a row with COLUMN-WIDTHS."
  (let ((padded-row-content
         (cl-mapcar (lambda (serialized-cell width)
                      (jirassic--pad-table-cell serialized-cell width))
                    row-content column-widths)))
   (concat "| "
           (jirassic--s-join padded-row-content " | ")
           " |")))

(defun jirassic--pad-table-cell (serialized-content width)
  "Pad SERIALIZED-CONTENT with whitespace so it is WIDTH chars wide.

Only supports left aligned cells (`org-mode' limitation)."
  (declare (pure t) (side-effect-free t))
  (let ((padding (- width (length serialized-content))))
    (concat serialized-content
            (jirassic--string-repeat padding " "))))

(defun jirassic--column-min-width (column)
  "Determine the min width needed for all values in COLUMN."
  ;; TODO: take into account `org-hide-emphasis-markers'. This changes
  ;; the width of the column when there are hidden characters.
  (declare (pure t) (side-effect-free t))
  (+ 2 (apply #'max (mapcar #'length column))))

(defun jirassic--s-join (strings separator)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (declare (pure t) (side-effect-free t))
  (mapconcat 'identity strings separator))

(defun jirassic-serializer--table-unsupported (table)
  "Return reason TABLE is unsupported or nil if supported.

Some nodes are not supported inside (org) tables that work just fine in
ADF tables because of org table limitations.

Tables with celss that span multiple rows or columns are not supported."
  (let* ((unsupported-nodes '(adf-blockquote
                              adf-bullet-list
                              adf-code-block
                              adf-heading
                              adf-media-group
                              adf-nested-expand
                              adf-ordered-list
                              adf-panel
                              adf-rule))
         (cells (apply #'append (mapcar #'adf-table-row-content (adf-table-content table)))))
    (seq-some
     (lambda (cell)
       (cond
        ((cl-typecase cell
           (adf-table-header   (or (adf-table-header-row-span cell)
                                   (adf-table-header-col-span cell)))
           (adf-table-cell (or (adf-table-cell-row-span cell)
                               (adf-table-cell-col-span cell))))
         "spans multiple rows or columns")

        ((jirassic--content-contains-node
          (cl-typecase cell
            (adf-table-cell (adf-table-cell-content cell))
            (adf-table-header (adf-table-header-content cell)))
          unsupported-nodes)
         "contains an unsupported block node"))))))

(defun jirassic-serializer--table-cell-content-unsupported (cell)
  "If table CELL is unsupported, return a reason.

A Cell is unsupported if:
1. It spans multiple columns or rows.
2. It contains a block node that cannot represented in an org table."
  (let (;; Nodes that are not supported inside table cells. See
        ;; `adf-table-cell' and `adf-table-header' for more info.
        (unsupported-nodes '(adf-blockquote
                             adf-bullet-list
                             adf-code-block
                             adf-heading
                             adf-media-group
                             adf-nested-expand
                             adf-ordered-list
                             adf-panel
                             adf-rule)))
    (cond
     ((cl-typecase cell
        (adf-table-header   (or (adf-table-header-row-span cell)
                              (adf-table-header-col-span cell)))
        (adf-table-cell (or (adf-table-cell-row-span cell)
                              (adf-table-cell-col-span cell))))
      "spans multiple rows or columns")

     ((jirassic--content-contains-node
       (cl-typecase cell
         (adf-table-cell (adf-table-cell-content cell))
         (adf-table-header (adf-table-header-content cell)))
       unsupported-nodes)
      "contains an unsupported block node"))))

(defun jirassic-serializer--split-whitespace (string)
  "Split STRING in leading whitespace, center string and trailing spaces.

Returns a three element list containing the leading whitespace, center
string and trailing whitespace characters.

Example:
  (jirassic-serializer--split-whitespace \"  some string \")
  => '(\"  \" \"some string\" \" \")"
  (declare (pure t) (side-effect-free t))
  (string-match "\\`\\(\\s-*\\)\\(.*?\\)\\(\\s-*\\)\\'" string)
  (list (match-string 1 string)
        (match-string 2 string)
        (match-string 3 string)))

(defun jirassic-serializer--serialize-content-list (content &optional level)
  "Serialize a list of ADF node CONTENT at LEVEL."
  ;; Nodes in CONTENT can be block nodes (like `adf-paragraph' or
  ;; `adf-code-block'), in which case we have to put two newline
  ;; characters in between subsequent block nodes.
  (string-join
   (seq-mapn
    (lambda (cur-node next-node)
      (concat (jirassic--serialize-to-org cur-node (or level 0))
              (when (and (jirassic-jira-block-node-p cur-node)
                         (jirassic-jira-block-node-p next-node))
                (cond
                 ((and (adf-heading-p cur-node) (adf-heading-p next-node))
                  ;; We want an extra newline between sibling headings.
                  (if (and (eq (adf-heading-level cur-node) (adf-heading-level next-node))
                           jirassic-blank-line-between-headings)
                      "\n\n"
                    "\n"))

                 ;; Two newlines between a horizontal rule or a
                 ;; heading and any other block node.
                 ((or (adf-heading-p next-node)
                      (adf-rule-p cur-node)
                      (adf-rule-p next-node))
                  "\n\n")
                 (t "\n")))))
    ;; Loop over two nodes at a time, the current and next node, so we
    ;; can spot consecutive block nodes.
    content
    ;; Skip the first element and add `nil' so the list is the same
    ;; size and the last element has next node `nil'.
    (append (cdr content) '(nil)))))

(defun jirassic-serializer--level-spaces (level)
  "Return the number of whitespaces of indentation for LEVEL."
  (* jirassic-level-indent level))

(provide 'jirassic-org-serializer)
;;; jirassic-org-serializer.el ends here
