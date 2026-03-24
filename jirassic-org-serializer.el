;;; jirassic-org-serializer.el --- Serialize ADF objects into Org strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'cl-lib)

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

(defvar jirassic-serializer--supported-marks
  '(code em link strike strong subsup underline)
  "The type of text marks supported by `jirassic--serialize-to-org'.")

(cl-defgeneric jirassic--serialize-to-org (obj &optional level)
  "Serialize OBJ to an `org-mode' string at heading LEVEL.")

(cl-defmethod jirassic--serialize-to-org :around (obj &optional level)
  "Default level to 0."
  (cl-call-next-method obj (or level 0)))

(defun jirassic--string-repeat (num s)
  "Repeat string S NUM times."
  (declare (pure t) (side-effect-free t))
  (apply #'concat (make-list num s)))

;;; Default serializer when there isn't one specific to the node type.
(cl-defmethod jirassic--serialize-to-org ((obj t) &optional _level)
  "Warn the user and return a placeholder of unsupported node OBJ."
  ;; This is a fallback serializer that is only meant to be dispatched
  ;; when no specific serializer is defined for the given node type.
  (declare (pure t) (side-effect-free t))
  (let ((node-type (cl-type-of obj)))
    (warn "Jirassic serializer doesn't support serializing %s" node-type)
    (format "###unsupported ADF node: %s###" node-type)))

;;; `jira-issue'
(cl-defmethod jirassic--serialize-to-org ((obj jira-issue) &optional _level)
  "Serialise Jira issue OBJ to an org mode task."
  (declare (pure t) (side-effect-free t))
  (format "* %s\n%s"
          (jira-issue-summary obj)
          (jirassic--serialize-to-org (jira-issue-description obj))))

;;; `adf-doc'
(cl-defmethod jirassic--serialize-to-org ((obj adf-doc) &optional _level)
  "Serialize an `adf-doc' OBJ."
  (declare (pure t) (side-effect-free t))
  (jirassic-serializer--serialize-content-list (adf-doc-content obj)))

;;; `adf-heading'
(cl-defmethod jirassic--serialize-to-org ((obj adf-heading) &optional level)
  "Convert an ADF heading OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (let ((stars (adf-heading-level obj)))
    (format "%s %s"
            (jirassic--string-repeat stars "*")
            (mapconcat (lambda (heading-part)
                         (jirassic--serialize-to-org heading-part level))
                       (adf-heading-content obj)))))

;;; `adf-paragraph'
(cl-defmethod jirassic--serialize-to-org ((obj adf-paragraph) &optional _level)
  "Serialize an `adf-paragraph' OBJ."
  (declare (pure t) (side-effect-free t))
  (jirassic-serializer--serialize-content-list (adf-paragraph-content obj)))

;;; `adf-text'
(cl-defmethod jirassic--serialize-to-org ((obj adf-text) &optional _level)
  "Return the text of an ADF text OBJ, LEVEL is ignored.

LEVEL is ignored because `adf-text' is an inline node, so only its
parent container will consider indentation.

Only applies the first supported mark because of org syntax limitations."
  (declare (pure t) (side-effect-free t))
  (let (;; Get the first mark that is supported by the serializer (if any).
        (mark (car (seq-filter
                    (lambda (mark) (member (adf-mark-type mark)
                                           jirassic-serializer--supported-marks))
                    (adf-text-marks obj))))
        (full-text (adf-text-text obj)))
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
(cl-defmethod jirassic--serialize-to-org ((obj adf-rule) &optional _level)
  "Convert an ADF rule OBJ to an org mode string, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  "-----")

;;; `adf-emoji'
(cl-defmethod jirassic--serialize-to-org ((obj adf-emoji) &optional _level)
  "Convert an `adf-emoji' OBJ to an org mode string, LEVEL is ignored.

LEVEL is ignored because `adf-emoji' is an inline node, so only its
parent container will consider indentation."
  (declare (pure t) (side-effect-free t))
  (adf-emoji-text obj))

;;; `adf-bullet-list'
(cl-defmethod jirassic--serialize-to-org ((obj adf-bullet-list) &optional level)
  "Convert an ADF bullet list OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (let ((bullet-char (if (functionp jirassic-org-bullet-char)
                         (funcall jirassic-org-bullet-char level)
                       jirassic-org-bullet-char)))
    (format
     ;; Put a leading and trailing newline before each list and pad with
     ;; LEVEL whitespaces.
     "%s"
     (mapconcat (lambda (list-item-text)
                  (format "%s%s %s"
                          ;; Pad with whitespace for the indentation level
                          (jirassic--string-repeat (jirassic-serializer--level-spaces level)
                                                   " ")
                          ;; insert the bullet character
                          bullet-char
                          ;; And the content of the bullet
                          list-item-text))
                ;; Serialize the content of each `adf-list-item' into an org string
                (mapcar (lambda (list-item)
                          (jirassic-serializer--serialize-content-list
                           (adf-list-item-content list-item)
                           ;; Since this is inside a list item, we
                           ;; need to increment the indentation level.
                           (1+ level)))
                        (adf-bullet-list-content obj))
                ;; Join all serialized bullets with newline characters.
                ;; "\n"
                ))))

;;; `adf-date'
(cl-defmethod jirassic--serialize-to-org ((obj adf-date) &optional _level)
  "Serialize the `adf-date-timestamp' of OBJ to an org timestamp, LEVEL is ignored.

LEVEL is ignored because `adf-date' is an inline node, so only its
parent container will consider indentation.

Formats to a date without time components."
  (declare (pure t) (side-effect-free t))
  (format-time-string "<%Y-%m-%d %a>"
                      (seconds-to-time (string-to-number (adf-date-timestamp obj)))))

;;; `adf-hard-break'
(cl-defmethod jirassic--serialize-to-org ((obj adf-hard-break) &optional _level)
  "Serialize an `adf-hard-break' OBJ as a newline character, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  "\n")

;;; `adf-code-block'
(cl-defmethod jirassic--serialize-to-org ((obj adf-code-block) &optional _level)
  "Serialize an `adf-code-block' OBJ as an org source block, LEVEL is ignored."
  (declare (pure t) (side-effect-free t))
  (let ((language (adf-code-block-language obj))
        (content (adf-code-block-content obj)))
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
(cl-defmethod jirassic--serialize-to-org ((obj adf-blockquote) &optional _level)
  "Serialise an `adf-blockquote' OBJ to an org mode ."
  (declare (pure t) (side-effect-free t))
  (format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
          (jirassic-serializer--serialize-content-list (adf-blockquote-content obj))))

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
                  (if (eq (adf-heading-level cur-node) (adf-heading-level next-node))
                      "\n\n"
                    "\n"))
                 ((and (adf-heading-p cur-node) (adf-heading-p next-node)) "\n\n")
                 ((adf-heading-p next-node) "\n\n")
                 ;; Between a horizontal rule and any other block
                 ;; node, insert two newlines.
                 ((or (and (adf-rule-p cur-node) (not (null next-node)))
                      (and (adf-rule-p next-node) (not (null cur-node))))
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
