;;; jirassic-org-serializer.el --- Serialize ADF objects into Org strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'cl-lib)

(require 'jirassic-adf)

(defvar level-indent 2
  "Number of whitespace characters of indentation per level.")

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
  ;; TODO: indentation
  (declare (pure t) (side-effect-free t))
  (mapconcat (lambda (list-item-text) (format "- %s" list-item-text))
             ;; Serialize the content of each `adf-list-item' into an org string
             (mapcar (lambda (list-item)
                       (jirassic--serialize-to-org (adf-list-item-content list-item)))
                     (adf-bullet-list-content obj))
             "\n"))

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
     (mapconcat #'jirassic--serialize-to-org content)
     "\n#+END_SRC\n")))

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

(provide 'jirassic-org-serializer)
;;; jirassic-org-serializer.el ends here
