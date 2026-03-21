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
(cl-defmethod jirassic--serialize-to-org ((obj adf-text) &optional level)
  "Return the text of an ADF text OBJ at LEVEL.

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
(cl-defmethod jirassic--serialize-to-org ((obj adf-rule) &optional level)
  "Convert an ADF rule OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  "-----")

;;; `adf-emoji'
(cl-defmethod jirassic--serialize-to-org ((obj adf-emoji) &optional level)
  "Convert an ADF emoji OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (adf-emoji-text obj))

;;; `adf-bullet-list'
(cl-defmethod jirassic--serialize-to-org ((obj adf-bullet-list) &optional level)
  "Convert an ADF bullet list OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (mapconcat (lambda (list-item-text) (format "- %s" list-item-text))
             ;; Serialize the content of each `adf-list-item' into an org string
             (mapcar (lambda (list-item)
                       (jirassic--serialize-to-org (adf-list-item-content list-item)))
                     (adf-bullet-list-content obj))
             "\n"))

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
