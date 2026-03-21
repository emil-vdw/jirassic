;;; jirassic-org-serializer.el --- Serialize Jira objects into Org strings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;;; Code:
(require 'cl-lib)

(require 'jirassic-jira)

(defvar level-indent (* level 2)
  "Number of whitespace characters of indentation per level.")

(defvar jirassic-serializer--supported-marks
  '(code em link strike strong subsup underline)
  "The type of text marks supported by `jirassic--serialize-to-org'.")

(cl-defgeneric jirassic--serialize-to-org (obj level)
  "Serialize OBJ to an org-mode string at heading LEVEL.")

(defun jirassic--string-repeat (num s)
  "Repeat string S NUM times."
  (apply #'concat (make-list num s)))

;;; `jira-heading' serialiser
(cl-defmethod jirassic--serialize-to-org ((obj jira-heading) level)
  "Convert a Jira heading OBJ to an org mode string at LEVEL."
  (declare (pure t) (side-effect-free t))
  (let ((stars (jira-heading-level obj)))
    (format "%s %s"
            (jirassic--string-repeat stars "*")
            (mapconcat (lambda (heading-part)
                         (jirassic--serialize-to-org heading-part level))
                       (jira-heading-content obj)))))

;;; `jira-text' serialiser
(cl-defmethod jirassic--serialize-to-org ((obj jira-text) level)
  "Return the text of a Jira text OBJ at LEVEL.

Only applies the first supported mark because of org syntax limitations."
  (declare (pure t) (side-effect-free t))
  (let (;; Get the first mark that is supported by the serializer (if any).
        (mark (car (seq-filter
                    (lambda (mark) (member (jira-mark-type mark)
                                           jirassic-serializer--supported-marks))
                    (jira-text-marks obj))))
        (text (jira-text-text obj)))
    (if mark
        (pcase (jira-mark-type mark)
          ('code      (format "~%s~" text))
          ('em        (format "/%s/" text))
          ('strike    (format "+%s+" text))
          ('strong    (format "*%s*" text))
          ('underline (format "_%s_" text))
          ('subsup    (format
                       (if (string= (alist-get 'attrs (jira-mark-attrs mark)) "sub")
                           "_{%s}"    ; subscript
                         "^{%s}")    ;superscript
                       text))
          ('link      (format "[[%s][%s]]" (alist-get 'href (jira-mark-attrs mark)) text))
          (type (warn "Unsupported Jira text mark %s" type)))
      text)))

(provide 'jirassic-org-serializer)
;;; jirassic-org-serializer.el ends here
