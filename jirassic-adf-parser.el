;;; jirassic-adf-parser.el --- Parses ADF alists into Jira objects -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Parses jira structs to org format.

;;; Code:
(require 'jirassic-jira)

(defun jirassic-parse-adf-node (node)
  ""
  (declare (pure t) (side-effect-free t))
  (let-alist node
    (pcase .type
      ("heading" (jirassic--parse-heading node))
      ("text" (jirassic--parse-text node)))))

(defun jirassic--parse-content (content)
  "Parse all ADF nodes in CONTENT to a list of Jira objects."
  (declare (pure t) (side-effect-free t))
  (mapcar #'jirassic-parse-adf-node content))

(defun jirassic--parse-heading (heading)
  "Create a JIRA-HEADING object from a HEADING ADF node."
  ;; Example ADF alist:
  ;; ((type . "heading") (attrs (level . 2))
  ;;  (content
  ;;   . [((type . "text")
  ;;       (text . "Areas to investigate"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist heading
    (make-jira-heading :content (jirassic--parse-content .content)
                       :level .attrs.level)))

(defun jirassic--parse-text (text)
  "Create a JIRA-TEXT object from the TEXT ADF node."
  ;; Example ADF alist:
  ;; ((type . "text")
  ;;  (text
  ;;   . "Review metrics included in the current design : ")
  ;;  (marks
  ;;   . [((type . "strong"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist text
    (make-jira-text :text .text
                    :marks (when .marks
                             (mapcar
                              (lambda (mark)
                                (make-jira-mark :type (intern (alist-get 'type mark))
                                                :attrs (alist-get 'attrs mark)))
                              .marks)))))

(provide 'jirassic-adf-parser)
;;; jirassic-adf-parser.el ends here
