;;; jirassic-jira-parser.el --- Parses ADF alists into ADF objects -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Parses ADF structs to org format.

;;; Code:
(require 'jirassic-jira)

(defun jirassic-parse-adf-node (node)
  "Convert an alist of data for a Jira ADF NODE into its corresponding object."
  (declare (pure t) (side-effect-free t))
  (let-alist node
    (pcase .type
      ("doc" (jirassic--parse-doc node))
      ("heading" (jirassic--parse-heading node))
      ("text" (jirassic--parse-text node))
      ("codeBlock" (jirassic--parse-code-block node))
      ("rule" (make-adf-rule))
      ;; ((type . "emoji")
      ;;  (attrs (shortName . ":thinking:")
      ;;         (id . "1f914") (text . "🤔")))
      ("emoji" (make-adf-emoji :text .attrs.text))
      ;; ((type . "bulletList")
      ;;  (content
      ;;   . [((type . "listItem")
      ;;       (content . [((type . "paragraph")
      ;;                    (content . [((type . "text") (text . "First line"))]))]))
      ;;      ((type . "listItem")
      ;;       (content . [((type . "paragraph")
      ;;                    (content . [((type . "text")(text . "Second line"))]))]))]))
      ("bulletList" (make-adf-bullet-list :content (jirassic--parse-content-list .content)))
      ("orderedList" (make-adf-ordered-list :content (jirassic--parse-content-list .content)))
      ("listItem" (make-adf-list-item :content (jirassic--parse-content-list .content)))
      ("paragraph" (jirassic--parse-paragraph node))
      ("blockquote" (jirassic--parse-blockquote node))
      (_ (warn "Unsupported ADF node type %s" .type)
         ;; Return `nil' so this node can be filtered out.
         nil))))

(defun jirassic--parse-issue (issue)
  "Parse a Jira ISSUE object."
  (let-alist issue
   (make-jira-issue
    :id .id :key .key
    :description (jirassic-parse-adf-node .fields.description)
    :summary .fields.summary)))

(defun jirassic--parse-doc (doc)
  "Parse an ADF DOC node."
  (declare (pure t) (side-effect-free t))
  (make-adf-doc :content (jirassic--parse-content-list (alist-get 'content doc))))

(defun jirassic--parse-content-list (content)
  "Parse all ADF nodes in CONTENT to a list of Jira objects."
  (declare (pure t) (side-effect-free t))
  (seq-remove
   ;; Remove all occurrences of `nil'. This happens when parsing an
   ;; unsupported node type, which we will warn the user about,
   ;; continue with what we support.
   #'null
   (mapcar #'jirassic-parse-adf-node content)))

(defun jirassic--parse-heading (heading)
  "Create an `adf-heading' object from a HEADING ADF node."
  ;; Example ADF alist:
  ;; ((type . "heading") (attrs (level . 2))
  ;;  (content
  ;;   . [((type . "text")
  ;;       (text . "Areas to investigate"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist heading
    (make-adf-heading :content (jirassic--parse-content-list .content)
                      :level .attrs.level)))

(defun jirassic--parse-text (text)
  "Create an ADF-TEXT object from the TEXT ADF node."
  ;; Example ADF alist:
  ;; ((type . "text")
  ;;  (text
  ;;   . "Review metrics included in the current design : ")
  ;;  (marks
  ;;   . [((type . "strong"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist text
    (make-adf-text :text .text
                   :marks (when .marks
                            (mapcar
                             (lambda (mark)
                               (make-adf-mark :type (intern (alist-get 'type mark))
                                              :attrs (alist-get 'attrs mark)))
                             .marks)))))

(defun jirassic--parse-code-block (code-block)
  "Create an `adf-code-block' object from a CODE-BLOCK node.

See the definition of `adf-code-block' for the constraints of
`adf-code-block-content'."
  ;; Example code block:
  ;; ((type . "codeBlock")
  ;;  (attrs (language . "python"))
  ;;  (content
  ;;   . [((type . "text")
  ;;       (text
  ;;        . "class Foo:\n    x: int = 5\n\nf = Foo()"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist code-block
    (make-adf-code-block
     :content (jirassic--parse-content-list .content)
     :language .attrs.language)))

(defun jirassic--parse-paragraph (paragraph)
  "Create an `adf-paragraph' object from a PARAGRAPH ADF node."
  ;; Example ADF alist:
  ;; ((type . "paragraph")
  ;;  (content . [((type . "text")
  ;;               (text . "Hello world"))]))
  (declare (pure t) (side-effect-free t))
  (let-alist paragraph
    (make-adf-paragraph :content (jirassic--parse-content-list .content))))

(defun jirassic--parse-blockquote (blockquote)
  "Create an `adf-blockquote' object from a BLOCKQUOTE ADF node."
  ;; Example ADF alist:
  ;; ((type . "blockquote")
  ;;  (content
  ;;   . [((type . "paragraph")
  ;;       (content
  ;;        . [((type . "text")
  ;;            (text . "This is a"))]))
  ;;      ((type . "paragraph")
  ;;       (content
  ;;        . [((type . "text")
  ;;            (text . "multiline quote"))]))]))
  (declare (pure t) (side-effect-free t))
  (let-alist blockquote
    (make-adf-blockquote :content (jirassic--parse-content-list .content))))

(provide 'jirassic-jira-parser)
;;; jirassic-jira-parser.el ends here
