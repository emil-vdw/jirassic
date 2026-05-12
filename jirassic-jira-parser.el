;;; jirassic-jira-parser.el --- Parses ADF alists into ADF objects -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Parses ADF structs to org format.

;;; Code:
(require 'jirassic-jira)

(defun jirassic-parse-adf-node (node)
  "Convert an alist of data for a Jira ADF NODE into its corresponding object."
  ;; Some examples are of the data for the relevant node are given
  ;; here inline, node types that are created in their own function,
  ;; you can find the example there.
  (declare (pure t) (side-effect-free t))
  (let-alist node
    (pcase .type
      ("doc" (jirassic--parse-doc node))
      ("heading" (jirassic--parse-heading node))
      ("text" (jirassic--parse-text node))
      ("codeBlock" (jirassic--parse-code-block node))
      ("rule" (make-adf-rule))
      ;; ((type . "hardBreak"))
      ("hardBreak" (make-adf-hard-break))
      ;; ((type . "emoji")
      ;;  (attrs (shortName . ":thinking:")
      ;;         (id . "1f914") (text . "🤔")))
      ("emoji" (make-adf-emoji :text .attrs.text))
      ;; ((type . "mention")
      ;;  (attrs (id . "5dd37afd98792b0ef9d9c3dc")
      ;;         (text . "@John Doe")
      ;;         (accessLevel . "APPLICATION")))
      ("mention" (make-adf-mention :id .attrs.id :text .attrs.text))
      ;; ((type . "date") (attrs (timestamp . "1582070400000")))
      ("date" (make-adf-date :timestamp .attrs.timestamp))
      ;; ((type . "status")
      ;;  (attrs (text . "In Progress")
      ;;         (color . "blue")
      ;;         (localId . "abc-123")
      ;;         (style . "")))
      ("status" (make-adf-status :text .attrs.text :color .attrs.color))
      ;; ((type . "bulletList")
      ;;  (content
      ;;   . [((type . "listItem") (content . [...]))
      ;;      ((type . "listItem") (content . [...]))]))
      ("bulletList" (make-adf-bullet-list :content (jirassic--parse-content-list .content)))
      ("orderedList" (make-adf-ordered-list :content (jirassic--parse-content-list .content)))
      ("listItem" (make-adf-list-item :content (jirassic--parse-content-list .content)))
      ("paragraph" (jirassic--parse-paragraph node))
      ("blockquote" (jirassic--parse-blockquote node))
      ("panel" (jirassic--parse-panel node))
      ("expand" (jirassic--parse-expand node))
      ;; ((type . "inlineCard") (attrs (url . "https://acme.com")))
      ;; ((type . "inlineCard") (attrs (data (@context . "https://schema.org") ...)))
      ("inlineCard" (make-adf-inline-card :url .attrs.url :data .attrs.data))
      ("table" (jirassic--parse-table node))
      ;; ((type . "tableRow")
      ;;  (content . [((type . "tableHeader") ...) ((type . "tableCell") ...)]))
      ("tableRow" (make-adf-table-row :content (jirassic--parse-content-list .content)))
      ("tableHeader" (make-adf-table-header
                      :content (jirassic--parse-content-list .content)
                      :row-span .attrs.rowspan :col-span .attrs.colspan))
      ("tableCell" (make-adf-table-cell
                    :content (jirassic--parse-content-list .content)
                    :row-span .attrs.rowspan :col-span .attrs.colspan))
      (_ (lwarn 'jirassic :warning "Unsupported ADF node type %s" .type)
         ;; Return `nil' so this unsupported node can be filtered out.
         nil))))

(defun jirassic--parse-issue (issue)
  "Parse a Jira ISSUE object."
  (let-alist issue
    (make-jira-issue
     :id .id :key .key
     :url (when .self
            (concat (replace-regexp-in-string "/rest/api/.*$" "" .self)
                    "/browse/" .key))
     :description (when .fields.description
                    (jirassic-parse-adf-node .fields.description))
     :status .fields.status.name
     :summary .fields.summary
     :type .fields.issuetype.name
     :priority .fields.priority.name
     :creator (when .fields.creator
                (make-jira-user
                 :account-id .fields.creator.accountId
                 :display-name .fields.creator.displayName
                 :email .fields.creator.emailAddress))
     :project (when .fields.project
                (make-jira-project
                 :id .fields.project.id
                 :key .fields.project.key
                 :name .fields.project.name)))))

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

(defun jirassic--parse-table (table)
  "Create an `adf-table' object from a TABLE ADF node."
  ;; Example ADF alist:
  ;; ((type . "table")
  ;;  (attrs (isNumberColumnEnabled . t))
  ;;  (content
  ;;   . [((type . "tableRow")
  ;;       (content
  ;;        . [((type . "tableHeader")
  ;;            (content . [((type . "paragraph")
  ;;                         (content . [((type . "text") (text . "Name"))]))]))
  ;;           ((type . "tableCell")
  ;;            (content . [((type . "paragraph")
  ;;                         (content . [((type . "text") (text . "foo"))]))]))]))]))
  (declare (pure t) (side-effect-free t))
  (let-alist table
    (make-adf-table
     :content (jirassic--parse-content-list .content)
     :is-numbere-columns-enabled .attrs.isNumberColumnEnabled)))

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

(defun jirassic--parse-panel (panel)
  "Create an `adf-panel' object from a PANEL ADF node."
  ;; Example ADF alist:
  ;; ((type . "panel")
  ;;  (attrs (panelType . "warning")
  ;;         (localId . "088d70b6fe14"))
  ;;  (content
  ;;   . [((type . "paragraph")
  ;;       (content . [((type . "text") (text . "Heads up!"))]))]))
  (declare (pure t) (side-effect-free t))
  (let-alist panel
    (make-adf-panel
     :content (jirassic--parse-content-list .content)
     :panel-type .attrs.panelType)))

(defun jirassic--parse-expand (expand)
  "Create an `adf-expand' object from an EXPAND ADF node."
  ;; Example ADF alist:
  ;; ((type . "expand")
  ;;  (attrs (title . "Hello world"))
  ;;  (content
  ;;   . [((type . "paragraph")
  ;;       (content . [((type . "text") (text . "Hello world"))]))]))
  ;;
  ;; `attrs.title' is optional and may be absent or nil.
  (declare (pure t) (side-effect-free t))
  (let-alist expand
    (make-adf-expand
     :content (jirassic--parse-content-list .content)
     :title .attrs.title)))

(provide 'jirassic-jira-parser)
;;; jirassic-jira-parser.el ends here
