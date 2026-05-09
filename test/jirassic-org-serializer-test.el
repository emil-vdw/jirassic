;;; jirassic-org-serializer-test.el --- Tests for jirassic-org-serializer -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)

(require 'jirassic-org-serializer)

;;; `adf-text'
(ert-deftest jirassic-serializer-test-text-code ()
  (let ((link (make-adf-text
               :text "Order 66"
               :marks (list
                       (make-adf-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-link ()
  (let ((link (make-adf-text
               :text "Order 66"
               :marks (list
                       (make-adf-mark
                        :type 'link
                        :attrs '((href . "https://acme.com/order-66/")))))))
    (should (string= (jirassic--serialize-to-org link 0)
                     "[[https://acme.com/order-66/][Order 66]]"))))

(ert-deftest jirassic-serializer-test-text-whitespace ()
  "Test that text formatted with emphasis markers handle whitespace correctly."
  ;; Leading whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  leading whitespace"
                            :marks (list (make-adf-mark :type 'strong))))
           "  *leading whitespace*"))

  ;; Trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "trailing whitespace   "
                            :marks (list (make-adf-mark :type 'em))))
           "/trailing whitespace/   "))

  ;; Leading and trailing whitespace
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  space "
                            :marks (list (make-adf-mark
                                          :type 'link
                                          :attrs '((href . "https://acme.com/"))))))
           "  [[https://acme.com/][space]] "))

  ;; Whitespace with no marker
  (should (string=
           (jirassic--serialize-to-org
            (make-adf-text :text "  space "))
           "  space ")))

;;; `adf-bullet-list'
(ert-deftest jirassic-serializer-test-bullet-list ()
  (let ((bullet-list (make-adf-bullet-list
                      :content (list (make-adf-list-item
                                      :content (list (make-adf-text :text "first bullet")))
                                     (make-adf-list-item
                                      :content (list (make-adf-text :text "second bullet")))
                                     (make-adf-list-item
                                      :content (list (make-adf-text :text "third bullet")))))))
    (should (string= (jirassic--serialize-to-org bullet-list)
                     "- first bullet\n- second bullet\n- third bullet"))))

;;; `adf-ordered-list'
(ert-deftest jirassic-serializer-test-ordered-list ()
  ;; Basic numbered list
  (let ((ordered-list
         (make-adf-ordered-list
          :content (list (make-adf-list-item
                          :content (list (make-adf-paragraph
                                          :content (list (make-adf-text :text "first item")))))
                         (make-adf-list-item
                          :content (list (make-adf-paragraph
                                          :content (list (make-adf-text :text "second item")))))
                         (make-adf-list-item
                          :content (list (make-adf-paragraph
                                          :content (list (make-adf-text :text "third item")))))))))
    (should (string= (jirassic--serialize-to-org ordered-list)
                     "1. first item\n2. second item\n3. third item")))

  ;; Indented (level 1)
  (let ((ordered-list
         (make-adf-ordered-list
          :content (list (make-adf-list-item
                          :content (list (make-adf-paragraph
                                          :content (list (make-adf-text :text "item")))))))))
    (should (string= (jirassic--serialize-to-org ordered-list 1)
                     "  1. item"))))

;;; `adf-date'
(ert-deftest jirassic-serializer-test-date ()
  (let ((system-time-locale "en_GB.UTF-8"))
    (should (string= (jirassic--serialize-to-org
                      (make-adf-date :timestamp "1582152559"))
                     "<2020-02-19 Wed>"))

    ;; Test the empty string case since the API doesn't gaurentee a non-empty string.
    (should (string= (jirassic--serialize-to-org
                      (make-adf-date :timestamp ""))
                     "<1970-01-01 Thu>"))))

;;; `adf-hard-break'
(ert-deftest jirassic-serializer-test-hard-break ()
  (should (string= (jirassic--serialize-to-org (make-adf-hard-break))
                   "\n")))

;;; `adf-rule'
(ert-deftest jirassic-serializer-test-rule ()
  (should (string= (jirassic--serialize-to-org (make-adf-rule))
                   "-----")))

;;; `adf-emoji'
(ert-deftest jirassic-serializer-test-emoji ()
  (should (string= (jirassic--serialize-to-org (make-adf-emoji :text ":smile:"))
                   ":smile:")))

;;; `adf-mention'
(ert-deftest jirassic-serializer-test-mention ()
  ;; TEXT is used as the link description when present.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-mention :id "abc123" :text "@John Doe"))
                   "[[mention:abc123][@John Doe]]"))

  ;; Falls back to @id as the description when TEXT is nil.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-mention :id "abc123"))
                   "[[mention:abc123][@abc123]]")))

;;; `adf-status'
(ert-deftest jirassic-serializer-test-status ()
  ;; Status text is wrapped in brackets.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-status :text "In Progress" :color "blue"))
                   "=In Progress="))

  ;; nil text produces empty verbatim markers.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-status))
                   "==")))

;;; `adf-inline-card'
(ert-deftest jirassic-serializer-test-inline-card ()
  ;; URL variant — no display text
  (should (string= (jirassic--serialize-to-org
                    (make-adf-inline-card :url "https://acme.com"))
                   "[[https://acme.com]]"))

  ;; Data variant with name — becomes a described link
  (should (string= (jirassic--serialize-to-org
                    (make-adf-inline-card
                     :data '((url . "https://acme.com/page")
                             (name . "My Page"))))
                   "[[https://acme.com/page][My Page]]"))

  ;; Data variant without name — falls back to bare link
  (should (string= (jirassic--serialize-to-org
                    (make-adf-inline-card
                     :data '((@id . "https://acme.com/page"))))
                   "[[https://acme.com/page]]")))

;;; `adf-code-block'
(ert-deftest jirassic-serializer-test-code-block ()
  ;; With language
  (should (string= (jirassic--serialize-to-org
                    (make-adf-code-block
                     :language "python"
                     :content (list
                               (make-adf-text :text
                                              "class Foo:\n    x: int = 5\n\nf = Foo()"))))
                   "#+BEGIN_SRC python\nclass Foo:\n    x: int = 5\n\nf = Foo()\n#+END_SRC"))

  ;; Without language
  (should (string= (jirassic--serialize-to-org
                    (make-adf-code-block
                     :content (list (make-adf-text :text "hello"))))
                   "#+BEGIN_SRC\nhello\n#+END_SRC"))

  ;; Empty string language treated as omitted
  (should (string= (jirassic--serialize-to-org
                    (make-adf-code-block
                     :language ""
                     :content (list (make-adf-text :text "hello"))))
                   "#+BEGIN_SRC\nhello\n#+END_SRC"))

  ;; Language "none"
  (should (string= (jirassic--serialize-to-org
                    (make-adf-code-block
                     :language "none"
                     :content (list (make-adf-text :text "hello"))))
                   "#+BEGIN_SRC\nhello\n#+END_SRC")))

;;; `adf-paragraph'
(ert-deftest jirassic-serializer-test-paragraph ()
  ;; Plain text content
  (should (string= (jirassic--serialize-to-org
                    (make-adf-paragraph
                     :content (list (make-adf-text :text "Hello world"))))
                   "Hello world"))

  ;; Multiple inline nodes concatenated
  (should (string= (jirassic--serialize-to-org
                    (make-adf-paragraph
                     :content (list (make-adf-text :text "Hello ")
                                    (make-adf-text :text "world"
                                                   :marks (list (make-adf-mark :type 'strong))))))
                   "Hello *world*")))

;;; `adf-heading'
(ert-deftest jirassic-serializer-test-heading ()
  ;; Level 1
  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 1
                     :content (list (make-adf-text :text "Top level"))))
                   "* Top level"))

  ;; Level 2
  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 2
                     :content (list (make-adf-text :text "Subsection"))))
                   "** Subsection"))

  ;; Emphasis marks are stripped from heading text — org-mode does not apply
  ;; them inside headings and `strong' would conflict with the `*' heading stars.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 3
                     :content (list (make-adf-text :text "Emphasized"
                                                   :marks (list (make-adf-mark :type 'em))))))
                   "*** Emphasized"))

  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 2
                     :content (list (make-adf-text :text "Bold"
                                                   :marks (list (make-adf-mark :type 'strong))))))
                   "** Bold"))

  ;; Links are kept — [[url][desc]] is valid inside an org heading.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 1
                     :content (list (make-adf-text
                                     :text "Click here"
                                     :marks (list (make-adf-mark
                                                   :type 'link
                                                   :attrs '((href . "https://acme.com"))))))))
                   "* [[https://acme.com][Click here]]")))

;;; `adf-doc'
(ert-deftest jirassic-serializer-test-doc ()
  ;; Single paragraph
  (should (string= (jirassic--serialize-to-org
                    (make-adf-doc
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "Hello world"))))))
                   "Hello world"))

  ;; Multiple nodes: two newlines are inserted before a heading
  (should (string= (jirassic--serialize-to-org
                    (make-adf-doc
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "Intro")))
                                    (make-adf-heading
                                     :level 1
                                     :content (list (make-adf-text :text "Section"))))))
                   "Intro\n\n* Section")))

;;; Test serializing content lists
(ert-deftest jirassic-serializer-test-content-line-between-headings ()
  "Test serializing content of a node containing headings."
  ;; Sibling headings respect `jirassic-blank-line-between-headings'
  (let ((content (list (make-adf-heading :content (list (make-adf-text :text "sibling1"))
                                         :level 1)
                       (make-adf-heading :content (list (make-adf-text :text "sibling2"))
                                         :level 1))))
    (let ((jirassic-blank-line-between-headings t))
      (should
       (string= (jirassic-serializer--serialize-content-list content)
                "* sibling1\n\n* sibling2")))

    (let ((jirassic-blank-line-between-headings nil))
      (should
       (string= (jirassic-serializer--serialize-content-list content)
                "* sibling1\n* sibling2"))))

  ;; Child headings have no blank line between them and the parent.
  (let ((content (list (make-adf-heading :content (list (make-adf-text :text "parent"))
                                         :level 1)
                       (make-adf-heading :content (list (make-adf-text :text "child"))
                                         :level 2))))
    (let ((jirassic-blank-line-between-headings t))
      (should
       (string= (jirassic-serializer--serialize-content-list content)
                "* parent\n** child")))))

;;; `adf-expand'
(ert-deftest jirassic-serializer-test-expand ()
  ;; Title is emitted as a block argument so it stays attached to the block.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-expand
                     :title "Hello world"
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "Hello world"))))))
                   "#+BEGIN_EXPAND Hello world\nHello world\n#+END_EXPAND"))

  ;; nil title — no block argument.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-expand
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "body"))))))
                   "#+BEGIN_EXPAND\nbody\n#+END_EXPAND"))

  ;; Empty-string title is treated the same as nil.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-expand
                     :title ""
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "body"))))))
                   "#+BEGIN_EXPAND\nbody\n#+END_EXPAND")))

;;; `adf-panel'
(ert-deftest jirassic-serializer-test-panel ()
  ;; Panel type drives the block name.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-panel
                     :panel-type "warning"
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "Heads up!"))))))
                   "#+BEGIN_WARNING\nHeads up!\n#+END_WARNING"))

  ;; Multiple block children get the standard inter-block spacing.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-panel
                     :panel-type "info"
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "First.")))
                                    (make-adf-paragraph
                                     :content (list (make-adf-text :text "Second."))))))
                   "#+BEGIN_INFO\nFirst.\nSecond.\n#+END_INFO"))

  ;; Missing panel type falls back to the generic block name.
  (should (string= (jirassic--serialize-to-org
                    (make-adf-panel
                     :content (list (make-adf-paragraph
                                     :content (list (make-adf-text :text "x"))))))
                   "#+BEGIN_PANEL\nx\n#+END_PANEL")))

;;; `adf-table'
(ert-deftest jirassic-serializer-test-table-plain ()
  "Table with plain text cells serializes to a correctly padded org table."
  (let ((table
         (make-adf-table
          :content
          (list (make-adf-table-row
                 :content
                 (list (make-adf-table-header
                        :content (list (make-adf-paragraph
                                        :content (list (make-adf-text :text "Name")))))
                       (make-adf-table-header
                        :content (list (make-adf-paragraph
                                        :content (list (make-adf-text :text "Value")))))))
                (make-adf-table-row
                 :content
                 (list (make-adf-table-cell
                        :content (list (make-adf-paragraph
                                        :content (list (make-adf-text :text "foo")))))
                       (make-adf-table-cell
                        :content (list (make-adf-paragraph
                                        :content (list (make-adf-text :text "bar")))))))))))
    (should (string= (jirassic--serialize-to-org table)
                     (concat "| Name | Value |\n"
                             "|------+-------|\n"
                             "| foo  | bar   |")))))

(ert-deftest jirassic-serializer-test-table-emphasis-column-width ()
  "Table column widths account for `org-hide-emphasis-markers'.

With markers visible the emphasis chars count toward the column width.
With markers hidden the column is sized to the visible text and cells get
extra padding to compensate for the hidden marker characters."
  (let* ((bold-mark (list (make-adf-mark :type 'strong)))
         (table
          (make-adf-table
           :content
           (list (make-adf-table-row
                  :content
                  (list (make-adf-table-header
                         :content (list (make-adf-paragraph
                                         :content (list (make-adf-text :text "Name")))))
                        (make-adf-table-header
                         :content (list (make-adf-paragraph
                                         :content (list (make-adf-text :text "Bold")))))))
                 (make-adf-table-row
                  :content
                  (list (make-adf-table-cell
                         :content (list (make-adf-paragraph
                                         :content (list (make-adf-text :text "foo")))))
                        (make-adf-table-cell
                         :content (list (make-adf-paragraph
                                         :content (list (make-adf-text :text "bar"
                                                                        :marks bold-mark)))))))))))
    ;; Markers visible: *bar* is 5 chars, so col2 width = max(4,5)+2 = 7.
    (let ((org-hide-emphasis-markers nil))
      (should (string= (jirassic--serialize-to-org table)
                       (concat "| Name | Bold  |\n"
                               "|------+-------|\n"
                               "| foo  | *bar* |"))))
    ;; Markers hidden: *bar* is 3 chars, so col2 width = max(4,3)+2 = 6.
    ;; The *bar* cell gets 3 spaces of padding (to fill 6 visible chars) rather than 2.
    (let ((org-hide-emphasis-markers t))
      (should (string= (jirassic--serialize-to-org table)
                       (concat "| Name | Bold |\n"
                               "|------+------|\n"
                               "| foo  | *bar*  |"))))))

;;; fallback serializer
(cl-defstruct jirassic-test--unsupported-node)

(ert-deftest jirassic-serializer-test-fallback ()
  (should (string= (jirassic--serialize-to-org (make-jirassic-test--unsupported-node))
                   "###unsupported ADF node: jirassic-test--unsupported-node###")))

;;; Utilities to help us test serializing larger issues from with
;;; expected representations from org files.
(defvar jirassic-serializer-test--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun jirassic-serializer-test--output-should-match (expected actual)
  "If EXPECTED and ACTUAL differ, signal an ERT failure with a unified diff."
  (if noninteractive
      ;; If running non-interactively, just compare normally.
      (should (string= expected actual))
    ;; If running interactively, then show the differences in a diff
    ;; buffer to make it easy to debug.
    (unless (string= expected actual)
      (let* ((file-expected (make-temp-file "expected-" nil ".org" expected))
             (file-actual   (make-temp-file "actual-"   nil ".org" actual))
             (buf (diff-no-select file-expected file-actual)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (rename-buffer "*test-diff*" t)
                (diff-mode))
              (display-buffer buf)
              (ert-fail "Output mismatch — see *test-diff* buffer"))
          (delete-file file-expected)
          (delete-file file-actual))))))

(defun jirassic-serializer-test--read-fixture-alist (fixture-path)
  "Read and eval an elisp fixture from FIXTURE-PATH.

FIXTURE-PATH is relative to `jirassic-serializer-test--fixtures-dir'."
  (let ((path (expand-file-name fixture-path jirassic-serializer-test--fixtures-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (read (current-buffer)))))

(defun jirassic-serializer-test--read-fixture-string (fixture-path)
  "Read a fixture as a string from FIXTURE-PATH.

FIXTURE-PATH is relative to `jirassic-serializer-test--fixtures-dir'."
  (let ((path (expand-file-name fixture-path jirassic-serializer-test--fixtures-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(provide 'jirassic-org-serializer-test)
;;; jirassic-org-serializer-test.el ends here
