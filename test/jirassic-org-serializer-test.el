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

  ;; Content with a mark
  (should (string= (jirassic--serialize-to-org
                    (make-adf-heading
                     :level 3
                     :content (list (make-adf-text :text "Emphasized"
                                                   :marks (list (make-adf-mark :type 'em))))))
                   "*** /Emphasized/")))

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
