;;; jirassic.el --- An Org centered Jira client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created: 19 April 2025
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (org "9.5") (aio "1.0") (plz "0.9.1") (seq "2.24"))
;; Homepage: https://github.com/emil-vdw/jirassic
;; Keywords: tools, convenience, jira

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an Org centered Jira client.

;;; Code:
(defgroup jirassic nil
  "Org centered Jira client."
  :link '(url-link "https://github.com/emil-vdw/jirassic")
  :group 'tool)

(define-error 'jirassic-error "Jirassic error")

(provide 'jirassic)
;;; jirassic.el ends here
