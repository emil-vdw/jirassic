;;; jirassic.el --- An Org centered Jira client -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created: 19 April 2025
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.0.0") (s "1.12.0") (org "9.5") (aio "1.0") (f "0.20.0") (request "0.3.0"))
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

(provide 'jirassic)
;;; jirassic.el ends here
