;;; jirassic-core.el --- Shared base definitions for Jirassic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;;; Commentary:

;; Base definitions (group, error conditions) required by all Jirassic modules.
;; This file has no internal dependencies and must not require any other
;; jirassic-*.el file.

;;; Code:

(defgroup jirassic nil
  "Org centered Jira client."
  :link '(url-link "https://github.com/emil-vdw/jirassic")
  :group 'tool)

(define-error 'jirassic-error "Jirassic error")

(provide 'jirassic-core)
;;; jirassic-core.el ends here
