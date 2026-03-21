;;; jirassic-issue.el --- Jira issue definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emil van der Westhuizen
;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:
(require 'cl-lib)

(cl-defstruct jira-issue id key description)

(cl-defstruct jira-doc content)
(cl-defstruct jira-heading content level)
(cl-defstruct jira-paragraph
  "A text paragraph that has a list of Jira objects as CONTENT."
  content)
(cl-defstruct jira-rule
  "A horizontal rule.")
(cl-defstruct jira-text
  "Jira text object that may have MARKS applied.

MARKS is a list of JIRA-MARK objects."
  text marks)
(cl-defstruct jira-emoji text)
(cl-defstruct jira-mark
  "A mark that describes a text modifier.

Possible values for TYPE:
- `backgroundColor'
- `code'
- `em' (italic styling)
- `link'
  Will always have an href attribute in ATTRS
- `strike'
- `strong'
- `subsup'
  Subscript or superscript, indicated by `type' in ATTRS,
  either \"sub\" or \"sup\".
- `textColor'
- `underline'"
  type attrs)

;;; Lists
(cl-defstruct jira-bullet-list content)


(provide 'jirassic-jira)
;;; jirassic-issue.el ends here
