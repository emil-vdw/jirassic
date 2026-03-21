;;; jirassic-adf.el --- Atlassian Document Format definitions -*- lexical-binding: t; -*-

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

(cl-defstruct adf-issue id key description)

(cl-defstruct adf-doc content)
(cl-defstruct adf-heading content level)
(cl-defstruct adf-paragraph
  "A text paragraph that has a list of ADF objects as CONTENT."
  content)
(cl-defstruct adf-rule
  "A horizontal rule.")
(cl-defstruct adf-text
  "ADF text object that may have MARKS applied.

MARKS is a list of ADF-MARK objects."
  text marks)
(cl-defstruct adf-emoji text)
(cl-defstruct adf-mark
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
(cl-defstruct adf-bullet-list content)
(cl-defstruct adf-ordered-list content)
(cl-defstruct adf-list-item content)


(provide 'jirassic-adf)
;;; jirassic-adf.el ends here
