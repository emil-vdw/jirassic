;;; jirassic-jira.el --- Atlassian Document Format definitions -*- lexical-binding: t; -*-

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

;;; Structs for Jira objects and ADF nodes.

;;; Code:
(require 'cl-lib)

(cl-defstruct jira-issue
  "Container for a Jira issue.

DESCRIPTION is an `adf-doc' node."
  id key summary description)

(cl-defstruct adf-doc content)

(cl-defstruct adf-heading
  "Represents a heading in the document.

LEVEL is an integer greater or equal to 1."
  content level)

(cl-defstruct adf-paragraph
  "Container for a block of formatted text delineated by a carriage return.

It's the equivalent of the HTML <p> tag.

CONTENT must contain one or more inline objects."
  content)

;;; Inline text objects
(cl-defstruct adf-text
  "ADF text object that may have MARKS applied.

MARKS is a list of ADF-MARK objects."
  text marks)

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

(cl-defstruct adf-status
  "Mutable inline node that represents the state of work.

TEXT is the textual representation of the status.
Neutral is the default and represents the grey color."
  text color)

(cl-defstruct adf-mention
  "Represents a user mention.

ID the Atlassian account ID or collection name of the person or
collection being mentioned.

TEXT the textual representation of the mention, including a leading @.
TEXT is optional and may be nil."
  id text)

(cl-defstruct adf-inline-card
  "An Atlassian link card with a type icon and content description.

URL contains the link and DATA is a JSONLD representation of the link.
Either DATA or URL must be provided, but not both.

Exampl DATA value:
'((@context . \"https://schema.org\")
  (@type . \"DigitalDocument\")
  (@id . \"https://mysite.atlassian.net/wiki/spaces/~123/pages/456\")
  (name . \"My Confluence Page\")
  (summary . \"A short description\")
  (url . \"https://mysite.atlassian.net/...\")
  (generator . ((@type . \"Application\")
                (name . \"Confluence\"))))"
  url data)

(cl-defstruct adf-hard-break
  "Inserts a new line in a text string.

It's the equivalent to a <br/> in HTML.")

(cl-defstruct adf-date
  "Displays a date in the user's locale.

TIMESTAMP is a unix timestamp of the date as a string."
  timestamp)
;;; Inline ends here

(cl-defstruct adf-rule
  "A horizontal rule.")

(cl-defstruct adf-emoji
  "An inline node that represents an emoji.

TEXT contains the emoji to display"
  text)

(cl-defstruct adf-code-block
  "A container of lines of code.

CONTENT takes an array of one or more `adf-text' objects without marks.
LANGUAGE may be provided as a string, e.g. \"python\"."
  content language)

(cl-defstruct adf-blockquote
  "A container for quotes.

- `adf-paragraph' with no marks
- `adf-bullet-list'
- `adf-ordered-list'
- `adf-code-block'
- `adf-media-group'
- `adf-media-single'"
  content)

;;; Media objects
(cl-defstruct adf-media-single
  "Container for one media item.

Enables the display of the content in full, in contrast to a mediaGroup
that is intended for a list of attachments.

CONTENT must be a media node."
  content)

(cl-defstruct adf-media-group
  "Container for several media items.

Compare to mediaSingle, which is intended for the display of a single
media item in full.

CONTENT must contain one or more media nodes."
  content)

(cl-defstruct adf-media
  "Represents a single file or link stored in media services.

`adf-media' is a child of either:
- `adf-media-group'
- `adf-media-single'")

;;; Lists
(cl-defstruct adf-bullet-list content)
(cl-defstruct adf-ordered-list content)
(cl-defstruct adf-list-item
  "An item in a list.

content must contain at least one of the following nodes:
- `adf-bullet-list'
- `adf-code-block'
- `adf-media-single'
- `adf-ordered-list'
- `adf-paragraph' with no `adf-mark'"
  content)

(defun jirassic-jira-block-node-p (node)
  "Return t if NODE is as block node."
  (cl-typep node '(or
                   ;; Top level block nodes
                   adf-blockquote
                   adf-bullet-list
                   adf-code-block
                   ;; adf-expand
                   adf-heading
                   adf-media-group
                   adf-media-single
                   adf-ordered-list
                   ;; adf-panel
                   adf-paragraph
                   adf-rule
                   ;; adf-table

                   ;; Child block nodes:
                   adf-list-item
                   adf-media
                   ;; adf-nested-expand
                   ;; adf-table-cell
                   ;; adf-table-header
                   ;; adf-table-row
                   )))

(cl-defgeneric jirassic-adjust-heading-level (obj amount)
  "Recursively promote or demote all headings in OBJ by AMOUNT.

AMOUNT may be a positive or negative integer, negative values demote and
positive values promote headings.")

(cl-defmethod jirassic-adjust-heading-level ((node t) _amount)
  "NODE should not contain any headings, return as is."
  node)

(cl-defmethod jirassic-adjust-heading-level ((heading adf-heading) amount)
  "Adjust HEADING by adding AMOUNT but never less than 1."
  (let ((new-heading (copy-adf-heading heading)))
    (setf (adf-heading-level new-heading)
          (max 1 (+ (adf-heading-level heading) amount)))
    new-heading))

(cl-defmethod jirassic-adjust-heading-level ((doc adf-doc) amount)
  "Adjust all heading nodes in DOC by AMOUNT."
  (let ((new-doc (copy-adf-doc doc)))
    (setf (adf-doc-content new-doc)
          (mapcar (lambda (node)
                    (jirassic-adjust-heading-level node amount))
                  (adf-doc-content doc)))
    new-doc))

(cl-defmethod jirassic-adjust-heading-level ((issue jira-issue) amount)
  "Return a copy of ISSUE with all heading adjusted by AMOUNT."
  (let ((new-issue (copy-jira-issue issue)))
    (setf (jira-issue-description new-issue)
          (jirassic-adjust-heading-level (jira-issue-description issue) amount))
    new-issue))

(provide 'jirassic-jira)
;;; jirassic-jira.el ends here
