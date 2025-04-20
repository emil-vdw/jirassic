;;; jirassic-parser.el --- Parse API response data -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>


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

(require 'jirassic-issue)


(defmacro jirassic-alist-bind (alist bindings &rest body)
  "Bind variables in BINDINGS to values in ALIST."
  (declare (indent 2))
  `(let ,(mapcar (lambda (binding)
                   (let* ((var (if (listp binding)
                                   (car binding)
                                 binding))
                          (key (if (listp binding)
                                   (cadr binding)
                                 binding)))
                     (if (listp key)
                         ;; Handle nested keys
                         `(,var (--reduce-from
                                 (alist-get it acc)
                                 ,alist ',key))
                       ;; Handle top-level keys
                       `(,var (alist-get ',key ,alist)))))
                 bindings)
     ,@body))

(defun jirassic--parse-user (user-data)
  "Parse USER-DATA into a `jirassic-user' struct."
  (jirassic-alist-bind user-data
      ((link self)
       (account-id accountId)
       (email-address emailAddress)
       (display-name displayName))
    (make-jirassic-user
     :account-id account-id
     :link link
     :email-address email-address
     :display-name display-name)))

(defun jirassic--parse-issue (issue-data)
  "Parse ISSUE-DATA into a `jirassic-issue' struct."
  (jirassic-alist-bind issue-data
      ((link self)
       (description (fields description))
       (status (fields status name))
       (summary (fields summary))
       (creator-data (fields creator))
       (project (fields project key))
       (type (fields issuetype name))
       (priority (fields priority name))
       (attachments (fields attachment))
       id key)
    (make-jirassic-issue
     :id id
     :key key
     :type type
     :priority priority
     :project project
     :creator (jirassic--parse-user creator-data)
     :description description
     :link link
     :status status
     :summary summary
     :attachments attachments)))

(provide 'jirassic-parser)
;;; jirassic-parser.el ends here
