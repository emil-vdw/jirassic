;;; jirassic-parser.el --- Parse API response data -*- lexical-binding: t; -*-

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
       id key type)
    (make-jirassic-issue
     :id id
     :key key
     :creator (jirassic--parse-user creator-data)
     :description description
     :link link
     :status status
     :summary summary)))

(provide 'jirassic-parser)
;;; jirassic-parser.el ends here
