;;; jirassic-parser.el --- Parse API response data -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

(require 'cl-lib)

(require 'jirassic-issue)


(defmacro jirassic--rbind (alist bindings &rest body)
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

(defun jirassic--parse-issue (issue-data)
  ""
  (jirassic--rbind issue-data
      ((id)
       ())))

(provide 'jirassic-parser)
;;; jirassic-parser.el ends here
