;;; jirassic-client.el --- Jira API client -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'plz)
(require 'auth-source)


(defcustom jirassic-host nil
  "Jira host URL."
  :type 'string
  :group 'jirassic)

(defun jirassic--host ()
  "Return the Jira host URL."
  (or jirassic-host
      (error "Jira host not set. Use `jirassic-set-host' to set it.")))

(defun jirassic--base-url ()
  "Return the base URL for the Jira API."
  (s-concat (s-chop-suffix "/" (jirassic--host)) "/rest/api/3/"))

(defun jirassic--credentials ()
  "Return the credentials for the Jira host."
  (car (auth-source-search
        :host (url-host (url-generic-parse-url jirassic-host))
        :require '(:user :secret)
        :max 1)))

(defun jirassic--http-headers (credentials)
  "Return the HTTP headers for the Jira API request."
  (let* ((username (plist-get credentials :user))
         (secret (plist-get credentials :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    `(("Authorization" .
       ,(concat "Basic "
                (base64-encode-string
                 (concat
                  username ":" token) t))))))

(cl-defun jirassic--get (segments &key params then)
  "Make a GET request to the Jira API with SEGMENTS and optional PARAMS.
SEGMENTS is a list of URL segments to append to the base URL. PARAMS is
an alist of query parameters to include in the request."
  (let* ((segments (or (and (listp segments)
                            segments)
                       (list segments)))
         (resource (s-join "/" segments))
         (url (s-concat (s-chop-suffix "/" (jirassic--base-url)) "/" resource
                        (when params
                          (concat "?" (url-build-query-string params)))))
         (credentials (jirassic--credentials))
         (headers (jirassic--http-headers credentials)))

    (plz 'get url
      :headers headers
      :as #'json-read
      :then then
      ;; XXX: Handle errors
      :else (lambda (&rest e)
              (message "error!\n%s" e)))))


(provide 'jirassic-client)
;;; jirassic-client.el ends here
