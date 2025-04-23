;;; jirassic-client.el --- Jira API client -*- lexical-binding: t; -*-

;;; Commentary:



;;; Code:

(require 'aio)
(require 'request)

(require 'auth-source)


(defcustom jirassic-host nil
  "Jira host URL."
  :type 'string
  :group 'jirassic)

(defcustom jirassic-overwrite-attachments nil
  "Overwrite existing attachments."
  :type 'boolean
  :group 'jirassic)

(define-error 'jirassic-client-error "An HTTP client error occurred")

(cl-defstruct jirassic-client-response
  "A Jira client response."
  (status nil
          :read-only t
          :type integer
          :documentation "HTTP status code.")
  (headers nil
           :read-only t
           :type alist
           :documentation "Response headers.")
  (body nil
        :read-only t
        :type string
        :documentation "Response body."))

(cl-defstruct jirassic-client-error
  "A Jira client error."
  (message nil
           :read-only t
           :type string
           :documentation "Error message.")
  (code nil
        :read-only t
        :type integer
        :documentation "HTTP status code.")
  (response nil
            :read-only t
            :type jirassic-client-response
            :documentation "Response body."))

(defun jirassic--host ()
  "Return the Jira host URL."
  (or jirassic-host
      (error "Jira host not set. Use `jirassic-set-host' to set it.")))

(defun jirassic--base-url ()
  "Return the base URL for the Jira API."
  (s-concat (s-chop-suffix "/" (jirassic--host)) "/rest/api/3/"))

(defun jirassic--credentials ()
  "Return the credentials for the Jira host."
  (or (car (auth-source-search
            :host (url-host (url-generic-parse-url jirassic-host))
            :require '(:user :secret)
            :max 1))
      (error "No credentials found for host: %s" jirassic-host)))

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

(defun jirassic--default-error-handler (err)
  "Default error handler for Jira API errors."
  (error "Jira API error: %s" (jirassic-client-error-message err)))

(cl-defun jirassic--parse-client-error (error-thrown response)
  "Parse a client error from the Jira API response.

ERROR-THROWN is the error thrown by the request. RESPONSE is the
`request-response' object."
  (if response
      (let* ((response (jirassic--parse-client-response response))
             (status (jirassic-client-response-status response))
             (message
              (cond
               ((and (>= status 400)
                     (< status 500)
                     response)
                (let* ((headers (jirassic-client-response-headers response))
                       (body (jirassic-client-response-body response))
                       (message (s-join " " (alist-get 'errorMessages body))))
                  (or message
                      "An unknown error occurred.")))

               (t (if (stringp error-thrown)
                      error-thrown
                    (format "%s" error-thrown))))))
        (make-jirassic-client-error
         :message message
         :code status
         :response response))

    (make-jirassic-client-error
     :message (format "An error occurred: %s" error-thrown)
     :code nil
     :response nil)))

(cl-defun jirassic--parse-client-response (response)
  "Parse a client response from the Jira API response."
  (make-jirassic-client-response
   :status (request-response-status-code response)
   :headers (request-response-headers response)
   :body (request-response-data response)))

(cl-defmacro jirassic--error-callback (callback-function)
  `(cl-function (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                  (funcall ,callback-function
                           (jirassic--parse-client-error
                            error-thrown
                            response)))))

(cl-defmacro jirassic--then-callback (callback-function)
  (declare (indent 2))
  `(cl-function (lambda (&key data &allow-other-keys)
                  (funcall ,callback-function
                           data))))

(cl-defun jirassic--get (segments &key params)
  "Make a GET request to the Jira API with SEGMENTS and optional PARAMS.

SEGMENTS is a list of URL segments to append to the base URL. PARAMS is
an alist of query parameters to include in the request."
  (let* ((segments (or (and (listp segments)
                            segments)
                       (list segments)))
         (resource (s-join "/" segments))
         (url (s-concat (s-chop-suffix "/" (jirassic--base-url)) "/" resource))
         (credentials (jirassic--credentials))
         (headers (jirassic--http-headers credentials))
         (promise (aio-promise)))

    (request url
      :params params
      :parser #'json-read
      :headers headers
      :success
      (cl-function (lambda (&key data &allow-other-keys)
                     (aio-resolve promise (lambda () data))))
      :error
      (lambda (err)
        (cl-function
         (lambda (&key data error-thrown symbol-status response &allow-other-keys)
           (aio-resolve promise
                        (lambda (err)
                          (signal 'jirassic-client-error
                                  (jirassic--parse-client-error
                                   error-thrown
                                   response))))))))

    promise))

(cl-defun jirassic-get-issue (key)
  "Get a Jira issue by KEY."
  (jirassic--get (list "issue" key)))

(cl-defun jirassic-download-attachment (id to &key then else)
  "Download a Jira attachment by ID to a local file TO."
  (if (and (file-exists-p to)
           (not jirassic-overwrite-attachments))
      (message "Jira attachment %s exists, skipping" to)
    (let* ((resource (s-join "/" (list "attachment" "content" id)))
           (url (s-concat (s-chop-suffix "/" (jirassic--base-url)) "/" resource))
           (credentials (jirassic--credentials))
           (headers (jirassic--http-headers credentials)))

      (request url
        :encoding 'binary
        :parser #'buffer-string
        :headers headers
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (with-temp-file to
                                  (insert data))
                                (funcall then to)))
        :error
        (jirassic--error-callback (or else
                                      #'jirassic--default-error-handler))))))

(provide 'jirassic-client)
;;; jirassic-client.el ends here
