(in-package #:alice.grimoire)

(defparameter *url-shortening-regexp* "(http.*)")

(defun shorten-url (url)
  "Shorten `URL' using an external service (currently, tinyurl.com). Returns shortened URL or a failure message."
  (if url
      (or (ignore-errors (drakma::http-request "http://tinyurl.com/api-create.php"
                                               :external-format-out :UTF-8
                                               :parameters `(("url" . ,url))))
          :failed-in-shortening)
      :nothing-to-shorten))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))
