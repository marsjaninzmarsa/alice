;;;; Alice's Grimoire, the source of her more powerful magic.
(in-package #:alice)

(defun check-for-spelling-mistakes (message)
  (let* ((mistakes (find-if (lambda (test)
                              (not (null (cl-ppcre:all-matches (car test)
                                                               message))))
                            *spelling-tests*)))
    (if mistakes
        (cdr mistakes)
        nil)))

(defun do-google-search (query)
  (declare (ignore query))
  )

(defun shorten-url (url)
  (if url
      (or (ignore-errors (drakma::http-request "http://tinyurl.com/api-create.php"
                                               :external-format-out :UTF-8
                                               :parameters `(("url" . ,url))))
          :failed-in-shortening)
      :nothing-to-shorten))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))

(defun do-wolfram-computation (query)
  (flet ((xml-response-to-speechstrings (xml)
           (coerce (alexandria:flatten (map 'list
                                            (lambda (el)
                                              (let ((val (dom:first-child el)))
                                                (if val
                                                    (split-sequence:split-sequence #\Newline (dom:data val)))))
                                            (dom:get-elements-by-tag-name xml "plaintext")))
                   'vector))

         (get-xml-response (query)
           (let ((response (drakma:http-request "http://api.wolframalpha.com/v2/query"
                                                :external-format-out :UTF-8
                                                :parameters `(("appid" . ,*wolfram-app-id*)
                                                              ("input" . ,query)
                                                              ("format" . "plaintext")))))
             (cxml:parse-rod response
                             (cxml-dom:make-dom-builder))))
         (clean-up (response)
           (let ((cleaned-up (remove nil response)))
             (if (= (length cleaned-up) 0)
                 :nothing-computed
                 cleaned-up))))

    ;; code
    (if query
        (or (ignore-errors (clean-up (xml-response-to-speechstrings (get-xml-response query))))
            :failed-in-computing)
        :nothing-to-compute)))

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))

(defun send-notification (what &optional (from ""))
  (or (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,*pushover-user*)
                                                        ("title" . ,*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4))
      :failed-in-sending-notification))


(defun send-email (where-to text)
  (or (ignore-errors (drakma:http-request (concatenate 'string "https://api.mailgun.net/v2/" *mailgun-domain* "/messages")
                                          :method :post
                                          :basic-authorization `("api" ,*mailgun-key*)
                                          :parameters `(("from" . ,(concatenate 'string "Alice Margatroid <alice.margatroid@" *mailgun-domain* ">"))
                                                        ("to" . ,where-to)
                                                        ("subject" . "Alice Margatroid here; got a notification for you.")
                                                        ("text" . ,text))
                                          :external-format-out :UTF-8))
      :failed-in-sending-notification))


(defun output-feed-item (type details)
  (with-open-file (stream "/tmp/alice.rss"
                   :direction :output
                   :if-exists :supersede)
  (xml-emitter:with-rss2 (stream)
    (xml-emitter:rss-channel-header "Alice Margatroid" "http://temporal.pr0.pl/devblog")
    (xml-emitter:rss-item type
                          :description details
                          :pubdate (current-time-as-rss-string)))))

(defun current-time-as-rss-string ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (let ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
          (days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
      (format nil "~A, ~2,'0d ~2,'0d ~d ~2,'0d:~2,'0d:~2,'0d GMT~@d" (nth day-of-week days) date (nth month months) year hour minute second (- tz)))))
