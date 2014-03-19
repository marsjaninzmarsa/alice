;;;; Alice's Grimoire, the source of her more powerful magic.
(defpackage #:alice.grimoire
  (:use #:cl
        #:alice.globals)
  (:export #:do-google-search
           #:shorten-url
           #:do-wolfram-computation
           #:send-pushover-notification
           #:send-email
           #:check-for-memos
           #:notify-via-memo            ;?? should this be exported?
           #:make-pushover-notifier
           #:make-email-notifier
           #:notify-person))
       
(in-package #:alice.grimoire)

;; exported functions

(defun do-google-search (query)
  "Run a Google search. Not yet implemented."
  (declare (ignore query))
  nil)

(defun shorten-url (url)
  "Shorten `URL' using an external service (currently, tinyurl.com). Returns shortened URL or a failure message."
  (if url
      (or (ignore-errors (drakma::http-request "http://tinyurl.com/api-create.php"
                                               :external-format-out :UTF-8
                                               :parameters `(("url" . ,url))))
          :failed-in-shortening)
      :nothing-to-shorten))

(defun do-wolfram-computation (query)
  "Query Wolfram|Alpha API for `QUERY' and format response as string."
  (flet ((xml-response-to-speechstrings (xml)
           (coerce (alexandria:flatten (map 'list
                                            (lambda (el)
                                              (let ((val (dom:first-child el)))
                                                (when val
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

(defun send-pushover-notification (what to-token from)
  "Use Pushover API to send a message from `FROM' with contents `WHAT' to a Pushover user/group key `TO-TOKEN'."
  (if (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,to-token)
                                                        ("title" . ,*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4))
      :notification-sent
      :failed-in-sending-notification))

;; private / implementation-details functions

;; TODO move this function elsewhere? e.g. sentence-features?
(defun extract-urls-from-message (message-body)
  (remove nil (mapcar (lambda (str)(cl-ppcre::scan-to-strings *url-regexp* str))
                      (split-sequence:split-sequence #\Space message-body))))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))


(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))

(defun send-email (where-to text)
  "Use an e-mail API (currently Mailgun) to send an e-mail containing `TEXT' to `WHERE-TO' address."
  (if (ignore-errors (drakma:http-request (concatenate 'string "https://api.mailgun.net/v2/" *mailgun-domain* "/messages")
                                          :method :post
                                          :basic-authorization `("api" ,*mailgun-key*)
                                          :parameters `(("from" . ,(concatenate 'string "Alice Margatroid <alice.margatroid@" *mailgun-domain* ">"))
                                                        ("to" . ,where-to)
                                                        ("subject" . "Alice Margatroid here; got a notification for you.")
                                                        ("text" . ,text))
                                          :external-format-out :UTF-8))
      ;; 
      :notification-sent
      :failed-in-sending-notification))


;; MEMOS
;; FIXME move this somewhere?
(defvar *memos* (make-hash-table :test 'equalp))

(defun make-memo (channel who what from-who)
  (let ((target (identify-person-canonical-name who)))
     (when target (list channel (identify-person-canonical-name who) what from-who))))

(defun memo-to-string (memo)
  (format nil "~A ma dla Ciebie wiadomość: ~A" (fourth memo) (third memo)))

(defun save-memo (memo)
  "Save a memo for user."
  (let ((memos (gethash (second memo) *memos*)))
    (setf (gethash (second memo) *memos*)
          (cons memo memos))))

(defun find-matching-memos (user destination memos)
  (remove-if-not (lambda (memo)
                   (and (or (equalp destination (first memo))
                            (null (first memo)))
                        (equalp user (second memo))))
                 memos))

(defun remove-memo (memo memos)
  (remove-if (lambda (m)
               (and (or (equalp (first memo) (first m))
                        (null (first m)))
                    (equalp (second memo) (second m))))
             memos
             :count 1))

(defun check-for-memos (destination for-who)
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos and if so, grab the first one and write it to him/her."
  (let* ((who (alice.world-model:identify-person-canonical-name for-who))
         (all-memos (gethash who *memos*))
         (matching-memos (find-matching-memos for-who destination all-memos))
         (memo (first matching-memos)))
    (when memo
      (progn (setf (gethash who *memos*) (remove-memo memo all-memos))
             (say destination (memo-to-string memo) :to for-who)
             (if (> (length matching-memos) 1)
                 (say destination :more-memos :to for-who))))))


(defun notify-via-memo (channel who what from-who is-global)
  "Create a memo for `WHO' on `CHANNEL' containing the text `WHAT' from person `FROM-WHO'. `IS-GLOBAL' is irrelevant and can be ignored; it will be removed (or reworked) in the future."
  (let ((memo (make-memo (and is-global channel)
                               who what from-who)))
    (if memo
        (progn (save-memo memo)
               :memo-saved)
        :memo-failed)))

(defun make-pushover-notifier (pushover-key)
  "Returns function that will invoke `SEND-PUSHOVER-NOTIFICATION' with `PUSHOVER-KEY' as the destination."
  (lambda (channel who what from-who is-global)
    (declare (ignore channel who is-global))
    (send-pushover-notification what pushover-key from-who)))

(defun make-email-notifier (email)
  "Returns function that will invoke `SEND-EMAIL' with `EMAIL' as the destination."
  (lambda (channel who what from-who is-global)
    (declare (ignore channel who from-who is-global))
    (send-email email what)))

;; GENERAL NOTIFICATIONS

(defun notify-person (channel target-user message-body from-who is-global)
  "Send a message from `FROM-WHO' to `TARGET-USER' on `CHANNEL' containing `MESSAGE-BODY' using the best method available for particular `TARGET-USER'.
Parameter `IS-GLOBAL' can be safely ignored, as it will be removed or reworked in the future."
  (funcall (pick-notifier channel target-user message-body from-who is-global)
           channel target-user message-body from-who is-global))

(defun pick-notifier (channel target-user message-body from-who is-global)
  "Select notification method for given user using provided context."
  (gethash (alice.world-model:identify-person-canonical-name target-user) *user-notification-medium* #'notify-via-memo))
