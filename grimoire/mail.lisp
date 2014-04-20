(in-package #:alice.grimoire)

(defparameter *mailgun-domain* "")
(defparameter *mailgun-key* "")

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
