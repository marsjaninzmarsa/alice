(in-package #:alice.grimoire)

(defparameter *pushover-token* "")
(defparameter *pushover-admin-user* "")

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
