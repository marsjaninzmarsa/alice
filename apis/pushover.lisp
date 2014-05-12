(in-package #:alice.api)

(defparameter *pushover-token* "")
(defparameter *pushover-admin-user* "")

(defun enable-pushover-api (token admin-user)
  (setf *pushover-token* token
        *pushover-admin-user* admin-user))

(defun send-pushover-notification (what to-token from)
  "Use Pushover API to send a message from `FROM' with contents `WHAT' to a Pushover user/group key `TO-TOKEN'."
  (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,to-token)
                                                        ("title" . ,alice:*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4)))
