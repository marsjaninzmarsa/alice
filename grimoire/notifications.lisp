(in-package #:alice.grimoire)

(defun make-pushover-notifier (pushover-key)
  "Returns function that will invoke `SEND-PUSHOVER-NOTIFICATION' with `PUSHOVER-KEY' as the destination."
  (lambda (channel who what from-who is-private)
    (declare (ignore channel who is-private))
    (send-pushover-notification what pushover-key from-who)))

(defun make-email-notifier (email)
  "Returns function that will invoke `SEND-EMAIL' with `EMAIL' as the destination."
  (lambda (channel who what from-who is-private)
    (declare (ignore channel who from-who is-private))
    (send-email email what)))

;; GENERAL NOTIFICATIONS
(defun notify-person (channel target-user message-body from-who is-private)
  "Send a message from `FROM-WHO' to `TARGET-USER' on `CHANNEL' containing `MESSAGE-BODY' using the best method available for particular `TARGET-USER'."
  (funcall (pick-notifier channel target-user message-body from-who is-private)
           channel target-user message-body from-who is-private))

(defun pick-notifier (channel target-user message-body from-who is-private)
  "Select notification method for given user using provided context."
  (gethash (alice.world-model:identify-person-canonical-name target-user) *user-notification-medium* #'notify-via-memo))

