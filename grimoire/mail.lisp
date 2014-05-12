(in-package #:alice.grimoire)

(defun send-email (where-to text)
  "Use an e-mail API (currently Mailgun) to send an e-mail containing `TEXT' to `WHERE-TO' address."
  (if (alice.apis:mailgun-send-email where-to what)
      :notification-sent
      :failed-in-sending-notification))
