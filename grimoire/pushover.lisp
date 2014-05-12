(in-package #:alice.grimoire)

(defun send-pushover-notification (what to-token from)
  "Use Pushover API to send a message from `FROM' with contents `WHAT' to a Pushover user/group key `TO-TOKEN'."
  (if (alice.apis:send-pushover-notification what to-token from)
      :notification-sent
      :failed-in-sending-notification))
