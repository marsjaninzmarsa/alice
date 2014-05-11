(in-package #:alice.grimoire)

(defvar *uptime-global* (local-time:now))
(defvar *uptime-event-handler* (local-time:now))

;; FIXME this way of generating text is broken and not compatibile with future plans.
;; We need to be able to do interpolated texts, I guess.
(defun reset-event-handler-uptime ()
  (setf *uptime-event-handler* (local-time:now)))

(defun uptime-message ()
  (concatenate 'string "Żyję od " (alice.language:format-date *uptime-global*) " " (alice.language:format-time *uptime-global*) "; nie śpię od " (alice.language:format-date *uptime-event-handler*) " " (alice.language:format-time *uptime-event-handler*) "."))
