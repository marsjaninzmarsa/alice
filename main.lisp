(in-package #:alice)

(defparameter *full-name* "Alice Margatroid")

;; entry point
(defun start-alice (&key (server alice.irc:*server*) (nick alice.irc:*nick*) (password alice.irc:*password*) (channels alice.irc:*autojoin-channels*))
  (alice.world-model:clear-nonpersistent-worldstate)
  (alice.world-model:load-persistent-world-model-data)

  (alice.irc:start-irc-connection :server server
                                  :nick nick
                                  :password password
                                  :channels channels)

(defun stop-alice (&optional (msg "Goodbye!"))
  (irc:stop-irc-connection msg))

