(in-package #:alice)

(defparameter *full-name* "Alice Margatroid")

;; entry point
(defun start-alice (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  ;; FIXME move those somewhere. I don't think they're part of the
  ;; "world model"; they either stay here or move to another "central"
  ;; piece of Alice (one tha doesn't yet exist).

  ;; (alice.world-model:clear-nonpersistent-worldstate)
  ;; (alice.world-model:load-persistent-world-model-data)

  (alice.irc:start-irc-connection ...)  ;TODO

  (mapcar (lambda (channel) (alice.world-model:join-channel channel)) channels) ;; <-- ??

  ;; TODO add those hooks through alice.irc?
  (irc:add-hook *connection* 'irc:irc-privmsg-message 'alice.event-handlers.irc-msg-hook)
  (irc:add-hook *connection* 'irc:irc-join-message 'alice.event-handlers.irc-join-hook)
  (irc:add-hook *connection* 'irc:irc-part-message 'alice.event-handlers.irc-part-hook)
  (irc:add-hook *connection* 'irc:irc-rpl_namreply-message 'alice.event-handlers.irc-names-hook)
  (irc:add-hook *connection* 'irc:irc-nick-message 'alice.event-handlers.irc-nick-hook))

(defun stop-alice (&optional (msg "Goodbye!"))
  (irc:stop-irc-connection msg))

