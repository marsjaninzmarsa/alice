(in-package #:alice)

(defparameter *muted* nil)


(defparameter *max-output-sequence-length* 4)

(defparameter *full-name* "Alice Margatroid")

(defparameter *excluded-from-replying-to* '("kdbot") "List of users that the bot won't reply to for unrecognized queries.")

;;; utils
;; typical of sentence-features utils;
(defun mentions (what string)
  (search (string-downcase what) (string-downcase string)))

(defun mentions-regexp (regexp string)
  (alice.string-utils:matches-regexp-p regexp string))

(defun mentions-name (name string)
  (mentions name string))

;; types of message
;; these should go to sentence-features, or - as it should be named - message-features; but I guess we might keep the old name for the reason it sounds nicer,
(defun public-message-p (message)
  (and
   (not (string-equal *nick* (first (irc:arguments message)))) ; search message
   (not (starts-with-subseq *nick* (second (irc:arguments message)))))) ; search message target
       
(defun private-message-p (message)
  (or (string-equal (first (irc:arguments message))
                    *nick*)))

(defun directed-message-p (message)
  (or (string-equal (first (irc:arguments message))
                    *nick*)
      (mentions-name *nick* (second (irc:arguments message)))))

;; entry point
(defun start-alice (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  (alice.world-model:clear-nonpersistent-worldstate)
  (alice.world-model:load-persistent-world-model-data)

  (alice.irc:start-irc-connection ...)

  (mapcar (lambda (channel) (alice.world-model:join-channel channel)) channels) ;; <-- ??

  ;; TODO add those hooks through alice.irc?
  (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
  (irc:add-hook *connection* 'irc:irc-join-message 'join-hook)
  (irc:add-hook *connection* 'irc:irc-part-message 'part-hook)
  (irc:add-hook *connection* 'irc:irc-rpl_namreply-message 'names-hook)
  (irc:add-hook *connection* 'irc:irc-nick-message 'nick-hook))

(defun stop-alice (&optional (msg "Goodbye!"))
  (irc:stop-irc-connection msg))

;; impersonate function
(defun impersonate-slap (channel user)
  (irc::action alice::*connection* channel (concatenate 'string "slaps " user " with a Shanghai doll.")))

