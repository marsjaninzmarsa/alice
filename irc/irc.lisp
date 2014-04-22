(in-package #:alice.irc)

;; TODO this module should handle ALL direct interactions with cl-irc, as well as server stuff, nickserv, etc.


(defvar *connection*)

(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")

(defparameter *autojoin-channels* '())

(define-constant +nickserv+ "NickServ" :test #'string=)
(define-constant +nickserv-identify-msg-template+ "IDENTIFY ~a" #'string=)

(defun start-irc-connection (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  (alice.world-model:clear-nonpersistent-worldstate)
  (alice.world-model:load-persistent-world-model-data)

  (setf *nick* nick)
  (setf *connection* (irc:connect :nickname *nick*
                                  :server server))

  (irc:privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ password))

  (mapcar (lambda (channel) (alice.world-model:join-channel channel)) channels) ;FIXME this function definitely goes out of 

  (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
  (irc:add-hook *connection* 'irc:irc-join-message 'join-hook)
  (irc:add-hook *connection* 'irc:irc-part-message 'part-hook)
  (irc:add-hook *connection* 'irc:irc-rpl_namreply-message 'names-hook)
  (irc:add-hook *connection* 'irc:irc-nick-message 'nick-hook)

  #+(or sbcl
        openmcl)
  (irc:start-background-message-handler *connection*))

(defun stop-irc-connection (&optional (msg "Goodbye!"))
      (irc:quit *connection* msg))
