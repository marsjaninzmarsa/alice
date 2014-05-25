(in-package #:alice.irc)

(defvar *connection*)

(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")

(defparameter *autojoin-channels* '())

(define-constant +nickserv+ "NickServ" :test #'string=)
(define-constant +nickserv-identify-msg-template+ "IDENTIFY ~a" :test #'string=)

(defun start-irc-connection (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  
  (setf *nick* nick)
  (setf *connection* (irc:connect :nickname *nick*
                                  :server server))

  (irc:privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ password))

  (mapcar (lambda (channel) (join-channel channel)) channels)

  (add-hooks))

(defun stop-irc-connection (&optional (msg "Goodbye!"))
      (irc:quit *connection* msg))

(defun privmsg (to-where what &key to)
  (if to
      (irc:privmsg *connection* to-where (concatenate 'string to ": " what))
      (irc:privmsg *connection* to-where what)))

(defun action (to-where what)
  (irc::action *connection* to-where what))

(defun notice (to-where what)
  (irc:notice *connection* to-where what))

(defun join-channel (channel &key password)
  (irc:join *connection channel :password password))

(defun part-channel (channel)
  (irc:part *connection* channel))



(defun add-hooks () 
  (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
  (irc:add-hook *connection* 'irc:irc-join-message 'join-hook)
  (irc:add-hook *connection* 'irc:irc-part-message 'part-hook)
  (irc:add-hook *connection* 'irc:irc-rpl_namreply-message 'names-hook)
  (irc:add-hook *connection* 'irc:irc-nick-message 'nick-hook))

(defun start-message-handler ()
  (alice.grimoire:reset-event-handler-uptime)
  #+(or sbcl
        openmcl)
  (irc:start-background-message-handler *connection*))
