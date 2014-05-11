(in-package #:alice.world-model)

(defparameter *muted* nil)


(defparameter *max-output-sequence-length* 4)

(defparameter *full-name* "Alice Margatroid")

(defparameter *excluded-from-replying-to* '("kdbot") "List of users that the bot won't reply to for unrecognized queries.")

(defvar *uptime-global* (local-time:now))
(defvar *uptime-message-handler* (local-time:now))

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

(defun uptime ()
  (concatenate 'string "Żyję od " (alice.language:format-date *uptime-global*) " " (alice.language:format-time *uptime-global*) "; nie śpię od " (alice.language:format-date *uptime-message-handler*) " " (alice.language:format-time *uptime-message-handler*) "."))

;;; handling

(defun msg-hook (message)
    (let ((destination (if (string-equal (first (irc:arguments message)) *nick*)
                         (irc:source message)
                         (first (irc:arguments message))))
          (is-private (private-message-p message))
          (is-public (public-message-p message))
          (is-directed (directed-message-p message))
          (from-who (irc:source message))
          (message-body (second (irc:arguments message))))

      (alice.grimoire:check-for-memos destination from-who)
      (alice.specials:handle-specials destination is-private is-public is-directed from-who message-body)

      (cond
        ((and is-directed
              (or (mentions "zawiadom" message-body)
                       (mentions "powiadom" message-body)
                       (mentions "przeka" message-body)
                       (mentions "pingnij" message-body)
                       (mentions "zapyta" message-body)
                       (mentions "spyta" message-body)
                       (mentions "memo" message-body)))
         (progn (say destination (alice.grimoire:notify-person destination
                                                               (alice.world-model:identify-person-mentioned message-body)
                                                               message-body
                                                               from-who
                                                               is-private))))

        ((or (mentions "kirisame" message-body)
             (mentions "marisa" message-body))
         (say destination :marisa))

        ;; introductions
        ((and is-directed
              (or (mentions "poznaj" message-body)
                  (mentions "przedstaw się" message-body)
                  (mentions "przedstaw sie" message-body)
                  (mentions "przedstawisz"  message-body)))

         (say destination :introduction))

        ;; version number
        ((and is-directed
              (or (mentions "numer wersji" message-body)
                  (mentions "wersje" message-body)
                  (mentions "wersja" message-body)
                  (mentions "wersją" message-body)
                  (mentions "wersję" message-body)))
         (say destination :version))

        ;; version number
        ((and is-directed
              (or (mentions "uptime" message-body)))
         (say destination (uptime)))


        ;; be nice to thanks
        ((and is-directed
              (or (mentions "thx" message-body)
                  (mentions "thanks" message-body)
                  (mentions "thank you" message-body)
                  (mentions "dzieki" message-body)
                  (mentions "dzięki" message-body)
                  (mentions "dziekuje" message-body)
                  (mentions "dziękuje" message-body)
                  (mentions "dziękuję" message-body)))
         (progn (say destination :thanks-reply)
                (if (or (mentions ":*" message-body)
                        (mentions "sło" message-body))
                    (say destination :blush))))

        ;; temp check
        ((and is-directed
              (or (mentions "temperatur" message-body)
                  (mentions "zimno" message-body)
                  (mentions "cieplo" message-body)
                  (mentions "ciepło" message-body)))
         (say destination :temperature))

        ;; anyone in HS?
        ((and is-directed
              (mentions "kto" message-body)
              (mentions "jest w HS" message-body))
         (say destination :who-in-hs))

        ;; sing
        ((and is-directed
              (or (mentions "spiew" message-body)
                  (mentions "śpiew" message-body)))
         (say destination :songs))

        ;; talking about
        ((and (or is-public
                  is-directed)
              (or (mentions "アリス・マーガトロイド" message-body)
                  (mentions "Arisu māgatoroido" message-body)
                  (mentions "Margatroid" message-body)))
         (say destination :mentioned-my-name))

        ;; TCP handshake for Bambucha
        ((and is-directed
              (mentions "SYN" message-body))
         (say destination :tcp :to from-who))

        ;; URL advanced shortener (not yet implemented)
        ((and is-directed
              (and (or (mentions "skró" message-body)
                       (mentions "skracaj" message-body))
                   (or (mentions "poprzedni" message-body)
                       (mentions "ostatni" message-body))))
         (say destination :not-yet-implemented))

        ;; URL shortener
        ((and is-directed
              (or (mentions "skró" message-body)
                  (mentions "skracaj" message-body)))
         (say destination (alice.grimoire:shorten-url (alice.grimoire:parse-message-for-url-shortening message-body))))

        ;; Wolfram|Alpha
        ((and is-directed
              (or (mentions "licz" message-body)
                  (mentions "compute" message-body)))
         ;; (say destination :wolfram-turned-off))
         (say destination (alice.grimoire:do-wolfram-computation (alice.grimoire:parse-message-for-wolfram-computation message-body))))

        ;; continue throttled output
        ((and is-directed
              (or (mentions "tak" message-body)
                  (mentions "yes" message-body)
                  (mentions "dawaj" message-body)
                  (mentions "pros" message-body))
              (not (null *throttled-output*)))
         (say destination *throttled-output*))

        ;; save
        ((and is-directed
              (or (mentions "pisz" message-body)
                  (mentions "notuj" message-body)))
         (say destination :save))

        ;; say hi!
        ((and is-directed
              (or (mentions "czesc" message-body)
                  (mentions "cześć" message-body)
                  (mentions "hi" message-body)
                  (mentions "hej" message-body)
                  (mentions "hey" message-body)
                  (mentions "yo" message-body)
                  (mentions "joł" message-body)
                  (mentions "hello" message-body)))
         (say destination :hello :to from-who))

        ;; kdbot is a doll
        ((and is-directed
              (mentions "kdbot" message-body))
         (say destination :kdbot))

        ((and is-directed
              (mentions "cycki" message-body))
         (say destination :notitsforyou :to from-who))

        ((and is-public
              (mentions "!votekick" message-body))
         (say destination "y"))

        ((and is-public
              (or (mentions-regexp "^dobranoc$" message-body)
                  (and (mentions "spadam" message-body)
                       (mentions "spać" message-body))))
         (say destination :goodnight :to from-who))

        ((and (or is-public
                  is-directed)
              (equalp destination "#hackerspace-krk") 
              (or (mentions "robi sens" message-body)
                  (mentions "robią sens" message-body)
                  (mentions "robić sens" message-body)))
         (when (= 0 (random 3))
           (say destination :point-out-making-sense)))

        ;; is this an accident?
        ((and (or is-public
                  is-directed)
              (mentions "przypadek?" message-body))
         (say destination "nie sądzę."))

        ((and (or is-public
                 is-directed)
              (or
               (mentions "yolo" message-body)
               (mentions "jolo" message-body)))
         (if (= 0 (random 3))
             (say destination :yolo :to from-who)))

        ;; temporary control for remembering names
        ((and is-private
              (mentions "zapamiętaj:" message-body))
         (let* ((names (alice.language:extract-words message-body))
                (alias (second names))
                (canonical (third names)))
           (if (and alias canonical)
               (progn
                 (alice.world-model:learn-canonical-name alias canonical)
                 (say destination (format nil "Zapamiętałam ~A jako ~A." alias canonical)))
               (say destination "You fail at wydawanie poleceń. *sigh*"))))

        ;; default responder
        (is-directed
         (if (and (/= 0 (random 5))
                  (not (position from-who *excluded-from-replying-to* :test #'equal)))
             (say destination :smiles :to from-who))))))

;; those hooks handle world state
(defun join-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (alice.world-model:store-joining-name where who)))

(defun part-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (alice.world-model:store-parting-name where who)))

(defun names-hook (message)
  (let ((channel (third (irc:arguments message)))
        (nicks (fourth (irc:arguments message))))
    (alice.world-model:store-names channel (split-sequence:split-sequence #\Space nicks))))

(defun nick-hook (message)
  (let ((from (irc:source message))
        (to (first (irc:arguments message))))
    (alice.world-model:register-nick-change from to)))

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

;; REPL-utils ?
;; I mean, really, c'mon.
(defun mute ()
  (setf *muted* t))

(defun unmute ()
  (setf *muted* nil))

;; impersonate function

(defun impersonate-slap (channel user)
  (irc::action alice::*connection* channel (concatenate 'string "slaps " user " with a Shanghai doll.")))

