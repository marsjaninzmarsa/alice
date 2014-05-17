;; Package definition central.

(defpackage #:alice
  (:use #:cl
        #:alexandria)
  (:export #:*full-name*

           #:start-alice
           #:stop-alice))

(defpackage #:alice.persistence-utils
  (:use #:cl)
  (:export #:dump-hashtable
           #:read-back-into-hashtable))

(defpackage #:alice.string-utils
  (:use #:cl)
  (:export #:escape-for-regexp
           #:matches-regexp-p
           #:extract-words))

(defpackage #:alice.irc
  (:use #:cl)

  (:export #:*server*
           #:*nick*
           #:*password*
           #:*autojoin-channels*

           #:start-irc-connection
           #:stop-irc-connection
           #:privmsg
           #:action
           #:join-channel
           #:part-channel))

(defpackage #:alice.event-handlers
  (:use #:cl)
  (:export #:irc-msg-hook
           #:irc-join-hook
           #:irc-part-hook
           #:irc-names-hook
           #:irc-nick-hook))

(defpackage #:alice.language
  (:use #:cl
        #:alexandria)
  (:export #:*default-phrase*

           #:assemble-utterance
           #:extract-words
           #:stem-matches-p
           #:format-date
           #:format-time))

(defpackage #:alice.apis
  (:use #:cl)
  (:export 

   ;; API functions
   #:mailgun-send-email
   #:send-pushover-notification
   #:query-wolfram-alpha

   ;; enabling functions (for configuration)
   #:enable-etherpad-api
   #:enable-facebook-api
   #:enable-google-calendar-api
   #:enable-google-search-api
   #:enable-mailgun-api
   #:enable-pushbullet-api
   #:enable-pushover-api
   #:enable-wolfram-alpha-api))

(defpackage #:alice.grimoire
  (:use #:cl)
  (:export #:do-google-search
           #:shorten-url
           #:do-wolfram-computation
           #:send-pushover-notification
           #:parse-message-for-url-shortening
           #:parse-message-for-wolfram-computation
           #:send-email
           #:check-for-memos
           #:notify-via-memo            ;?? should this be exported?
           #:make-pushover-notifier
           #:make-email-notifier
           #:notify-person
           #:uptime-message
           #:reset-event-handler-uptime
           #:handle-specials))

(defpackage #:alice.core
  (:use #:cl)
  (:export #:*max-output-sequence-length*
           #:mute
           #:unmute
           #:say
           
           #:clear-nonpersistent-worldstate
           #:load-persistent-world-model-data
           #:store-joining-name
           #:store-parting-name
           #:store-names
           #:join-channel
           #:part-channel
           #:register-nick-change
           #:known-nick
           #:learn-canonical-name
           #:remember-seen-nick
           #:identify-person-mentioned
           #:identify-person-canonical-name))

(defpackage #:alice.utils
  (:use #:cl #:alice)
  (:export #:get-background-handler-instance
           #:attach-standard-output-to-slime
           #:detach-standard-output-from-slime

           #:dump-hashtable
           #:read-back-into-hashtable
           
           #:escape-for-regexp
           #:matches-regexp-p
           #:extract-words))
