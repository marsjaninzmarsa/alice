;; Package definition central.

(defpackage #:alice
  (:use #:cl
        #:alexandria)
  (:export #:*connection*
           #:*server*
           #:*nick*
           #:*password*
           #:*autojoin-channels*
           #:*muted*
           #:+nickserv+
           #:+nickserv-identify-msg-template+
           #:*max-output-sequence-length*
           #:*uptime-global*
           #:*uptime-message-handler*
           #:*full-name*
           #:*excluded-from-replying-to*
           #:*throttled-output*

           #:start-alice
           #:stop-alice
           #:mute
           #:unmute
           #:say
           #:mentions
           #:mentions-regexp))

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

  (:export #:server
           #:nick
           #:password
           #:autojoin-channels

           #:start-irc-connection
           #:stop-irc-connection
           #:say
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
   ;; TODO move from grimoire and list them here.

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
  (:export #:*pushover-token*           ;NOTE (TODO FIXME) this stuff (constants) goes (hidden) into #alice.apis.
           #:*pushover-admin-user*
           #:*wolfram-app-id*
           #:*mailgun-domain*
           #:*mailgun-key*
           #:*url-shortening-regexp*
           #:*wolfram-query-regexp*
           #:*user-notification-medium*

           #:do-google-search
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
           #:notify-person))

(defpackage #:alice.sentence-features
  (:use #:cl))

(defpackage #:alice.emotions
  (:use #:cl))

(defpackage #:alice.world-model
  (:use #:cl)
  (:export #:clear-nonpersistent-worldstate
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

(defpackage #:alice.debug
  (:use #:cl #:alice)
  (:nicknames #:aldbg)
  (:export #:get-background-handler-instance
           #:attach-standard-output-to-slime
           #:detach-standard-output-from-slime))

(defpackage #:alice.specials
  (:use #:cl)
  (:export #:handle-specials))
