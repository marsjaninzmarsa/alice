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
           #:*default-phrase*
           #:*answers*
           #:*excluded-from-replying-to*
           #:*throttled-output*

           #:start-alice
           #:stop-alice
           #:impersonate-say
           #:impersonate-join
           #:impersonate-part
           #:impersonate-slap
           #:mute
           #:unmute
           #:say
           #:mentions
           #:mentions-regexp))

(defpackage #:alice.persistence-utils
  (:use #:cl)
  (:export #:dump-hashtable
           #:read-back-into-hashtable))

(defpackage #:alice.language
  (:use #:cl
        #:alexandria)
  (:export #:extract-words
           #:stem-matches-p
           #:format-date
           #:format-time))

(defpackage #:alice.grimoire
  (:use #:cl)
  (:export #:*pushover-token*
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
