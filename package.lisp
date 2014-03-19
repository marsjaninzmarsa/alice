;;;; package.lisp

(defpackage #:alice
  (:use #:cl
        #:alice.globals)
  (:export #:start-alice
           #:stop-alice
           #:impersonate-say
           #:impersonate-join
           #:impersonate-part
           #:mute
           #:unmute))

