;;;; alice.asd

(asdf:defsystem #:alice
  :serial t
  :description "Alice Margatroid, the Doll Maker of Bucuresti. An IRC-bot that pretends to be human."
  :author "Jacek 'TeMPOraL' Złydach"
  :license "Teaware - do whatever you want with it, but I wouldn't mind getting invited for a cup of tea ;)."
  :depends-on (#:cl-irc
               #:alexandria
               #:drakma
               #:cl-unicode
               #:cl-ppcre
               #:cxml
               #:local-time)
  :components ((:file "packages")
               (:module "mind"
                        :components ((:file "world-model")
                                     (:file "sentence-features")))
               (:module "language"
                        :components ((:file "language")))

               (:module "utils"
                        :components ((:file "persistence-utils")
                                     (:file "debug-utils")))
               (:module "grimoire"
                        :components ((:file "google")
                                     (:file "mail")
                                     (:file "memo")
                                     (:file "notifications" :depends-on ("memo" "mail" "pushover"))
                                     (:file "pushover")
                                     (:file "url-shortener")
                                     (:file "wolfram")
                                     (:file "specials")))

               (:file "main" :depends-on ("grimoire"))

               (:module "config"
                        :components ((:file "local-config")))))
