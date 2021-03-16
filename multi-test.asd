;;;; multi-test.asd

(asdf:defsystem #:multi-test
  :description "trivial-gamekit multiplayer experiment"
  :author "ava fox"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-colors #:trivial-gamekit-ui #:usocket #:bordeaux-threads #:babel)
  :components ((:file "package")
               (:file "util")
               (:file "network")
               (:file "main")))
