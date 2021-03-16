;;;; multi-test.asd

(asdf:defsystem #:multi-test
  :description "Describe multi-test here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-colors #:trivial-gamekit-ui #:usocket #:bordeaux-threads #:babel)
  :components ((:file "package")
               (:file "util")
               (:file "network")
               (:file "main")))
