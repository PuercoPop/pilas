(in-package #:asdf-user)

(defsystem "pilas"
  :description "The bare bones wiki"
  :license "APLGv3+"
  :depends-on (#:alexandria
               #:hunchentoot
               #:trivia
               #:trivia.ppcre
               #:spinneret)
  :serial t
  :components ((:file "package")
               (:file "pilas")
               (:file "server")
               (:File "controllers")
               (:file "templates")))
