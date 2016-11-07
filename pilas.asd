(in-package #:asdf-user)

(defsystem "pilas"
  :description "The bare bones wiki"
  :license "APLGv3+"
  :depends-on (#:alexandria
               #:closer-mop
               #:ironclad
               #:hunchentoot
               #:trivia
               #:trivia.ppcre
               #:spinneret)
  :components ((:file "package")
               (:file "pilas")
               (:file "server")
               (:file "templates")
               (:File "controllers" :depends-on ("templates" "package"))))
