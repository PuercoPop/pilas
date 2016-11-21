(in-package #:asdf-user)

(defsystem "pilas"
  :description "The bare bones wiki"
  :license "APLGv3+"
  :depends-on (#:alexandria
               #:babel
               #:closer-mop
               #:css-lite
               #:ironclad
               #:hunchentoot
               #:trivia
               #:trivia.ppcre
               #:spinneret)
  :components ((:file "package")
               (:file "users" :depends-on ("package"))
               (:file "pilas" :depends-on ("users" "package"))
               (:module "web"
                :components ((:file "server")
                             (:file "urls")
                             (:file "css" :depends-on ("urls"))
                             (:file "templates" :depends-on ("css" "urls"))
                             (:File "handlers" :depends-on ("templates" "urls")))
                :depends-on ("pilas" "package"))))
