(in-package #:asdf-user)

(defsystem "pilas-web"
  :description "A web interface to the Pilas wiki"
  :license "AGPLv3+"
  :depends-on (#:css-lite
               #:html-forms
               #:hunchentoot
               #:pilas
               #:spinneret
               #:trivia
               #:trivia.ppcre)
  :components ((:module "web"
                :components ((:file "package")
                             (:file "server" :depends-on ("package"))
                             (:file "urls" :depends-on ("package"))
                             (:file "css" :depends-on ("urls" "package"))
                             (:file "templates" :depends-on ("css" "urls" "package"))
                             (:file "forms" :depends-on ("templates" "package"))
                             (:File "handlers" :depends-on ("templates" "urls" "package"))))))
