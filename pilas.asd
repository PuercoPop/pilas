(in-package #:asdf-user)

(defsystem "pilas"
  :description "The bare bones wiki"
  :license "APLGv3+"
  :depends-on (#:alexandria
               #:hunchentoot
               #:trivia
               #:trivia.ppcre
               #:spinneret)
  :components ((:file "pilas")))
