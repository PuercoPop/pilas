(defsystem "pilas"
  :description "The bare bones wiki"
  :license "AGPLv3+"
  :depends-on (#:alexandria
               #:babel
               #:closer-mop
               #:ironclad)
  :components ((:file "package")
               (:file "users" :depends-on ("package"))
               (:file "pilas" :depends-on ("users" "package"))))
