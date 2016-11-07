(in-package "PILAS")

(defmacro with-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body (navigation-bar)
       ,@body))))

(deftag navigation-bar (control attrs)
  (declare (ignore control attrs))
  '(:ul
    (:li (:a :href "/create/" "Create new article"))
    (:li (:a :href "/random/" "Random article"))))
