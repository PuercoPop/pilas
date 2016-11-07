(in-package "PILAS")

(deftag navigation-bar (control attrs)
  (declare (ignore control attrs))
  '(:ul
    (:li (:a :href "/" "Front Page"))
    (:li (:a :href "/create/" "Create new article"))
    (:li (:a :href "/random/" "Random article"))))

(defmacro with-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body (navigation-bar)
       ,@body))))

(defun show-entry (entry)
  (with-page (:title (title entry))
      (:p (content entry))))
