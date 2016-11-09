(in-package "PILAS")

(deftag navigation-bar (control attrs)
  (declare (ignore control attrs))
  '(:ul
    (:li (:a :href "/" "Front Page"))
    (:li (:a :href "/create/" "Create new article"))
    (:li (:a :href "/random/" "Random article"))))

(defmacro with-page ((&key title (css nil css-supplied-p)) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       ,@(when css-supplied-p
          (loop :for css-filename :in css
                :collect `(:link :rel "stylesheet" :href ,(url-for-css css-filename)))))
      (:body (navigation-bar)
       ,@body))))

(defun show-entry (entry)
  (with-page (:title (title entry)
              :css (entry))
    (:aside
     (:li :class "actions"
          (:ul (:p (:a :href (url-for-entry-edition entry) "Editar")))))
    (:article
     (:h3 (title entry))
     (:p (content entry)))))
