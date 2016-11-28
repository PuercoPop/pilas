(in-package "PILAS/WEB")

(defun navigation-bar ()
  (with-html (:nav
              (:ul
               (:li (:a :href "/" "Front Page"))
               (:li (:a :href "/create/" "Create new article"))
               (:li (:a :href "/random/" "Random article"))))))

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
              :css (global entry))
    (:aside
     (:ul :class "actions"
          (:li (:a :href (url-for-entry-edition entry) "Editar"))
          (:li (:a :href (url-for-entry-deletion entry) "Borrar"))))
    (:article
     (:h3 (title entry))
     (:p (content entry)))))

(defun show-entry-edit (entry)
  (with-page (:title (title entry)
              :css (global entry))
    (:aside)
    (:article
     (show (make-entry-form :title (title entry)
                            :content (content entry))
              *html*))))
