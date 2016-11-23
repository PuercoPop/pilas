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

;; Copied verbatim from Spinneret's README.md
(deftag input (default attrs &key name label (type "text"))
   (once-only (name)
     `(progn
        (:label :for ,name ,label)
        (:input :name ,name :id ,name :type ,type
          ,@attrs
          :value (progn ,@default)))))

(deftag entry-form (extra-fields attrs &key (entry nil entry-provided-p))
  (once-only (entry)
    `(:form :action "/validate-entry/" :method "post" ,@attrs
            (:ul
             (:li (input :type "text" :name "title" :label "Título:" :required t (progn ,(when entry-provided-p `(title ,entry)))))
             (:li (:label :for "content ""Content:"))
             (:li (:textarea :name "content" (progn ,(when entry-provided-p `(content ,entry))))))
            ,@extra-fields
            (:input :type "submit"))))

(defun show-entry-edit (entry)
  (with-page (:title (or title (title entry))
              :css (global entry))
    (:aside)
    (:article
     (entry-form :entry entry))))
