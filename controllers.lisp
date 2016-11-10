;; Routes & controllers
(in-package "PILAS")


;; Helpers
(defun clear-routes ()
  (setf hunchentoot:*dispatch-table*
        (last hunchentoot:*dispatch-table*)))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) &body body)
  (multiple-value-bind (body declarations documentation)
      (parse-body body :documentation t)
    `(progn
       (defun ,name ()
         ,@(when documentation
             (list documentation))
         ,@declarations
         (match (script-name *request*)
           ((ppcre ,url-regexp ,@capture-names)
            ,@body)))
       (push (create-regex-dispatcher ,url-regexp ',name)
             *dispatch-table*))))


;; Handlers
(hunchentoot:define-easy-handler (index :uri "/") ()
  "A list of links to every article"
  (with-page (:title "Index"
              :css (global))
    (:ul
     (loop :for entry :in (list-entries)
           :collect (:li (:a :href (url-for-entry entry) (title entry)))))))

(define-easy-handler (display-random-entry :uri "/random/") ()
  (redirect (url-for-entry (random-entry))))

(hunchentoot:define-easy-handler (create-entry :uri "/create/") ()
  (with-page (:title "new-article")
    ;; XXX: Abstract to a validation layer
    (:form :action "/validate-entry/" :method "post"
           (:ul
            (:li (:label "Título:" ))
            (:li (:input :type "text" :name "title" :required t))
            (:li (:label "Content:"))
            (:li (:textarea :name "content")))
           (:input :type "submit"))))

(define-easy-handler (validate-entry-endpoint :uri "/validate-entry/") (title content)
  (let ((entry (make-entry title content)))
    ;; TODO: Add validation
    (save-entry entry)
    (redirect (url-for-entry entry))))
(define-regexp-route entry-deletion ("^/entry/delete/(.*)$" entry-title)
  (when-let ((entry (find-entry-by-title entry-title)))
    (delete-entry entry))
  (redirect "/"))

(define-regexp-route display-entry ("^/entry/(.*)$" entry-title)
  "Display the contents of the ENTRY."
  (when-let ((entry (find-entry-by-title entry-title)))
    (show-entry entry)))
