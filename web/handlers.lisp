;; Routes & controllers
(in-package "PILAS/WEB")


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
         (when *debug*
           (format *trace-output* "=== Calling ~A Handler ===" ',name))
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
  (with-page (:title "new-article"
              :css (global))
    (entry-form)))

(define-regexp-route display-entry ("^/entry/(.*?)/$" entry-title)
  "Display the contents of the ENTRY."
  (when-let ((entry (find-entry-by-title entry-title)))
    (show-entry entry)))

(define-regexp-route edit-entry ("^/entry/(.*)/edit/$" entry-title)
  (if-let (entry (find-entry-by-title entry-title))
    (show-entry-edit entry)
    ;; XXX: Allow the creation handler to take an optional name parameter to
    ;; pre-fill the title of the entry to be created
    (redirect "/create/")))

(define-easy-handler (validate-entry-endpoint :uri "/validate-entry/") (title content)
  ;; TODO: Add validation
  (let ((updated-entry (if-let (entry (find-entry-by-title title))
                         (progn (setf (slot-value entry 'title) title
                                      (slot-value entry 'content) content)
                                entry)
                         (make-entry title content))))
    (save-entry updated-entry)
    (redirect (url-for-entry updated-entry))))

(define-regexp-route entry-deletion ("^/entry/(.*)/delete/$" entry-title)
  (when-let ((entry (find-entry-by-title entry-title)))
    (delete-entry entry))
  (redirect "/"))
