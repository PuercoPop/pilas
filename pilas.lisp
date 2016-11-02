(defpackage "PILAS"
  (:use #:cl
        #:alexandria
        #:hunchentoot
        #:trivia
        #:trivia.ppcre
        #:spinneret)
  (:export
   #:+wiki-root+
   #:entry
   #:title
   #:content
   #:make-entry
   #:save-entry
   #:load-entry
   #:list-entries
   #:list-entry-titles))
(in-package "PILAS")

#|
Diseño:

Flat file database. nombre del archivo es el título de la entrada. {{ }} embeber lisp
|#


(defparameter +wiki-root+ (asdf/system:system-relative-pathname :pilas #P"wiki/")
  "The top level directory where the wiki database lives.")

(defclass entry ()
  ((title :initarg :title
          :reader title
          :documentation "The title of the entry")
   (content :initarg :content
            :reader content
            :documentation "The textual content of the entry in wiki-text.")))

(defun make-entry (title content)
  (make-instance 'entry :title title :content content))

(defun at-most (max-length string)
  (if (> (length string) (1+ max-length))
      (concatenate 'string (subseq string 0 (1- max-length)) "…")
      string))

(defmethod print-object ((obj entry) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "T: ~A. C: ~A" (title obj) (at-most 10 (content obj)))))

(defun pathname-from-title (title)
  "Return the pathname and entry should be stored if titled TITLE."
  (merge-pathnames title +wiki-root+))

(defun entry-pathname (entry)
  "Return the pathname where the entry is stored."
  (pathname-from-title (title entry)))

(defun save-entry (entry)
  (with-open-file (out (entry-pathname entry) :direction :output :external-format :utf-8)
    (write (content entry) :stream out)))

(defun load-entry (title)
  (with-open-file (in (pathname-from-title title) :direction :input :external-format :utf-8)
    (make-entry title (read in))))

(defun list-entry-titles ()
  "Return a list of the title of every entry in the wiki."
  (mapcar #'pathname-name (uiop/filesystem:directory-files +wiki-root+)))

(defun list-entries ()
  "Return a list of  every entry in the wiki."
  (mapcar #'load-entry (mapcar #'pathname-name (uiop/filesystem:directory-files +wiki-root+))))

(defun find-entry-by-title (title)
  (find title (list-entries) :test #'string-equal :key #'title))

(defun random-entry ()
  "Return a random entry."
  (random-elt (list-entries)))


;; Templates
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


;; Routes & controllers
(defun clear-routes ()
  (setf hunchentoot:*dispatch-table*
        (last hunchentoot:*dispatch-table*)))

(defun url-for-entry (entry)
  (format nil "/entry/~A" (title entry)))

(hunchentoot:define-easy-handler (index :uri "/") ()
  "A list of links to every article"
  (with-page (:title "Index")
    (:ul
     (loop :for entry :in (list-entries)
           :collect (:li (:a :href (url-for-entry entry) (title entry)))))))

(hunchentoot:define-easy-handler (create-entry :uri "/create/") ()
  (with-page (:title "new-article")
    ;; XXX: Abstract to a validation layer
    (:form
     (:label "Título:" (:input ))
     (:label "Content:" (:textarea))
     (:input :type "submit"))))

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

(define-regexp-route show-entry ("^/entry/(.*)$" entry-title)
  "Display the contents of the ENTRY."
  (when-let ((entry (find-entry-by-title entry-title)))
    (with-page (:title (title entry))
      (:p (content entry)))))




;; Server
(defvar *server* nil)

(defvar *http-host* "127.0.0.1")
(defvar *http-port* 8000)

(defun start-server ()
  (setf *server*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :address *http-host*
                                          :port *http-port*))))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)))

;; Fixtures
(make-instance 'entry :title "José Olaya"
                      :content "Idolo")

(make-instance 'entry :title "José Olaya"
                      :content "Idolo maximo del futbol peruano. Conocido por sus sixpacks")
