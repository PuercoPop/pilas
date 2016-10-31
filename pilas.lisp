(defpackage "PILAS"
  (:use #:cl
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

(defparameter +wiki-root+ (merge-pathnames #P"wiki/" (directory-namestring *load-truename*))
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

(defun random-elt (list)
  (elt list (random (length list))))

(defun random-entry ()
  "Return a random entry."
  (random-elt (list-entries)))


;; Routes
(defmacro with-page ((&key title) &body body)
   `(with-html
      (:doctype)
      (:html
        (:head
         (:title ,title))
        (:body ,@body))))

(defun index ()
  "A list of links to every artile"
  (with-page (:title "Index")
    (:ul
     (loop :for entry :in (list-entries)
           :collect(:li (title entry))))))


;; Fixtures
(make-instance 'entry :title "José Olaya"
                      :content "Idolo")

(make-instance 'entry :title "José Olaya"
                      :content "Idolo maximo del futbol peruano. Conocido por sus sixpacks")
