(in-package "PILAS")

#|
Design:

- Flat file database.
- Name of the file a digest of all the slot values, that is the content.
- The file contents are a plist with each slot as an entry with the key being
  the slot name and the value being the slot-value.

- The wiki markup is just plain text for the time being.
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

;; FIXME: Create an index so we can lookup in constant time
(defun pathname-from-title (title)
  "Return the pathname and entry should be stored if titled TITLE."
  (when-let (entry (find-entry-by-title title))
    (merge-pathnames (compute-digest-for-entry entry) +wiki-root+)))

(defun entry-pathname (entry)
  "Return the pathname where the entry is stored."
  (merge-pathnames (compute-digest-for-entry entry) +wiki-root+))

(defun slots-of (instance)
  "Return a list of slots of an instance."
  (let ((class (class-of instance)))
    (c2mop:ensure-finalized class)
    (c2mop:class-slots class)))

(defmacro do-slots ((varsym instance &optional ret) &body body)
  "Iterate over the slots of an instance"
  `(dolist (,varsym (slots-of ,instance) ,@(when ret (list ret)))
     ,@body))

(defun compute-digest-for-entry (entry)
  (let ((digest (crypto:make-digest :sha256)))
    (do-slots (slot entry) ;; Or do-slot-values?
      (crypto:digest-sequence digest (babel:string-to-octets (slot-value entry (c2mop:slot-definition-name slot)))))
    (crypto:byte-array-to-hex-string (crypto:produce-digest digest))))

(defun entry-plist (entry)
  "Return a plist with each slot of ENTRY"
  (let ((result ()))
    (do-slots (slot entry result)
      (push (slot-value entry (c2mop:slot-definition-name slot)) result)
      (push (c2mop:slot-definition-name slot) result))
    result))

(defun plist-entry (plist)
  "Take a plist, return an Entry."
  (let ((entry (make-instance 'entry)))
    (doplist (slot-name value plist entry)
      (setf (slot-value entry slot-name)
            value))))

(defun save-entry (entry)
  (let ((*print-readably* t))
    (with-open-file (out (entry-pathname entry) :direction :output :external-format :utf-8)
      (write (entry-plist entry) :stream out))))

(defun load-entry (digest)
  (with-open-file (in (merge-pathnames digest +wiki-root+) :direction :input :external-format :utf-8)
    (plist-entry (read in))))

(defun list-entry-titles ()
  "Return a list of the title of every entry in the wiki."
  (mapcar #'title
          (mapcar #'load-entry
                  (mapcar #'pathname-name (uiop/filesystem:directory-files +wiki-root+)))))

(defun list-entries ()
  "Return a list of  every entry in the wiki."
  (mapcar #'load-entry (mapcar #'pathname-name (uiop/filesystem:directory-files +wiki-root+))))

(defun find-entry-by-title (title)
  (find title (list-entries) :test #'string-equal :key #'title))

(defun random-entry ()
  "Return a random entry."
  (random-elt (list-entries)))


;; Fixtures
(make-instance 'entry :title "José Olaya"
                      :content "Idolo")

(make-instance 'entry :title "José Olaya"
                      :content "Idolo maximo del futbol peruano. Conocido por sus sixpacks")
