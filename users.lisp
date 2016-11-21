(in-package "PILAS")


(defvar +users-file+ (asdf/system:system-relative-pathname :pilas #P"users")
  "The file where users credentials are stored in the form email:password-hash,
  similar to /etc/passwd.")

(defclass user ()
  ((email :initarg :email :reader user-email)))

(defmethod print-object ((obj user) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (user-email obj))))

(defun log-user (email password)
  "Return the user registered with EMAIL if the PASSWORD-HASH validates, NIL
  otherwise."
  (with-open-file (in +users-file+)
    (loop :for line := (read-line in nil 'eof)
          :until (eq line 'eof)
          :do
             (let* ((password (babel:string-to-octets password))
                    (end-of-email-marker (position #\: line :test 'char=))
                    (email@line (subseq line 0 end-of-email-marker))
                    (password-hash@line (subseq line (1+ end-of-email-marker))))
               (when (and (string-equal email email@line)
                          (crypto:pbkdf2-check-password password password-hash@line))
                 (return (make-instance 'user :email email@line)))))))

(defun hash-password (password)
  (crypto:pbkdf2-hash-password-to-combined-string (babel:string-to-octets password)))

(defun add-user (user-email password)
  "Add the USER to the +USERS-FILE+"
  (let* ((password-hash (hash-password password))
         (user-entry (format nil "~A:~A" user-email password-hash)))
    (with-open-file (out +users-file+ :direction :output :if-exists :append)
      (write-line user-entry out))))

(defun list-users ()
  "Return a list of the emails of all the registered users."
  (with-open-file (in +users-file+)
    (loop :for line := (read-line in nil 'eof)
          :until (eq line 'eof)
          :collect (subseq line 0 (position #\: line :test 'char=)))))
