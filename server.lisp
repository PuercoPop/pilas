(in-package "PILAS")

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
