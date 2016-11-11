(in-package "PILAS")

(defun url-for-css (filename)
  (format nil "/css/~A.css" filename))

(defun url-for-entry (entry)
  (format nil "/entry/~A/" (url-encode (title entry))))

(defun url-for-entry-edition (entry)
  (format nil "/entry/~A/edit/" (url-encode (title entry))))

(defun url-for-entry-deletion (entry)
  (format nil "/entry/~A/delete/" (url-encode (title entry))))
