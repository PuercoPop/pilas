(in-package "PILAS/WEB")

(html-forms:define-form entry
  ((title :text :label "Título:" :validator (clavier:not-blank))
    (content :textarea :label "Contenido:")))

(defun entry->form (entry)
  (make-entry-form :title (title entry) :content (content entry)))

(defun form->entry (form)
  (assert (validate form) nil "Form data invalid.")
  (let* ((form-data (form-data form))
         (entry-title (cdr (assoc 'title form-data)))
         (entry-content (cdr (assoc 'content form-data))))
    (if-let (entry (find-entry-by-title entry-title))
      (progn (setf (slot-value entry 'title) entry-title
                   (slot-value entry 'content) entry-content)
             entry)
      (make-entry entry-title entry-content))))

(define-easy-handler (validate-entry-endpoint :uri "/validate-entry/") (title content)
  (let ((form (make-entry-form :title title :content content)))
    (multiple-value-bind (validp errors)
        (validate form)
      (declare (ignore errors))
      (if validp
          (let ((entry (form->entry form)))
            (save-entry entry)
            (redirect (url-for-entry entry)))
          (with-output-to-string (out)
            (show form out))))))
