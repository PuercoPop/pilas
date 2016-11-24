(in-package "PILAS/WEB")

(html-forms:define-form entry
  ((title :text :validator (clavier:not-blank))
    (content :text)))

(define-easy-handler (validate-entry-endpoint :uri "/validate-entry/") (title content)
  (let ((form (make-entry-form :title title :content content)))
    (multiple-value-bind (data errors)
        (validate form)
      (declare (ignore errors))
      (if data
          (let* ((entry-title (cdr (assoc 'title data)))
                 (entry-content (cdr (assoc 'content data)))
                 (updated-entry (if-let (entry (find-entry-by-title entry-title))
                                  (progn (setf (slot-value entry 'title) entry-title
                                               (slot-value entry 'content) entry-content)
                                         entry)
                                  (make-entry (cdr (assoc 'title data)) (cdr (assoc 'content data))))))
            (save-entry updated-entry)
            (redirect (url-for-entry updated-entry)))
          (with-output-to-string (out)
            (show form out))))))
