(in-package "PILAS")

(define-easy-handler (global-css :uri (url-for-css 'global)) ()
  (setf (content-type*) "text/css")
  (css-lite:css
    (("nav")
     (:background-color "antiquewhite")
     (("nav ul li")
      (:display "inline")))))

(define-easy-handler (entry-css :uri (url-for-css 'entry)) ()
  (setf (content-type*) "text/css")
  (css-lite:css
    (("article")
     (:width "70%"))
    (("aside")
     (:width "20%"
      :display "inline"
      :float "left"
      :border-right-style "solid"
      :margin-right "10px"))))
