
;; original sense tagging handlers

(hunchentoot:define-easy-handler (sense-tagging-handler
				  :uri "/wn/sense-tagging") (userid)
  (let ((doc (get-sense-tagging)))
    (setf (hunchentoot:content-type*) "text/html")
    (cl-wnbrowser.templates:sense
     (list :userid userid :document doc))))

(hunchentoot:define-easy-handler (sense-tagging-frame-handler
				  :uri "/wn/sense-tagging-frame") (userid)
  (let ((doc (get-sense-tagging)))
    (setf (hunchentoot:content-type*) "text/html")
    (cl-wnbrowser.templates:sense-frame
       (list :userid userid))))

(hunchentoot:define-easy-handler (sense-tagging-details-handler
				  :uri "/wn/sense-tagging-details") (file text word userid)
  (let ((doc (get-sense-tagging-detail file text word))
        (sel (get-sense-tagging-suggestion file text word userid)))
    (setf (hunchentoot:content-type*) "text/html")
    (cl-wnbrowser.templates:sense-details
       (list :selection sel :file file :text text :word word :userid userid :document doc))))

(hunchentoot:define-easy-handler (sense-tagging-process-suggestion-handler
				  :uri "/wn/sense-tagging-process-suggestion") (file text word userid selection comment)
  (add-sense-tagging-suggestion file text word userid selection comment)
  (hunchentoot:redirect (format nil "/wn/sense-tagging-details?file=~a&text=~a&word=~a&userid=~a" file text word userid)))

