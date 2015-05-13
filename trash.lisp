(defun get-request-full-url ()
  (puri:parse-uri
  (format nil "~a://~a~a"
          (etypecase hunchentoot:*acceptor*
            (hunchentoot:ssl-acceptor "https")
            (hunchentoot:acceptor "http"))
          (hunchentoot:host) (hunchentoot:request-uri*))))

(defun get-request-callback-url ()
  (let ((url (get-request-full-url)))
    (setf (puri:uri-query)
          (setf (puri:uri-path url) "/callback")
          (format nil "forward-uri=~a" (url-encode url)))))
