(ql:quickload :drakma)

(defun call-rest-method (url &key parameters)
    (let ((stream (drakma:http-request
                   url
                   :parameters parameters
		   :external-format-out :utf-8
                   :method :post
                   :content "ignored"
                   :connection-timeout 120
                   :want-stream t)))
      (read stream)))

;; sample synset request (returns a PLIST)
(let ((result (call-rest-method "http://localhost:8040/wn/synset" :parameters '(("id" . "03538300-n") ("sexp" . "yes")))))
  (print (getf result :SYNSET)))

;; sample search request
(let ((result (call-rest-method "http://localhost:8040/wn/search" :parameters '(("term" . "carro") ("sexp" . "yes")))))
  (print result))

  
