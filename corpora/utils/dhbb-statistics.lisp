(in-package :cl-user)

(ql:quickload :cl-fad)
(ql:quickload :yason)
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

;; (setf drakma:*header-stream* *standard-output*)

(defpackage :kb-statistics
  (:use :cl :alexandria))

(in-package :kb-statistics)

(defun get-property (key doc)
  (gethash key doc))

(defun process-file (in-file)
  (let ((parsed (yason:parse in-file)))
    (dolist (document parsed)
      (let ((analysis (get-property "analysis" document)))
	(dolist (s (flatten analysis))
	  (when (starts-with-subseq "V" (get-property "tag" s))
	    (format t "~a,~a~%" (get-property "lemma" s) 
		    (if (> (length (get-property "sense" s)) 0) (get-property "sense" s) "NIL"))))))))

(defparameter *dhbb-input-dir* #p"/backup/dhbb/")

(defun execute ()
  (cl-fad:walk-directory
   *dhbb-input-dir*
   (lambda (f) 
     (format t "~a~%" f)
     (handler-case 
	 (process-file f)
       (error (condition)
	 (format t "[error] ~a ~%" condition))))))

(execute)
