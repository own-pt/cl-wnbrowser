(in-package :cl-wnbrowser)

(defun process-pairs (function list)
  "Process LIST and applies FUNCTION to every pair, accumulating the
results of FUNCTION via APPEND.  It is expected that FUNCTION returns
either a list or a cons.  Example:

CL-USER> (process-pairs #'cons '(1 2 3 4))
((1 . 2) (3 . 4))"
  (append
   (destructuring-bind (a b . rest) list
     (cons (funcall function a b)
	   (when rest (process-pairs function rest))))))
