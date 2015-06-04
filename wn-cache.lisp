(in-package :cl-wnbrowser)

(defparameter *suggestions* (make-hash-table :test #'equal))
(defparameter *wn*  (make-hash-table :test #'equal))

(defun cache-dbs ()
  (cache-wn)
  (cache-suggestions))

(defun cache-wn ()
  (dolist (s (execute-search "*" nil
                             "search-documents" "0" "2000000" nil nil))
    (let ((id (getf s :|doc_id|)))
      (setf (gethash id *wn*) s))))

(defun cache-suggestions ()
  (dolist (s (execute-search "*"
                             (make-drilldown-activity :status '("new")
                                                      :action '("add-gloss-pt"))
                             "search-activities" "0" "2000000" nil nil))
    (let ((id (getf s :|doc_id|)))
      (setf
       (gethash id *suggestions*)
       (push s (gethash id *suggestions*))))))

(defun get-cached-suggestions (id)
  (gethash id *suggestions*))

(defun get-cached-document (id)
  (gethash id *wn*))
