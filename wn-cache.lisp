(in-package :cl-wnbrowser)

(defparameter *suggestions* nil)
(defparameter *wn* nil)

(defun cache-dbs ()
  (setf *suggestions* (make-hash-table :test #'equal))
  (setf *wn* (make-hash-table :test #'equal))
  (cache-wn)
  (cache-suggestions))

(defun nominalization? (doc)
  (member "Nominalization" (getf doc :|rdf_type|) :test #'equal))

(defun cache-wn ()
  (dolist (s (execute-search "*" nil
                             "search-documents" "0" "2000000" nil nil))
    (let ((id (getf s :|doc_id|)))
      (when (not (nominalization? s))
        (setf (gethash id *wn*) s)))))

(defun cache-suggestions ()
  (dolist (s (execute-search "*"
                             (make-drilldown-activity :status '("new")
                                                      :action '("add-gloss-pt"))
                             "search-activities" "0" "2000000" nil nil))
    (let ((id (getf s :|doc_id|)))
      (setf
       (gethash id *suggestions*)
       (push s (gethash id *suggestions*))))))

(defun search-word-in-cache (str &key (type nil))
  (let ((synsets nil))
    (maphash (lambda (key value)
               (when (and
                      (member str (append (getf value :|word_en|)
                                          (getf value :|word_pt|))
                              :test #'string-equal)
                      (if type
                          (member type (getf value :|rdf_type|)
                                  :test #'string-equal)
                          t))
                 (push key synsets)))
             *wn*)
    synsets))

(defun get-cached-suggestions (id)
  (gethash id *suggestions*))

(defun get-cached-document (id)
  (gethash id *wn*))

(defun get-all-cached-ids ()
  (hash-table-keys *wn*))
