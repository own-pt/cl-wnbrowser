(in-package :cl-wnbrowser)

(defparameter *suggestions* nil)
(defparameter *wn* nil)

(defun cache-dbs (&key (core-only nil))
  (setf *suggestions* (make-hash-table :test #'equal :size 200000))
  (setf *wn* (make-hash-table :test #'equal :size 150000))
  (cache-wn core-only)
  (cache-suggestions))

(defun nominalization? (doc)
  (member "Nominalization" (getf doc :|rdf_type|) :test #'equal))

(defun cache-wn (core-only)
  (dolist (s (execute-search
              (if core-only "rdf_type:CoreConcept" "*")
              :api "search-documents" :start "0" :limit "2000000"))
    (let ((id (getf s :|doc_id|)))
      (when (not (nominalization? s))
        (setf (gethash id *wn*) s)))))

(defun cache-suggestions ()
  (dolist (s (execute-search
              "*"
              :drilldown (make-drilldown-activity :status '("new")
                                                  :action '("add-gloss-pt"))
              :api "search-activities" :start "0" :limit "2000000"))
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
