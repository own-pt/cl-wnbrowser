(in-package :cl-wnbrowser)

(defparameter *gloss-suggestions* nil)
(defparameter *word-suggestions* nil)
(defparameter *wn* nil)

(defun cache-dbs (&key (core-only nil))
  (setf *gloss-suggestions* (make-hash-table :test #'equal :size 200000))
  (setf *word-suggestions* (make-hash-table :test #'equal :size 200000))
  (setf *wn* (make-hash-table :test #'equal :size 150000))
  (cache-wn core-only)
  (cache-word-suggestions)
  (cache-gloss-suggestions))

(defun nominalization? (doc)
  (member "Nominalization" (getf doc :|rdf_type|) :test #'equal))

(defun verb? (doc)
  (member "VerbSynset" (getf doc :|rdf_type|) :test #'equal))

(defun adj? (doc)
  (member "AdjectiveSynset" (getf doc :|rdf_type|) :test #'equal))

(defun adv? (doc)
  (member "AdverbSynset" (getf doc :|rdf_type|) :test #'equal))

(defun noun? (doc)
  (member "NounSynset" (getf doc :|rdf_type|) :test #'equal))

(defun get-en-words (doc)
  (getf doc :|word_en|))

(defun get-pt-words (doc)
  (getf doc :|word_pt|))

(defun cache-wn (core-only)
  (when (not *wn*)
    (setf *wn* (make-hash-table :test #'equal :size 150000)))
  (dolist (s (execute-search
              (if core-only "rdf_type:CoreConcept" "*")
              :api "search-documents" :start "0" :limit "2000000"))
    (let ((id (getf s :|doc_id|)))
      (when (not (nominalization? s))
        (setf (gethash id *wn*) s)))))

(defun cache-word-suggestions ()
  (cache-suggestions "add-word-pt" *word-suggestions*))

(defun cache-gloss-suggestions ()
  (cache-suggestions "add-gloss-pt" *gloss-suggestions*))

(defun cache-suggestions (action hash-table)
  (when (not hash-table)
    (setf hash-table (make-hash-table :test #'equal :size 200000)))
  (dolist (s (execute-search
              "*"
              :drilldown (make-drilldown-activity :status '("new")
                                                  :action (list action))
              :api "search-activities" :start "0" :limit "2000000"))
    (let ((id (getf s :|doc_id|)))
      (setf
       (gethash id hash-table)
       (push s (gethash id hash-table))))))

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

(defun get-cached-gloss-suggestions (id)
  (gethash id *gloss-suggestions*))

(defun get-cached-document (id)
  (gethash id *wn*))

(defun get-all-cached-ids ()
  (hash-table-keys *wn*))

(defun get-all-cached-ids-by-type (type)
  (let ((predicate-function 
	 (case type
	   (:adj #'adj?)
	   (:adv #'adv?) 
	   (:verb #'verb?) 
	   (:noun #'noun?))))
    (remove-if-not (lambda (id)
		     (funcall predicate-function (get-cached-document id))) (get-all-cached-ids))))

(defun get-all-noun-ids ()
  (get-all-cached-ids-by-type :noun))

(defun get-all-adv-ids ()
  (get-all-cached-ids-by-type :adv))

(defun get-all-adj-ids ()
  (get-all-cached-ids-by-type :adj))

(defun get-all-verb-ids ()
  (get-all-cached-ids-by-type :verb))
