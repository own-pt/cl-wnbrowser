;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun make-solr-query-uri ()
   (format nil "~a/~a/select"
	   *solr-endpoint-uri*
	   *solr-collection-id*))

(defun make-solr-admin-query-uri ()
  (format nil "~a/admin/luke?wt=json" *solr-endpoint-uri*))

(defun get-facets-for-solr-query ()
  "Create the PLIST that specifies which fields should be used by SOLR
to construct the faceted search."
  (mapcar (lambda (f)
	    (cons "facet.field" (string f)))
	  *facets*))

(defun make-fq (&key rdf-type lex-file)
  "Creates the appropriate PLIST that should be fed to SOLR out of the
list of facet filters specified in the parameters RDF-TYPE and
LEX-FILE."
  (append 
   (mapcar #'(lambda (entry)
	       (cons "fq" (format nil "rdf_type:~a" entry)))
	   rdf-type)
   (mapcar #'(lambda (entry)
	       (cons "fq" (format nil "wn30_lexicographerFile:~a" entry)))
	   lex-file)))

(defun get-solr-query-plist (term df fq start rows)
  "Creates the PLIST that combines all the parameters (TERM DF FQ
START ROWS -- see the SOLR documentation for an explanation of those).
Some parameters are optional and should be specified as NIL."
  (remove
   nil 
   (append
    (list (when start (cons "start" start))
	  (when rows (cons "rows" rows))
	  (cons "q" term)
	  (when df (cons "df" df))
	  (cons "wt" "json")
	  (cons "facet" "true")
	  (cons "facet.mincount" "1")
	  (cons "indent" "false"))
    (when fq fq)
    (get-facets-for-solr-query))))

(defun %test-get-solr-query-plist ()
  (get-solr-query-plist
   "term" "df"
   (make-fq :rdf-type '("type1" "type2") :lex-file '("lex1" "lex2"))
   nil nil))

(defun execute-solr-admin-query ()
  (let ((stream (drakma:http-request
		 (make-solr-admin-query-uri)
		 :method :get
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))

(defun execute-solr-query (term &optional &key df fq start rows)
  "Calls the SELECT web service at the predefined SOLR URI, 
with TERM as the search term and DF as the default field."
  (let ((stream (drakma:http-request
		 (make-solr-query-uri)
		 :method :post
		 :external-format-out :utf-8
		 :parameters (get-solr-query-plist term df fq start rows)
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))

(defun search-solr-internal (term fq start rows)
  (execute-solr-query term :df "text" :fq fq :start start :rows rows))

(defun search-nomlexes (term)
  (execute-solr-query (format nil "nomlex_noun:\"~a\" nomlex_verb:\"~a\" nomlex_plural:\"~a\"" term term term)))

(defun search-solr-word-pt (term start rows)
  (execute-solr-query (format nil "word_pt:~a" term) :start start :rows rows))

(defun search-solr-by-id-internal (id)
  (execute-solr-query (format nil "\"~a\"" id) :df "id"))

(defun get-response (solr-result)
  (getjso "response" solr-result))

(defun get-docs (response)
  (getjso "docs" response))

(defun get-facet-fields (response)
  (getjso* "facet_counts.facet_fields" response))

(defun get-facet-count (facet response)
  (let ((facets (getf (get-facet-fields response) facet)))
    (when facets
      (process-pairs #'(lambda (a b)
			 (list :|name| a :|count| b)) facets))))

(defun get-facets-count (response)
  (mapcar #'(lambda (facet-field)
	      (let ((facet (get-facet-count facet-field response)))
		(cons facet-field facet)))
	  *facets*))

(defun get-num-found (response)
  (getjso "numFound" response))

(defun get-msg (result)
  (getjso* "error.msg" result))

(defun search-solr (term &optional fq start rows)
  (let* ((result (search-solr-internal term fq start rows))
	 (response (get-response result)))
    (if response
	(values
	 (get-docs response)
	 (get-num-found response)
	 (get-facets-count result)
	 nil)
	(values
	 nil nil nil (get-msg result)))))

(defun search-solr-by-id (id)
  (let ((response (get-response (search-solr-by-id-internal id))))
    (car (get-docs response))))

(defun get-related-synsets (term)
  "Search all synsets related to the given TERM.  Basically it
searches SOLR for documents whose 'word_pt' field matches TERM."
  (let ((response (get-response (search-solr-word-pt term "0" "1000"))))
    (get-docs response)))

(defun get-related-nomlexes-for-single-term (term)
  (remove-duplicates
   (mapcar #'(lambda (n)
	       (getjso "id" n))
	   (get-docs (get-response (search-nomlexes term))))
   :test #'equal))

(defun get-related-nomlexes (terms)
  (remove-duplicates
   (mapcan #'(lambda (term)
	       (get-related-nomlexes-for-single-term term))
	   terms)
   :test #'equal))

(defun is-synset (doc)
  (= 0 (count "Nominalization" doc :test #'string-equal)))

(defun get-synset-word-en (synset-id)
  "Returns the FIRST entry in the word_en property for the given SYNSET-ID"
  (let* ((response (get-response (search-solr-by-id-internal synset-id)))
	 (symset-response (car (get-docs response))))
    (car (getf symset-response :|word_en|))))

(defun get-solr-statistics ()
  (execute-solr-admin-query))
