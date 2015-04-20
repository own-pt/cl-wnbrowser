;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun get-synset-word-en (id)
  "Returns the FIRST entry in the word_en property for the given SYNSET-ID"
  (let* ((result (execute-cloudant-query (format nil "id:\"~a\"" id)))
	 (synset (car (get-docs result))))
    (car (getf synset :|word_en|))))

(defun get-cloudant-query-plist (q drilldown bookmark)
  (remove
   nil
   (append 
    (list
     (when q (cons "q" q))
     (when (and bookmark
		(> (length bookmark) 0))
       (cons "bookmark" bookmark)))
    (when drilldown drilldown))))

(defun execute-cloudant-query (term &key drilldown bookmark (api "search-documents"))
  (call-rest-method
   api
   :parameters (get-cloudant-query-plist term drilldown bookmark)))

(defun delete-suggestion (id)
  (call-rest-method
   (format nil "delete-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun accept-suggestion (id)
  (call-rest-method
   (format nil "accept-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun reject-suggestion (id)
  (call-rest-method
   (format nil "reject-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun delete-comment (id)
  (call-rest-method
   (format nil "delete-comment/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun add-suggestion (id doc-type type param login)
  (call-rest-method 
   (format nil "add-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "doc_type" doc-type)
                     (cons "suggestion_type" type)
                     (cons "params" param)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))

(defun add-comment (id doc-type text login)
  (call-rest-method 
   (format nil "add-comment/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "doc_type" doc-type)
                     (cons "text" text)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))

(defun get-suggestions (id)
  (get-docs (call-rest-method (format nil "get-suggestions/~a" id))))

(defun get-comments (id)
  (get-docs (call-rest-method (format nil "get-comments/~a" id))))

(defun delete-vote (id)
  (call-rest-method (format nil "delete-vote/~a" id)
                    :parameters (list (cons "key" *ownpt-api-key*))))

(defun add-vote (id user value)
  (call-rest-method (format nil "add-vote/~a" id)
                    :parameters (list
                                 (cons "user" user)
                                 (cons "value" (format nil "~a" value))
                                 (cons "key" *ownpt-api-key*))))
    
(defun get-document-by-id (doctype id)
  (call-rest-method (format nil "~a/~a" doctype (drakma:url-encode id :utf-8))))

(defun request-successful? (result)
  (null (getjso "error" result)))

(defun get-bookmark (result)
  (getjso "bookmark" result))

(defun get-error-reason (result)
  (getjso "reason" result))

(defun get-docs (result)
  (mapcar #'(lambda (row) (getjso "doc" row))
	  (getjso "rows" result)))

(defun get-num-found (result)
  (getjso "total_rows" result))

(defun get-facet-fields (response)
  (getjso "counts" response))

(defun make-drilldown (&key rdf-type lex-file word-count-pt word-count-en)
  "Creates the appropriate PLIST that should be fed to SOLR out of the
list of facet filters specified in the parameters RDF-TYPE and
LEX-FILE."
  (append
   (when word-count-pt
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"word_count_pt\",\"~a\"]" entry)))
	     word-count-pt))
   (when word-count-en
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"word_count_en\",\"~a\"]" entry)))
	     word-count-en))
   ;;; this deserves a better explanation.  As it stands, Cloudant
   ;;; does not seem to support multivalued fields.  We managed to work
   ;;; around the issue by issuing multiple "rdf_type" indexes when we
   ;;; encounter a document that contains an array of values.  This works
   ;;; fine until we attempt to drilldown into more than one of these values
   ;;; Cloudant complains that the dimension has already been added.
   ;;; Yet another workaround is to create multiple filter fields rdf_type1,
   ;;; rdf_type2, etc. so we can filter down on more than one value of the
   ;;; same field at the same time.
   (when rdf-type
     (let ((c 1))
       (mapcar #'(lambda (entry)
                   (progn
                     (setq c (1+ c))
                     (cons "drilldown"
                           (format nil "[\"rdf_type~a\",\"~a\"]" c entry))))
	     rdf-type)))
   (when lex-file
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"wn30_lexicographerFile\",\"~a\"]"
                               entry)))
	     lex-file))))

(defun make-drilldown-activity (&key type action status doc_type user provenance)
  (append
   (when provenance
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"provenance\",\"~a\"]" entry)))
	     provenance))
   (when type
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"type\",\"~a\"]" entry)))
	     type))
   (when action
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"action\",\"~a\"]" entry)))
	     action))
   (when status
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"status\",\"~a\"]" entry)))
	     status))
   (when doc_type
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"doc_type\",\"~a\"]" entry)))
	     doc_type))
   (when user
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"user\",\"~a\"]"
                               entry)))
	     user))))

(defun search-cloudant (term drilldown bookmark api)
  (let* ((result (execute-cloudant-query term
                                         :drilldown drilldown
                                         :bookmark bookmark
                                         :api api))
	 (success (request-successful? result)))
    (if success
	(values
	 (get-docs result)
	 (get-num-found result)
	 (get-facet-fields result)
	 (get-bookmark result)
	 nil)
	(values nil nil nil nil (get-error-reason result)))))

(defun get-synset (id)
  (get-document-by-id "synset" id))

(defun get-nomlex (id)
  (get-document-by-id "nomlex" id))

(defun get-sense-tagging ()
  (call-rest-method "sense-tagging"))

(defun get-root ()
  (call-rest-method ""))

(defun call-rest-method (method &key parameters)
    (let* ((stream (drakma:http-request
                   (format nil "~a/~a" *ownpt-api-uri* method)
                   :parameters parameters
		   :external-format-out :utf-8
                   :method :get
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))
  
