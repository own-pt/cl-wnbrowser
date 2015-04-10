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
  (let ((stream (drakma:http-request
                 (format nil "~a/~a" *ownpt-api-uri* api)
		 :method :get
		 :external-format-out :utf-8
		 :parameters (get-cloudant-query-plist term drilldown bookmark)
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))

(defun delete-suggestion (id)
  (drakma:http-request
   (format nil "~a/delete-suggestion/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun accept-suggestion (id)
  (drakma:http-request
   (format nil "~a/accept-suggestion/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun reject-suggestion (id)
  (drakma:http-request
   (format nil "~a/reject-suggestion/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun delete-comment (id)
  (drakma:http-request
   (format nil "~a/delete-comment/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defun add-suggestion (id doc-type type param login)
  (drakma:http-request
   (format nil "~a/add-suggestion/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :method :get
   :external-format-out :utf-8
   :parameters (list (cons "doc_type" doc-type)
                     (cons "suggestion_type" type)
                     (cons "params" param)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))

(defun add-comment (id doc-type text login)
  (drakma:http-request
   (format nil "~a/add-comment/~a"
           *ownpt-api-uri* (drakma:url-encode id :utf-8))
   :method :get
   :external-format-out :utf-8
   :parameters (list (cons "doc_type" doc-type)
                     (cons "text" text)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))

(defun get-suggestions (id)
  (let ((stream (drakma:http-request
		 (format nil "~a/get-suggestions/~a" *ownpt-api-uri* id)
		 :method :get
		 :external-format-out :utf-8
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      (get-docs obj))))

(defun get-comments (id)
  (let ((stream (drakma:http-request
		 (format nil "~a/get-comments/~a" *ownpt-api-uri* id)
		 :method :get
		 :external-format-out :utf-8
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      (get-docs obj))))

(defun get-document-by-id (doctype id)
  (let ((stream (drakma:http-request
		 (format nil "~a/~a/~a" *ownpt-api-uri* doctype
                         (drakma:url-encode id :utf-8))
		 :method :get
		 :external-format-out :utf-8
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))

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
   (when rdf-type
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"rdf_type\",\"~a\"]" entry)))
	     rdf-type))
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

(defun get-activity ()
  (let ((stream (drakma:http-request
		 (format nil "~a/get-activity" *ownpt-api-uri*)
		 :method :get
                 :parameters (list (cons "key" *ownpt-api-key*))
		 :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))
