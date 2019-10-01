;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)


;; own-api aux

(defun get-search-query-plist (q drilldown limit start sort-field sort-order fl)
  (remove
   nil
   (append 
    (list
     (when q (cons "q" q))
     (when fl (cons "fl" fl))
     (when start (cons "start" start))
     (when sort-field (cons "sf" sort-field))
     (when sort-order (cons "so" sort-order))
     (when (and limit (parse-integer limit :junk-allowed t))
           (cons "limit" limit)))
    (when drilldown drilldown))))

(defun execute-search-query (term &key drilldown limit sort-field sort-order (start 0) fl num-pages (api "search-documents"))
  (call-rest-method
   api
   :parameters (get-search-query-plist term drilldown limit start sort-field sort-order fl)))
    
(defun get-document-by-id (doctype id)
  (call-rest-method (format nil "~a/~a" doctype (drakma:url-encode id :utf-8))))

(defun request-successful? (result)
  (not (string-equal "SolrError" (getjso "name" result))))

(defun get-error-reason (result)
  (getjso "message" result))

(defun get-docs (result)
  (mapcar #'(lambda (row) (getjso "doc" row))
	  (getjso "rows" result)))

(defun get-num-found (result)
  (getjso "total_rows" result))

(defun get-facet-fields (response)
  (getjso "counts" response))

(defun make-drilldown (&key rdf-type lex-file word-count-pt word-count-en frame)
  "Creates the appropriate PLIST that should be fed to SOLR out of the
list of facet filters specified in the parameters RDF-TYPE and
LEX-FILE."
  (append
   (when frame
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"wn30_frame\",\"~a\"]" entry)))
	     frame))
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

(defun make-drilldown-activity (&key tag type action status doc_type user provenance sum_votes num_votes)
  (append
   (when sum_votes
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"sum_votes\",\"~a\"]" entry)))
	     sum_votes))
   (when num_votes
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"vote_score\",\"~a\"]" entry)))
	     num_votes))
   (when tag
     (mapcar #'(lambda (entry)
		 (cons "drilldown"
                       (format nil "[\"tags\",\"~a\"]" entry)))
	     tag))
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

(defun get-synset-ids (term drilldown start limit)
  (let* ((result (execute-search-query term
                                         :drilldown drilldown
                                         :api "search-documents"
                                         :start start
                                         :limit limit
                                         :fl "doc_id"))
	 (success (request-successful? result)))
    (if success
        (mapcar (lambda (s) (getf s :|doc_id|)) (get-docs result))
        nil)))

(defun get-nomlex (id)
  (get-document-by-id "nomlex" id))

(defun get-sense-tagging ()
  (call-rest-method "sense-tagging"
                    :parameters (list (cons "file" "bosque.json"))))

(defun get-sense-tagging-detail (file text word)
  (call-rest-method "sense-tagging-detail"
                    :parameters (list (cons "file" file)
                                      (cons "text" text)
                                      (cons "word" word))))

(defun get-root ()
  (call-rest-method ""))

(defun get-statistics ()
  (call-rest-method "statistics"))

(defun call-rest-method/stream (method &key parameters)
  "Alternative to CALL-REST-METHOD that uses a stream; this is more
memory efficient, but it may cause problems if YASON:PARSE takes too
long to parse the stream and the stream may be cut due to timeout."
    (let* ((stream (drakma:http-request
                   (format nil "~a/~a" *ownpt-api-uri* method)
                   :parameters parameters
		   :external-format-out :utf-8
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))
  
(defun call-rest-method (method &key parameters)
  (let ((octets (drakma:http-request
                 (format nil "~a/~a" *ownpt-api-uri* method)
                 :parameters parameters
		 :external-format-out :utf-8
                 :method :get
                 :connection-timeout 120
                 :want-stream nil)))
    (yason:parse
     (flexi-streams:octets-to-string octets :external-format :utf-8)
     :object-as :plist :object-key-fn #'make-keyword)))

  
;;; own-api backend

(defmethod get-synset ((backend (eql 'own-api)) id)
  (get-document-by-id "synset" id))

(defmethod delete-suggestion ((backend (eql 'own-api)) id)
  (call-rest-method
   (format nil "delete-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))

(defmethod accept-suggestion ((backend (eql 'own-api)) id)
  (call-rest-method
   (format nil "accept-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))


(defmethod reject-suggestion ((backend (eql 'own-api)) id)
  (call-rest-method
   (format nil "reject-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))


(defmethod delete-comment ((backend (eql 'own-api)) id)
  (call-rest-method
   (format nil "delete-comment/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "key" *ownpt-api-key*))))


(defmethod add-suggestion ((backend (eql 'own-api)) id doc-type type param login)
  (call-rest-method 
   (format nil "add-suggestion/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "doc_type" doc-type)
                     (cons "suggestion_type" type)
                     (cons "params" param)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))


(defmethod add-comment ((backend (eql 'own-api)) id doc-type text login)
  (call-rest-method 
   (format nil "add-comment/~a" (drakma:url-encode id :utf-8))
   :parameters (list (cons "doc_type" doc-type)
                     (cons "text" text)
                     (cons "key" *ownpt-api-key*)
                     (cons "user" login))))


(defmethod get-suggestions ((backend (eql 'own-api)) id)
  (get-docs (call-rest-method (format nil "get-suggestions/~a" id))))


(defmethod get-comments ((backend (eql 'own-api)) id)
  (get-docs (call-rest-method (format nil "get-comments/~a" id))))


(defmethod delete-vote ((backend (eql 'own-api)) id)
  (call-rest-method (format nil "delete-vote/~a" id)
                    :parameters (list (cons "key" *ownpt-api-key*))))


(defmethod add-vote ((backend (eql 'own-api)) id user value)
  (call-rest-method (format nil "add-vote/~a" id)
                    :parameters (list
                                 (cons "user" user)
                                 (cons "value" (format nil "~a" value))
                                 (cons "key" *ownpt-api-key*))))


(defmethod execute-search ((backend (eql 'own-api)) term  &key search-field rdf-type lex-file word-count-pt word-count-en
						      frame start limit sf so fl num-pages)
  (let* ((drilldown (make-drilldown :rdf-type rdf-type
                                    :lex-file lex-file
                                    :frame frame
                                    :word-count-pt word-count-pt
                                    :word-count-en word-count-en))
	 (api "search-documents")
	 (result (execute-search-query term
                                       :drilldown drilldown
                                       :api api
                                       :start start
                                       :limit limit
                                       :num-pages num-pages
                                       :sort-field sf
                                       :fl fl
                                       :sort-order so))
	 (success (request-successful? result)))
    (if success
	(values
	 (get-docs result)
	 (get-num-found result)
	 (get-facet-fields result)
	 nil)
	(values nil nil nil (get-error-reason result)))))
