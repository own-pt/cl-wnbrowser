;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(setq hunchentoot:*show-lisp-errors-p* t)

(defun get-previous (n &optional (step 10))
  (- n step))

(defun get-next (n &optional (step 10))
  (+ n step))

(defun get-login ()
  (list :login (hunchentoot:session-value :login)))

(defun process-synset (parameters)
  (if (getf parameters :error)
      (cl-wnbrowser.templates:synset-invalid
       (append
        (get-login)
        parameters))
      (cl-wnbrowser.templates:synset
       (append
        (get-login)
        parameters))))

(defun make-callback-uri (request-uri)
  (let* ((redirect-uri (format nil "http://~a~a" *base-url* request-uri))
         (callback-uri (format nil "http://~a/callback?destination=~a"
                               *base-url*
                               (hunchentoot:url-encode request-uri))))
    callback-uri))

(defun process-nomlex (nomlex)
  (cl-wnbrowser.templates:nomlex
   (append (get-login) nomlex)))

(defun process-results (result)
  (cl-wnbrowser.templates:result result))

(defun process-error (result)
  (cl-wnbrowser.templates:searcherror result))

(hunchentoot:define-easy-handler (get-root-handler-redirector :uri "/wn") ()
  (hunchentoot:redirect "/"))



(hunchentoot:define-easy-handler (get-root-handler :uri "/") ()
  (cl-wnbrowser.templates:home
   (append
    (get-login)
    (list
     :info (when (equal 'own-api *backend*) (get-root))
     :githubid *github-client-id*))))

(defun disable-caching ()
  (hunchentoot:no-cache))
 
(hunchentoot:define-easy-handler (execute-search-handler :uri "/search")
    (term search_field start debug limit num-pages
	  (fq_frame :parameter-type 'list)
          (fq_word_count_pt :parameter-type 'list)
	  (fq_word_count_en :parameter-type 'list)
	  (fq_rdftype :parameter-type 'list)
	  (fq_lexfile :parameter-type 'list) sexp)
  (disable-caching)
  (setf (hunchentoot:session-value :term) term)
  (setf (hunchentoot:session-value :search_field) search_field)
  (if (is-synset-id term)
      (hunchentoot:redirect (format nil "/synset?id=~a" term))
      (multiple-value-bind
	     (documents num-found facets error)
          (execute-search
	   *backend*
           term
	   :search-field search_field
           :rdf-type fq_rdftype
           :lex-file fq_lexfile
           :frame fq_frame
           :word-count-pt fq_word_count_pt
           :word-count-en fq_word_count_en
           :start start :limit limit)
	(let* ((start/i (if start (parse-integer start) 0))
	       (limit/i (if limit (parse-integer limit) 10))
         (num-pages/i (if num-pages (parse-integer num-pages) 5))
	       (request-uri (hunchentoot:request-uri*))
	      (result (if error 
			  (list :error error :term term)
			  (list :debug debug :term term :search_field search_field
				:githubid *github-client-id*
				:login (hunchentoot:session-value :login)
				:callbackuri (make-callback-uri request-uri)
				:returnuri request-uri
				:info (when (equal 'own-api *backend*) (get-root))
				:fq_frame fq_frame
				:fq_rdftype fq_rdftype
				:fq_lexfile fq_lexfile
				:fq_word_count_pt fq_word_count_pt
				:fq_word_count_en fq_word_count_en
				:previous (get-previous start/i)
				:next (get-next start/i limit/i)
				:limit limit/i
				:start start/i :numfound num-found
				:facets facets :documents documents :numpages num-pages/i))))
	(if (string-equal "yes" sexp)
	    (progn
	      (setf (hunchentoot:content-type*) "application/sexp")
	      (with-output-to-string (s)
		(print result s)))
	    (progn
	      (setf (hunchentoot:content-type*) "text/html")
	      (if error
		  (process-error result)
		  (progn 
		    (hunchentoot:delete-session-value :ids)
		    (process-results result)))))))))
		  
(hunchentoot:define-easy-handler (search-activity-handler :uri "/search-activities")
    (term start debug sf so
          (fq_sum_votes :parameter-type 'list)
          (fq_num_votes :parameter-type 'list)
	  (fq_type :parameter-type 'list)
	  (fq_tag :parameter-type 'list)
	  (fq_action :parameter-type 'list)
	  (fq_status :parameter-type 'list)
	  (fq_doc_type :parameter-type 'list)
          (fq_provenance :parameter-type 'list)
          (fq_user :parameter-type 'list) sexp)
  (disable-caching)
  (multiple-value-bind
        (documents num-found facets error)
      (search-activities
       *backend*
       term
       :sum_votes fq_sum_votes
       :num_votes fq_num_votes
       :type fq_type
       :tags fq_tag
       :action fq_action
       :status fq_status
       :doc_type fq_doc_type
       :provenance fq_provenance
       :user fq_user
       :start start
       :limit "25" :sf sf :so so)

    (let* ((start/i (if start (parse-integer start) 0))
           (request-uri (hunchentoot:request-uri*))
           (result (if error (list :error error :term term)
                       (append (get-login)
                               (list :debug debug
                                     :info (when (equal 'own-api *backend*) (get-root))
                                     :term term
                                     :githubid *github-client-id*
                                     :login (hunchentoot:session-value :login)
                                     :callbackuri (make-callback-uri request-uri)
                                     :returnuri request-uri
                                     :fq_type fq_type
                                     :fq_num_votes fq_num_votes
                                     :fq_sum_votes fq_sum_votes
                                     :fq_tag fq_tag
                                     :fq_action fq_action
                                     :fq_status fq_status
                                     :fq_doc_type fq_doc_type
                                     :fq_user fq_user
                                     :fq_provenance fq_provenance
                                     :previous (get-previous start/i)
                                     :next (get-next start/i 25)
                                     :so so
                                     :sf sf
                                     :start start/i :numfound num-found
                                     :facets facets
                                     :documents documents)))))
      (if (string-equal "yes" sexp)
          (progn
            (setf (hunchentoot:content-type*) "application/sexp")
            (with-output-to-string (s)
              (print result s)))
          (progn
            (setf (hunchentoot:session-value :term) term)
            (setf (hunchentoot:content-type*) "text/html")
            (if error (process-error (list :error error :term term))
                (cl-wnbrowser.templates:activities result)))))))

(hunchentoot:define-easy-handler
    (get-synset-handler :uri "/synset") (id debug sexp)
  (disable-caching)
  (let* ((synset (get-synset *backend* id))
         (suggestions (get-suggestions *backend* id))
         (comments (get-comments *backend* id))
         (request-uri (hunchentoot:request-uri*))
	 (term (hunchentoot:session-value :term))
	 (ids (hunchentoot:session-value :ids))
	 (search_field (hunchentoot:session-value :search_field)))
    (if (string-equal "yes" sexp)
	  (progn
	    (setf (hunchentoot:content-type*) "application/sexp")
	    (with-output-to-string (s)
              (print (list :comments comments :suggestions suggestions :synset synset) s)))
	  (progn
	    (when (not (string-equal (lastcar ids) id))
	      (setf (hunchentoot:session-value :ids) (append ids (list id))))
	    (setf (hunchentoot:content-type*) "text/html")
	    (process-synset
	     (append (list
		      :original-id id
		      :ids (last (hunchentoot:session-value :ids) *breadcrumb-size*)
		      :term term
		      :callbackuri (make-callback-uri request-uri)
		      :returnuri request-uri
		      :debug debug
		      :comments comments
		      :suggestions suggestions
		      :githubid *github-client-id*
		      :synset synset
		      :error (when (null synset) t))
		     synset
		     (list :search_field search_field)))))))

;; (hunchentoot:define-easy-handler (get-nomlex-handler
;; 				  :uri "/wn/nomlex") (id debug term)
;;   (setf (hunchentoot:content-type*) "text/html")
;;   (disable-caching)
;;   (let ((nomlex (get-nomlex id))
;; 	(term (hunchentoot:session-value :term)))
;;     (process-nomlex
;;      (append
;;       (list :term term
;; 	    :info (get-root)
;; 	    :callbackuri (make-callback-uri (hunchentoot:request-uri*))
;; 	    :debug debug
;;             :githubid *github-client-id*
;; 	    :nomlex nomlex)
;;       nomlex))))

(hunchentoot:define-easy-handler
    (process-suggestion-handler :uri "/process-suggestion") (id doc_type type params return-uri)
  (let ((login (hunchentoot:session-value :login)))
    (if login
        (let ((suggestion-id (add-suggestion *backend* id doc_type type params login)))
	  (if (member login *vote-authorized-accounts* :test #'equal)
	      (add-vote *backend* suggestion-id login 1))
	  (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
        (progn
          (setf (hunchentoot:content-type*) "text/html")
          (format nil "invalid login")))))

(hunchentoot:define-easy-handler (delete-suggestion-handler
				  :uri "/delete-suggestion") (id return-uri)
  (let ((login (hunchentoot:session-value :login)))
    (if login
        (progn
          (delete-suggestion *backend* login id)
          (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
        (progn
          (setf (hunchentoot:content-type*) "text/html")
          (format nil "invalid login")))))

;; (hunchentoot:define-easy-handler (accept-suggestion-handler
;; 				  :uri "/wn/accept-suggestion") (id return-uri)
;;   (let ((login (hunchentoot:session-value :login)))
;;     (if login
;;         (progn
;;           (accept-suggestion id)
;;           (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
;;         (progn
;;           (setf (hunchentoot:content-type*) "text/html")
;;           (format nil "invalid login")))))

;; (hunchentoot:define-easy-handler (reject-suggestion-handler
;; 				  :uri "/wn/reject-suggestion") (id return-uri)
;;   (let ((login (hunchentoot:session-value :login)))
;;     (if login
;;         (progn
;;           (reject-suggestion id)
;;           (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
;;         (progn
;;           (setf (hunchentoot:content-type*) "text/html")
;;           (format nil "invalid login")))))

;; comments
(hunchentoot:define-easy-handler
    (process-comment-handler :uri "/process-comment") (id doc_type text return-uri)
  (let ((login (hunchentoot:session-value :login)))
    (if login
        (progn
          (add-comment *backend* id doc_type text login)
          (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
        (progn
          (setf (hunchentoot:content-type*) "text/html")
          (format nil "invalid login")))))

(hunchentoot:define-easy-handler
    (delete-comment-handler :uri "/delete-comment") (id return-uri)
  (let ((login (hunchentoot:session-value :login)))
    (if login
        (progn
          (delete-comment *backend* login id)
          (hunchentoot:redirect (hunchentoot:url-decode return-uri)))
        (progn
          (setf (hunchentoot:content-type*) "text/html")
          (format nil "invalid login")))))

;; votes
(hunchentoot:define-easy-handler
    (vote-up-handler :uri "/vote-up") (id)
  (let ((login (hunchentoot:session-value :login)))
    (setf (hunchentoot:content-type*) "application/json")
    (if login
        (with-output-to-string (s)
          (yason:encode-plist (add-vote *backend* id login 1) s))
        (with-output-to-string (s)
          (yason:encode-plist (list :result "not-authorized") s)))))

(hunchentoot:define-easy-handler
    (vote-down-handler :uri "/vote-down") (id)
  (let ((login (hunchentoot:session-value :login)))
    (setf (hunchentoot:content-type*) "application/json")
    (if login
        (with-output-to-string (s)
          (yason:encode-plist (add-vote *backend* id login -1) s))
        (with-output-to-string (s)
          (yason:encode-plist (list :result "not-authorized") s)))))


(hunchentoot:define-easy-handler
    (delete-vote-handler :uri "/delete-vote") (id)
  (let ((login (hunchentoot:session-value :login)))
    (when login
      (delete-vote *backend* id))
    (setf (hunchentoot:content-type*) "application/json")
    (with-output-to-string (s)
      (yason:encode-plist (list :result "Done") s))))

;; github callback
(hunchentoot:define-easy-handler
    (github-callback-handler :uri "/callback") (code destination)
  (let ((access-token (get-access-token code))
        (request-uri (when destination (hunchentoot:url-decode destination))))
    (setf (hunchentoot:session-value :login) (get-user-login (get-user access-token)))
    (if (cl-strings:starts-with request-uri "/")
        (hunchentoot:redirect request-uri)
        (hunchentoot:redirect "/"))))

(defun publish-static-content (dir)
  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/st/"
	 (merge-pathnames #p"static/" dir))
	hunchentoot:*dispatch-table*))

(defun start-server (&optional (port 4243))
  (publish-static-content *basedir*)
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
		  :address "0.0.0.0"
		  :access-log-destination (merge-pathnames #p"wn.log" *basedir*)
		  :port port)))
