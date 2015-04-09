;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(setq hunchentoot:*show-lisp-errors-p* t)

(defun get-previous (n &optional (step 10))
  (- n step))

(defun get-next (n &optional (step 10))
  (+ n step))

(defun process-synset (synset)
  (cl-wnbrowser.templates:synset synset))

(defun process-nomlex (nomlex)
  (cl-wnbrowser.templates:nomlex nomlex))

(defun process-results (result)
  (cl-wnbrowser.templates:result result))

(defun process-error (result)
  (cl-wnbrowser.templates:searcherror result))

(hunchentoot:define-easy-handler (get-root-handler-redirector :uri "/wn") ()
  (hunchentoot:redirect "/wn/"))

(hunchentoot:define-easy-handler (get-root-handler :uri "/wn/") ()
  (cl-wnbrowser.templates:home))

(hunchentoot:define-easy-handler (get-update-stats-handler
				  :uri "/wn/update-stats") ()
  (update-stat-cache)
  (if (string-equal "application/json" (hunchentoot:header-in* :accept))
      (progn
	(setf (hunchentoot:content-type*) "application/json")
	(with-output-to-string (s)
	  (yason:encode-plist (list :result "Done") s)))
      (hunchentoot:redirect "/wn/stats")))

(defun disable-caching ()
  (setf (hunchentoot:header-out :cache-control)
	"no-cache, no-store, must-revalidate")
  (setf (hunchentoot:header-out :pragma) "no-cache")
  (setf (hunchentoot:header-out :expires) 0))

(hunchentoot:define-easy-handler (get-stats-handler :uri "/wn/stats") ()
  (disable-caching)
  (cl-wnbrowser.templates:stats
   (append
    (stats-count-classes-plist)
    (stats-percent-complete-plist))))

(hunchentoot:define-easy-handler (get-solr-stats-handler
				  :uri "/wn/solr-stats") ()
  (disable-caching)
  (cl-wnbrowser.templates:solrstats
   (get-solr-statistics)))

(hunchentoot:define-easy-handler (search-solr-handler :uri "/wn/search")
    (term start debug
	  (fq_word_count_pt :parameter-type 'list)
	  (fq_word_count_en :parameter-type 'list)
	  (fq_rdftype :parameter-type 'list)
	  (fq_lexfile :parameter-type 'list)
          limit)
  (setf (hunchentoot:content-type*) "text/html")
  (disable-caching)
  (if (is-synset-id term)
      (hunchentoot:redirect (format nil "/wn/synset?id=~a" term))
      (multiple-value-bind
	    (documents num-found facets error)
	  (search-solr term (make-fq :rdf-type fq_rdftype
				     :lex-file fq_lexfile
				     :word-count-pt fq_word_count_pt
				     :word-count-en fq_word_count_en) start limit)
	(if error
	    (process-error (list :error error :term term))
	    (let* ((start/i (if start (parse-integer start) 0)))
	      (hunchentoot:delete-session-value :ids)
	      (setf (hunchentoot:session-value :term) term)
	      (process-results
	       (list :debug debug :term term
		     :fq_rdftype fq_rdftype
		     :fq_lexfile fq_lexfile
		     :fq_word_count_pt fq_word_count_pt
		     :fq_word_count_en fq_word_count_en
		     :previous (get-previous start/i)
		     :next (get-next start/i)
		     :start start/i :numfound num-found
		     :facets facets :documents documents)))))))
  
(hunchentoot:define-easy-handler (get-synset-handler
				  :uri "/wn/synset") (id debug)
  (setf (hunchentoot:content-type*) "text/html")
  (disable-caching)
  (let* ((synset (search-solr-by-id id))
	 (term (hunchentoot:session-value :term))
	 (ids (hunchentoot:session-value :ids)))
    (when (not (string-equal (lastcar ids) id))
      (setf (hunchentoot:session-value :ids) (append ids (list id))))
    (process-synset
     (append
      (list
       :ids (last (hunchentoot:session-value :ids) *breadcrumb-size*)
       :term term
       :debug debug
       :synset synset)
      synset))))

(hunchentoot:define-easy-handler (get-nomlex-handler
				  :uri "/wn/nomlex") (id debug term)
  (setf (hunchentoot:content-type*) "text/html")
  (disable-caching)
  (let ((nomlex (search-solr-by-id id))
	(term (hunchentoot:session-value :term)))
    (process-nomlex
     (append
      (list :term term
	    :debug debug
	    :nomlex nomlex)
      nomlex))))

(defun start-server (&optional (port 4243))
  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/wn/st/"
	 (merge-pathnames #p"static/" *basedir*)) hunchentoot:*dispatch-table*)
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
		  :access-log-destination (merge-pathnames #p"wn.log" *basedir*)
		  :port port)))
