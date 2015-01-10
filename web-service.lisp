;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

;;;(setq hunchentoot:*show-lisp-errors-p* t)

(defun get-previous (n &optional (step 10))
  (- n step))

(defun get-next (n max &optional (step 10))
  (+ n step))

(defun process-synset (synset)
  (cl-wnbrowser.templates:synset synset))

(defun process-nomlex (nomlex)
  (cl-wnbrowser.templates:nomlex nomlex))

(defun process-results (result)
  (cl-wnbrowser.templates:result result))

(defun process-error (result)
  (cl-wnbrowser.templates:error result))

(hunchentoot:define-easy-handler (get-root-handler-redirector :uri "/wn") ()
  (hunchentoot:redirect "/wn/"))

(hunchentoot:define-easy-handler (get-root-handler :uri "/wn/") ()
  (cl-wnbrowser.templates:home))

(hunchentoot:define-easy-handler (get-update-stats-handler :uri "/wn/update-stats") ()
  (update-stat-cache)
  (if (string-equal "application/json" (hunchentoot:header-in* :accept))
      (progn
	(setf (hunchentoot:content-type*) "application/json")
	(with-output-to-string (s) (yason:encode-plist (list :result "Done") s)))
      (hunchentoot:redirect "/wn/stats")))

(hunchentoot:define-easy-handler (get-stats-handler :uri "/wn/stats") ()
  (cl-wnbrowser.templates:stats
   (append
    (stats-count-classes-plist)
    (stats-percent-complete-plist))))

(hunchentoot:define-easy-handler (search-solr-handler :uri "/wn/search") (term fq start debug)
  (setf (hunchentoot:content-type*) "text/html")
  (multiple-value-bind (num-found documents facets error) (search-solr term fq start)
    (if error
	(process-error (list :error error :term term))
	(let ((start/i (if start (parse-integer start) 0)))
	  (process-results
	   (list :fq fq :debug debug :term term
		 :previous (get-previous start/i)
		 :next (get-next start/i num-found)
		 :start start/i :numfound num-found
		 :facets facets :documents documents))))))
  
(hunchentoot:define-easy-handler (get-synset-handler :uri "/wn/synset") (id debug term)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((synset (search-solr-by-id id)))
    (process-synset
     (append
      (list :term term
	    :debug debug
	    :synset synset)
      synset))))

(hunchentoot:define-easy-handler (get-nomlex-handler :uri "/wn/nomlex") (id debug term)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((nomlex (search-solr-by-id id)))
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
