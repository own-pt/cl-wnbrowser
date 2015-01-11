;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun read-query (query-file)
   (read-file-into-string (merge-pathnames query-file *basedir*)))

(defun make-agraph-query-uri ()
  (format nil "~a/~a" *allegro-graph-url* *allegro-graph-repository*))

(defun call-allegro-graph (query)
  (let ((stream
	 (nth-value 0 (drakma:http-request
		       (make-agraph-query-uri)
		       :accept "text/csv"
		       :method :post
		       :external-format-out :utf-8
		       :parameters (list (cons "query" query))
		       :want-stream nil))))
    (cl-csv:read-csv stream :skip-first-p t)))

(defparameter *query-cache* (make-hash-table))

(defun initialize-stat-cache ()
  (progn
    (setf
     (gethash :percent-complete *query-cache*)
     '(("unknown" "0" "0" "0")))
    (setf
     (gethash :count-classes *query-cache*)
     '(("http://arademaker.github.com/wn30/schema/unknown" "0" "0" "0")))))

(initialize-stat-cache)

(defun stats-percent-complete-query ()
  (call-allegro-graph (read-query *query/by-lexfile*)))

(defun stats-count-classes-query ()
  (call-allegro-graph (read-query *query/by-pos-pt*)))

(defun update-stat-cache ()
  (progn
    (setf
     (gethash :percent-complete *query-cache*)
     (stats-percent-complete-query))
    (setf
     (gethash :count-classes *query-cache*)
     (stats-count-classes-query))))

(defun remove-schema-uri (uri)
  (cl-ppcre:regex-replace *wn30-schema-uri* uri ""))

(defun stats-count-classes-plist ()
  (list :rowscount (mapcar
	       (lambda (row)
		 (list :type (remove-schema-uri (first row))
		       :totalpt (parse-integer (second row))
		       :totalpr (parse-integer (third row))
		       :percent (parse-number (fourth row))))
	       (gethash :count-classes *query-cache*))))

(defun stats-percent-complete-plist ()
  (list :rowspercent (mapcar
	       (lambda (row)
		 (list :lexfile (first row)
		       :totalpt (parse-integer (second row))
		       :totalpr (parse-integer (third row))
		       :percent (parse-number (fourth row))))
	       (gethash :percent-complete *query-cache*))))
