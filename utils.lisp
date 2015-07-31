;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defun process-pairs (function list)
  "Process LIST and applies FUNCTION to every pair, accumulating the
results of FUNCTION via APPEND.  It is expected that FUNCTION returns
either a list or a cons.  Example:

CL-USER> (process-pairs #'cons '(1 2 3 4))
((1 . 2) (3 . 4))"
  (append
   (destructuring-bind (a b . rest) list
     (cons (funcall function a b)
	   (when rest (process-pairs function rest))))))

(defun is-synset (doc)
  (if (listp doc)
      (= 0 (count "Nominalization" doc :test #'string-equal))
      (not (string-equal doc "Nominalization"))))
  
(defun is-synset-id (term)
  "Check if TERM matches the expected behavior of a synset ID."
  (not (null (cl-ppcre:scan "^(\\d+)-[nvar]$" term))))

;;http://lucene.apache.org/core/2_9_4/queryparsersyntax.html#Escaping Special Characters

;; + - && || ! ( ) { } [ ] ^ " ~ * ? : \
(defparameter *lucene-special-chars* "([\\!\\^\\~\\\"\\*\\?\\\\\\+\\:\\(\\)\\{\\}\\[\\]])")

(defun solr-encode (string)
  "Encode SOLR/Lucene special characters"
  (cl-ppcre:regex-replace-all *lucene-special-chars* string '("\\" :match)))

;;; these functions are from ST-JSON, with modifications
;;; Copyright (c) Streamtech & Marijn Haverbeke (marijnh@gmail.com)
;;; from https://github.com/marijnh/ST-JSON/blob/master/st-json.lisp
;;; BEGIN ST-JSON code
(defun getjso (key map)
  (getf map (make-keyword key)))

(defmacro getjso* (keys jso)
  (let ((last (position #\. keys :from-end t)))
    (if last
	`(getjso ,(subseq keys (1+ last))
		 (getjso* ,(subseq keys 0 last) ,jso))
	`(getjso ,keys ,jso))))
;;; END ST-JSON code
