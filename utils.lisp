;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defun trim (s)
  (string-trim '(#\Space #\Return #\Tab #\Newline) s))

(defun is-synset (doc)
  (if (listp doc)
      (= 0 (count "Nominalization" doc :test #'string-equal))
      (not (string-equal doc "Nominalization"))))
  
(defun is-synset-id (term)
  "Check if TERM matches the expected behavior of a synset ID."
  (not (null (cl-ppcre:scan "^(\\d{8})-[nvar]$" 
                            (trim term)))))

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
