;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defparameter *words-pt-suggestions* nil)
(defparameter *words-pt-wn* nil)
(defparameter *words-en-wn* nil)

(defun cache-words (&key (core-only nil))
  (flet ((get-synset-words (synset)
	   (let ((synset-id (getf synset :|doc_id|)))
	     (dolist (w (get-pt-words synset))
	       (push synset-id (gethash w *words-pt-wn*)))
	     (dolist (w (get-en-words synset))
	       (push synset-id (gethash w *words-en-wn*)))))
	 (get-suggestion-words (suggestions)
	   (dolist (suggestion suggestions)
	     (let ((synset-id (getf suggestion :|doc_id|)))
	       (push synset-id (gethash (getf suggestion :|params|)
					*words-pt-suggestions*))))))
    (setf *words-pt-suggestions* (make-hash-table :test #'equal :size 200000))
    (setf *words-en-wn* (make-hash-table :test #'equal :size 150000))
    (setf *words-pt-wn* (make-hash-table :test #'equal :size 150000))
    (mapc #'get-synset-words (remove-if-not #'verb? (hash-table-values *wn*)))
    (mapc #'get-suggestion-words (hash-table-values *word-suggestions*))
    t))

(defun init-corpora ()
  (cache-words)
  (load-verbnet)
  (load-verbocean)
  (load-portal-da-lingua-portuguesa))
  
(defparameter *portal-da-lingua-portuguesa* nil)
(defparameter *verbnet* nil)
(defparameter *verbocean* nil)

(defun load-verbnet ()
  (setf *verbnet* (make-hash-table :test #'equal :size 15000))
  (fare-csv:with-rfc4180-csv-syntax ()
    (dolist (row (fare-csv:read-csv-file (merge-pathnames "corpora/verbnet.br.csv" *basedir*) :external-format :utf-8))
      (let ((pt (trim (first row)))
	    (en (trim (third row))))
	(push en (gethash pt *verbnet*))))))

(defun load-verbocean ()
  (setf *verbocean* (make-hash-table :test #'equal :size 15000))
  (fare-csv:with-rfc4180-csv-syntax ()
    (dolist (row (fare-csv:read-csv-file (merge-pathnames "corpora/verbocean-translated.csv" *basedir*)  :external-format :utf-8))
      (let ((en (trim (first row)))
	    (pt (trim (second row))))
	(push en (gethash pt *verbocean*))))))

(defun load-portal-da-lingua-portuguesa ()
  (setf *portal-da-lingua-portuguesa* (make-hash-table :test #'equal :size 15000))
  (with-open-file (stream (merge-pathnames "corpora/portal-da-lingua-pt.txt" *basedir*)  :external-format :utf-8)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (setf (gethash (trim line) *portal-da-lingua-portuguesa*) nil))))

(defun get-similar-words (word)
  (append (remove-if-not (lambda (x)
		   (> (mk-string-metrics:norm-damerau-levenshtein x word) .9))
			 (hash-table-keys *words-pt-wn*))
	  (remove-if-not (lambda (x)
			   (> (mk-string-metrics:norm-damerau-levenshtein x word) .9))
			 (hash-table-keys *words-pt-suggestions*))))

(defun get-possible-synsets (pt-word corpus)
  (let ((synsets nil))
    (dolist (en-word (gethash pt-word corpus))
      (dolist (s (gethash en-word *words-en-wn*))
	(push s synsets)))
    synsets))

(defun check-corpus (corpus)
  (let ((result nil))
    (dolist (word (hash-table-keys corpus))
      (when (and (not (gethash word *words-pt-suggestions*))
		 (not (gethash word *words-pt-wn*)))
	(push (list :word word
		    :synsets (get-possible-synsets word corpus))
	      result)))
    result))

(defun check-portal-da-lingua-portuguesa ()
  (check-corpus *portal-da-lingua-portuguesa*))

(defun check-verbnet ()
  (check-corpus *verbnet*))

(defun check-verbocean ()
  (check-corpus *verbocean*))
