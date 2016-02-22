;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
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
	       (push synset-id (gethash (trim (getf suggestion :|params|))
					*words-pt-suggestions*))))))
    (setf *words-pt-suggestions* (make-hash-table :test #'equal :size 200000))
    (setf *words-en-wn* (make-hash-table :test #'equal :size 150000))
    (setf *words-pt-wn* (make-hash-table :test #'equal :size 150000))
    (mapc #'get-synset-words (hash-table-values *wn*))
    (mapc #'get-suggestion-words (hash-table-values *word-suggestions*))
    t))

(defun init-corpora ()
  (cache-words)
  (load-dizer)
  (load-verbnet)
  (load-synset-candidates)
  (load-thousand-common-verbs)
  (load-verbnet-gold)
  (load-dhbb)
  (load-swadesh)
  (load-intersection)
  (load-portal-alta-freq)
  (load-propbank)
  (load-propbank-translated)
  (load-verbocean)
  (load-pt-ud)
  (load-pt-ud-cleaned)
  (load-verbos-dg)
  (load-verbos-dg-cleaned)
  (load-nomlex-floating)
  (load-nomlex-floating-translated)
  (load-compex)
  (load-portal-da-lingua-portuguesa))

(defparameter *compex* nil)
(defparameter *portal-alta-freq* nil)  
(defparameter *portal-da-lingua-portuguesa* nil)
(defparameter *verbnet* nil)
(defparameter *synset-candidates* nil)
(defparameter *propbank* nil)
(defparameter *propbank-translated* nil)
(defparameter *verbnet-gold* nil)
(defparameter *verbocean* nil)
(defparameter *dhbb* nil)
(defparameter *dizer* nil)
(defparameter *swadesh* nil)
(defparameter *dhbb-stats* nil)
(defparameter *intersection* nil)
(defparameter *thousand-common-verbs* nil)
(defparameter *pt-ud-verb* nil)
(defparameter *pt-ud-verb-stats* nil)
(defparameter *pt-ud-noun* nil)
(defparameter *pt-ud-noun-stats* nil)
(defparameter *pt-ud-adj* nil)
(defparameter *pt-ud-adj-stats* nil)
(defparameter *pt-ud-adv* nil)
(defparameter *pt-ud-adv-stats* nil)
(defparameter *pt-ud-verb-cleaned* nil)
(defparameter *verbos-dg* nil)
(defparameter *verbos-dg-cleaned* nil)
(defparameter *nomlex-floating* nil)
(defparameter *nomlex-floating-translated* nil)

(defun load-translated-corpus (filename hashtable)
  (fare-csv:with-rfc4180-csv-syntax ()
    (dolist (row (fare-csv:read-csv-file filename :external-format :utf-8))
      (let ((pt (trim (string-downcase (first row))))
	    (en (trim (string-downcase (second row)))))
        (when (> (length pt) 0)
          (push en (gethash pt hashtable)))))))

(defun load-frequency (filename hashtable)
  (dolist (row (fare-csv:read-csv-file filename :external-format :utf-8))
    (let ((freq (parse-integer (trim (string-downcase (first row)))))
          (word (trim (string-downcase (second row)))))
      (setf (gethash word hashtable) freq))))

(defun load-simple-corpus (filename hashtable)
  (with-open-file (stream filename  :external-format :utf-8)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let ((trimmed-line (trim line)))
        (when (> (length trimmed-line) 0)
          (setf (gethash (string-downcase (trim line)) hashtable) nil))))))

(defun load-dizer ()
  (setf *dizer* (make-hash-table :test #'equal))
  (load-simple-corpus  (merge-pathnames "corpora/DIZER_Bianca_Moura-Neves.txt" *basedir*) *dizer*))

(defun load-verbnet ()
  (setf *verbnet* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus  (merge-pathnames "corpora/verbnet.br.csv" *basedir*) *verbnet*))

(defun load-synset-candidates ()
  (setf *synset-candidates* (make-hash-table :test #'equal :size 100))
  (load-translated-corpus  (merge-pathnames "corpora/synset-candidates.csv" *basedir*) *synset-candidates*))

(defun load-thousand-common-verbs ()
  (setf *thousand-common-verbs* (make-hash-table :test #'equal :size 1000))
  (load-simple-corpus  (merge-pathnames "corpora/1000-verbs.txt" *basedir*) *thousand-common-verbs*))

(defun load-verbnet-gold ()
  (setf *verbnet-gold* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus  (merge-pathnames "corpora/verbnet.br-gold.txt" *basedir*) *verbnet-gold*))

(defun load-portal-alta-freq ()
  (setf *portal-alta-freq* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus  (merge-pathnames "corpora/portal-alta-freq.txt" *basedir*) *portal-alta-freq*))

(defun load-verbocean ()
  (setf *verbocean* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus (merge-pathnames "corpora/verbocean-translated.csv" *basedir*) *verbocean*))

(defun load-swadesh ()
  (setf *swadesh* (make-hash-table :test #'equal :size 320))
  (load-translated-corpus (merge-pathnames "corpora/swadesh.csv" *basedir*) *swadesh*))

(defun load-dhbb ()
  (setf *dhbb* (make-hash-table :test #'equal :size 15000))
  (setf *dhbb-stats* (make-hash-table :test #'equal))
  (load-frequency (merge-pathnames "corpora/dhbb-freq.csv" *basedir*) *dhbb-stats*)
  (load-translated-corpus (merge-pathnames "corpora/verbos-DHBB.csv" *basedir*) *dhbb*))

(defun load-compex ()
  (setf *compex* (make-hash-table :test #'equal :size 500))
  (load-translated-corpus (merge-pathnames "corpora/compex-en-pt.csv" *basedir*) *compex*))

(defun load-portal-da-lingua-portuguesa ()
  (setf *portal-da-lingua-portuguesa* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus (merge-pathnames "corpora/portal-da-lingua-pt.txt" *basedir*) *portal-da-lingua-portuguesa*))

(defun load-pt-ud ()
  (setf *pt-ud-verb* (make-hash-table :test #'equal :size 15000))
  (setf *pt-ud-verb-stats* (make-hash-table :test #'equal :size 15000))

  (setf *pt-ud-noun* (make-hash-table :test #'equal :size 15000))
  (setf *pt-ud-noun-stats* (make-hash-table :test #'equal :size 15000))

  (setf *pt-ud-adv* (make-hash-table :test #'equal :size 15000))
  (setf *pt-ud-adv-stats* (make-hash-table :test #'equal :size 15000))

  (setf *pt-ud-adj* (make-hash-table :test #'equal :size 15000))
  (setf *pt-ud-adj-stats* (make-hash-table :test #'equal :size 15000))

  (load-frequency (merge-pathnames "corpora/pt-ud-VERB-freq.csv" *basedir*) *pt-ud-verb-stats*)
  (load-simple-corpus (merge-pathnames "corpora/pt-ud-VERB.txt" *basedir*) *pt-ud-verb*)

  (load-frequency (merge-pathnames "corpora/pt-ud-ADJ-freq.csv" *basedir*) *pt-ud-adj-stats*)
  (load-simple-corpus (merge-pathnames "corpora/pt-ud-ADJ.txt" *basedir*) *pt-ud-adj*)

  (load-frequency (merge-pathnames "corpora/pt-ud-NOUN-freq.csv" *basedir*) *pt-ud-noun-stats*)
  (load-simple-corpus (merge-pathnames "corpora/pt-ud-NOUN.txt" *basedir*) *pt-ud-noun*)

  (load-frequency (merge-pathnames "corpora/pt-ud-ADV-freq.csv" *basedir*) *pt-ud-adv-stats*)
  (load-simple-corpus (merge-pathnames "corpora/pt-ud-ADV.txt" *basedir*) *pt-ud-adv*))

(defun load-intersection ()
  (setf *intersection* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus (merge-pathnames "corpora/intersection.txt" *basedir*) *intersection*))

(defun load-propbank ()
  (setf *propbank* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus (merge-pathnames "corpora/propbank-todos-os-verbos.txt" *basedir*) *propbank*))

(defun load-propbank-translated ()
  (setf *propbank-translated* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus (merge-pathnames "corpora/propbank-todos-os-verbos.csv" *basedir*) *propbank-translated*))

(defun load-pt-ud-cleaned ()
  (setf *pt-ud-verb-cleaned* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus (merge-pathnames "corpora/pt-ud-VERB-cleaned.csv" *basedir*) *pt-ud-verb-cleaned*))

(defun load-verbos-dg ()
  (setf *verbos-dg* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus (merge-pathnames "corpora/verbos-dg.txt" *basedir*) *verbos-dg*))

(defun load-verbos-dg-cleaned ()
  (setf *verbos-dg-cleaned* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus (merge-pathnames "corpora/verbos-dg-cleaned.csv" *basedir*) *verbos-dg-cleaned*))

(defun load-nomlex-floating ()
  (setf *nomlex-floating* (make-hash-table :test #'equal :size 15000))
  (load-simple-corpus (merge-pathnames "corpora/verbos-nomlex-floating.txt" *basedir*) *nomlex-floating*))

(defun load-nomlex-floating-translated ()
  (setf *nomlex-floating-translated* (make-hash-table :test #'equal :size 15000))
  (load-translated-corpus (merge-pathnames "corpora/verbos-nomlex-floating.csv" *basedir*) *nomlex-floating-translated*))

(defun get-similar-words (word)
  (append (remove-if-not (lambda (x)
		   (> (mk-string-metrics:norm-damerau-levenshtein x word) .9))
			 (hash-table-keys *words-pt-wn*))
	  (remove-if-not (lambda (x)
			   (> (mk-string-metrics:norm-damerau-levenshtein x word) .9))
			 (hash-table-keys *words-pt-suggestions*))))

(defun lf-test (lf)
  (case lf
    (:all #'identity)
    (:verb #'verb?)
    (:adjective #'adj?)
    (:adverb #'adv?)
    (:noun #'noun?)))

(defun get-possible-synsets (pt-word corpus lf)
  (let ((synsets nil)
	(test (lf-test lf)))
    (dolist (en-word (gethash pt-word corpus))
      (dolist (s (remove-if-not test (gethash en-word *words-en-wn*)))
	(push s synsets)))
    synsets))

(defun test-word (word lf)
  (let ((synsets (gethash word *words-pt-wn*))
	(test (lf-test lf)))
    (not (null (remove-if-not test synsets)))))
    
(defun check-corpus (corpus &key stats lf)
  (when corpus
    (let ((result nil)
          (total 0)
          (in-wn 0)
          (in-suggestions 0))
      (dolist (word (hash-table-keys corpus))
        (incf total)
        (if (gethash word *words-pt-suggestions*)
            (incf in-suggestions)
            (if (test-word word lf)
                (incf in-wn)
                (let ((word-info (list :word word :synsets (get-possible-synsets word corpus lf))))
                  (when stats
                    (setf word-info 
                          (append (list :count (gethash word stats 0)) word-info)))
                  (push word-info result)))))
      (when stats
        (setf result (sort result #'> :key (lambda (x) (getf x :count)))))
      (list :total total :totalwn in-wn :totalsuggestions in-suggestions :words result))))

(defun check-portal-da-lingua-portuguesa ()
  (check-corpus *portal-da-lingua-portuguesa* :lf :verb))

(defun check-dizer ()
  (check-corpus *dizer* :lf :verb))

(defun check-verbnet ()
  (check-corpus *verbnet* :lf :verb))

(defun check-synset-candidates ()
  (check-corpus *synset-candidates* :lf :verb))

(defun check-thousand-common-verbs ()
  (check-corpus *thousand-common-verbs* :lf :verb))

(defun check-verbnet-gold ()
  (check-corpus *verbnet-gold* :lf :verb))

(defun check-dhbb ()
  (check-corpus *dhbb* :stats *dhbb-stats* :lf :verb))

(defun check-compex ()
  (check-corpus *compex* :lf :verb))

(defun check-verbocean ()
  (check-corpus *verbocean* :lf :verb))

(defun check-swadesh ()
  (check-corpus *swadesh* :lf :all))

(defun check-pt-ud-verb ()
  (check-corpus *pt-ud-verb* :stats *pt-ud-verb-stats* :lf :verb))

(defun check-pt-ud-noun ()
  (check-corpus *pt-ud-noun* :stats *pt-ud-noun-stats* :lf :noun))

(defun check-pt-ud-adj ()
  (check-corpus *pt-ud-adj* :stats *pt-ud-adj-stats* :lf :adjective))

(defun check-pt-ud-adv ()
  (check-corpus *pt-ud-adv* :stats *pt-ud-adv-stats* :lf :adverb))

(defun check-intersection ()
  (check-corpus *intersection* :lf :verb))

(defun check-propbank ()
  (check-corpus *propbank* :lf :verb))

(defun check-propbank-translated ()
  (check-corpus *propbank-translated* :lf :verb))

(defun check-pt-ud-cleaned ()
  (check-corpus *pt-ud-verb-cleaned* :stats *pt-ud-verb-stats* :lf :verb))

(defun check-verbos-dg ()
  (check-corpus *verbos-dg* :lf :verb))

(defun check-verbos-dg-cleaned ()
  (check-corpus *verbos-dg-cleaned* :lf :verb))

(defun check-nomlex-floating ()
  (check-corpus *nomlex-floating* :lf :verb))

(defun check-portal-alta-freq ()
  (check-corpus *portal-alta-freq* :lf :verb))

(defun check-nomlex-floating-translated ()
  (check-corpus *nomlex-floating-translated* :lf :verb))
