;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun pretty-print-iso-date (ms-since-epoch)
  (smart-date
   (local-time:timestamp-to-universal
    (local-time:unix-to-timestamp (floor ms-since-epoch 1000)))))

;;; from https://github.com/smanek/common-lisp-utils/blob/master/date.lisp
(defun smart-date (date)
  (let ((diff (- (get-universal-time) date)))
    (cond ((< diff -604800) (format nil "~A week~:p from now" (floor (/ diff -604800))))
          ((< diff -86400) (format nil "~A day~:p from now" (floor (/ diff -86400))))
          ((< diff -3600) (format nil "~A hour~:p from now" (floor (/ diff -3600))))
          ((< diff -60) (format nil "~A minute~:p from now" (floor (/ diff -60))))
          ((< diff 0) (format nil "~A second~:p from now" (- 0 diff)))
          ((< diff 60) (format nil "~A second~:p ago" diff))
          ((< diff 3600) (format nil "~A minute~:p ago" (floor (/ diff 60))))
          ((< diff 86400) (format nil "~A hour~:p ago" (floor (/ diff 3600))))
          (t (format nil "~A day~:p ago" (floor (/ diff 86400)))))))

(defun trim-comment(s &optional (max-length 68))
  (if (< (length s) max-length)
      s
      (format nil "~a (...)" (subseq s 0 max-length))))

(defun get-vote-id (user votes)
  (getf (car (member user votes :test #'(lambda (a b)
                                          (equal (getf b :|user|) a)))) :|id|))

(defun already-voted (user votes)
  (member user votes :test #'(lambda (a b)
                               (equal (getf b :|user|) a))))

(defun is-self (logged user)
  (if (and logged user)
      (string-equal logged user)
      nil))

(defun is-authorized (user)
  (find user *authorized-accounts* :test #'string-equal))

(defun valid-status (status)
  (not (string-equal status "committed")))

(defun get-doc-id (doc)
  (getf doc :|_id|))

(defun checked (term list)
  "Helper function to be called from the TEMPLATE code.  It returns
CHECKED in case TERM belongs to LISP; nil otherwise.  This is useful
in dealing with checkboxes."
  (if (find term list :test #'string=)
      "checked"
      nil))

(defun is-array (obj)
  (listp obj))

;;; 1 is for nouns, 2 for verbs, 3 for adjectives and 4 for adverbs.
;;; 00449295-n
;;; 0123456789
(defun synset-id-to-sumo (synset-id)
  (let ((id (subseq synset-id 0 8))
        (type (subseq synset-id 9 10))
        (sumo-type "?"))
    (cond ((equal "n" type) (setq sumo-type "1"))
          ((equal "v" type) (setq sumo-type "2"))
          ((equal "a" type) (setq sumo-type "3"))
          ((equal "r" type) (setq sumo-type "4")))
    (format nil "~a~a" sumo-type id)))

(defun extract-links (txt index userid)
  (let ((tmpl "<a target=\"sense-tagging-details\" href=\"sense-tagging-details?file=bosque.json&text=~a&word=~a&userid=~a\">~a</a>")
	(offsets (mapcar (lambda (w) (getf w :|offset|))
                         (getf txt :|words|)))
        (tags  (mapcar (lambda (w) (getf w :|tag|))
                       (getf txt :|words|))))
    (labels ((convert (txt offsets tags idx ref out)
	       (if (null offsets)
		   (format nil "~{~a~}" (reverse (cons txt out)))
		   (let* ((offset (car offsets))
                          (tag (car tags))
			  (start (- (car offset) ref))
			  (end (- (cadr offset) ref))
			  (lnk
                           (if (alexandria:starts-with-subseq "NC" tag)
                               (format nil tmpl index idx userid (subseq txt start end))
                               (subseq txt start end))))
		     (convert (subseq txt end)
			      (cdr offsets)
                              (cdr tags)
			      (1+ idx)
			      (cadr offset)
			      (cons lnk (cons (subseq txt 0 start) out)))))))
      (convert (getf txt :|text|) offsets tags 0 0 nil))))

(defun setup-templates ()
  (closure-template:with-user-functions
      (("issynset" #'is-synset)
       ("alreadyvoted" #'already-voted)
       ("getvoteid" #'get-vote-id)
       ("extractlinks" #'extract-links)
       ("urlencode" #'hunchentoot:url-encode)
       ("synsetidtosumo" #'synset-id-to-sumo)
       ("trimcomment" #'trim-comment)
       ("getdocid" #'get-doc-id)
       ("validstatus" #'valid-status)
       ("isauthorized" #'is-authorized)
       ("isself" #'is-self)
       ("prettydate" #'pretty-print-iso-date)
       ("solrencode" #'solr-encode)
       ("checked" #'checked)
       ("isarray" #'is-array)
       ("synsetworden" #'get-synset-word-en))
    (walk-directory
     (merge-pathnames *templates-directory* *basedir*)
     (lambda (f)
       (closure-template:compile-template
	:common-lisp-backend
	(read-file-into-string f)))
     :test (lambda (f)
             (alexandria:ends-with-subseq ".tmpl" (namestring f))))))


(setup-templates)

