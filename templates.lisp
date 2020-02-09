;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

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

(defun nice-action (action)
  (cond ((string-equal "add-gloss-pt" action) "+gl")
        ((string-equal "remove-gloss-pt" action) "-gl")
        ((string-equal "add-word-pt" action) "+w")
        ((string-equal "remove-word-pt" action) "-w")
        ((string-equal "add-example-pt" action) "+ex")
        ((string-equal "remove-example-pt" action) "-ex")
        (t "??")))

(defun is-self (logged user)
  (if (and logged user)
      (string-equal logged user)
      nil))

(defun is-authorized-to-approve/reject (user)
  (member user *approve-reject-authorized-accounts* :test #'string-equal))

(defun is-authorized-to-vote (user)
  (member user *vote-authorized-accounts* :test #'string-equal))

(defun valid-status (status)
  (not (string-equal status "committed")))

(defun get-doc-id (doc)
  (getf doc :|id|))

(defun is-member (term list)
  (if (member term list :test #'string-equal)
      t
      nil))

(defun checked (term list &key (test #'string=))
  "Helper function to be called from the TEMPLATE code.  It returns
CHECKED in case TERM belongs to LISP; nil otherwise.  This is useful
in dealing with checkboxes."
  (if (member term list :test test)
      "checked"
      nil))

(defun is-array (obj)
  (listp obj))

;;; 1 is for nouns, 2 for verbs, 3 for adjectives and 4 for adverbs.
;;; 00449295-n
;;; 0123456789
(defun synset-id-to-sumo (synset-id)
  (when synset-id 
    (let ((id (subseq synset-id 0 8))
          (type (subseq synset-id 9 10))
          (sumo-type "?"))
      (cond ((equal "n" type) (setq sumo-type "1"))
            ((equal "v" type) (setq sumo-type "2"))
            ((equal "a" type) (setq sumo-type "3"))
            ((equal "r" type) (setq sumo-type "4")))
      (format nil "~a~a" sumo-type id))))

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

(defun in-suggestions (entry suggestions action)
  (let ((trimmed-entry (trim entry))
        (entries 
         (mapcan (lambda (s)
                   (when
                       (and
                        (not (string-equal "not-accepted" (getf s :|status|)))
                        (string-equal action (getf s :|action|)))
                       (list (trim (getf s :|params|)))))
                 suggestions)))
    (member trimmed-entry entries :test #'string=)))

(defun convert-tag (str)
  "Converts from the format stored in SOLR to a more human readable version."
  (cond ((starts-with-subseq "HASH" str :test #'string=)
         (format nil "#~a" (subseq str 4)))
        ((starts-with-subseq "AT" str :test #'string=)
         (format nil "@~a" (subseq str 2)))
        (t str)))

(defun get-sense-synset (s)
  (car (split-sequence #\: s)))

(defun get-sense-weight (s)
  (cadr (split-sequence #\: s)))

(defun is-synset-rel (rels)
  (remove-if (lambda (x) (getf x :|source_word|))
	     rels))


(defparameter *rel-symbols* (alexandria:plist-hash-table '(:|wn30_en_antonymOf|	"ant"
						:|wn30_en_causes|	"cause"
						:|wn30_en_classifiedByRegion|	"mr"
						:|wn30_en_classifiedByTopic| "mt"
						:|wn30_en_classifiedByUsage|	"mu"
						:|wn30_en_classifiesByRegion|	"dr"
						:|wn30_en_classifiesByTopic|	"dt"
						:|wn30_en_classifiesByUsage|	"du"
						:|wn30_en_derivationallyRelated|	"drf"
						:|wn30_en_participleOf|	"pv"
						:|wn30_en_pertainsTo|	"pe"
						:|wn30_en_property|	"prp"
						:|wn30_en_sameVerbGroupAs|	"vg"
						:|wn30_en_seeAlso|	"see") :test #'equal))

(defun aux-filter-sense-rel (rel links word)
  (loop for link in links
     when (equal (getf link :|source_word|) word)
     collect (cons :|symbol|
		   (cons (gethash rel *rel-symbols*)
				   link))))

(defun filter-sense-rel (synset word)

  (let ((relations '(:|wn30_en_classifiedByRegion| :|wn30_en_classifiedByTopic|
		     :|wn30_en_classifiedByUsage| :|wn30_en_classifiesByRegion|
		     :|wn30_en_classifiesByTopic| :|wn30_en_classifiesByUsage|
		     :|wn30_en_sameVerbGroupAs| :|wn30_en_seeAlso|
		     :|wn30_en_derivationallyRelated| :|wn30_en_antonymOf|
		     :|wn30_en_pertainsTo| :|wn30_en_property|
		     :|wn30_en_participleOf| :|wn30_en_causes|)))
    (loop for rel in relations
       append (aux-filter-sense-rel rel (getf synset rel) word))))

(defun setup-templates ()
  (closure-template:with-user-functions
      (("issynset" #'is-synset)
       ("niceaction" #'nice-action)
       ("alreadyvoted" #'already-voted)
       ("sensesynset" #'get-sense-synset)
       ("senseweight" #'get-sense-weight)
       ("getvoteid" #'get-vote-id)
       ("insuggestions" #'in-suggestions)
       ("extractlinks" #'extract-links)
       ("urlencode" #'hunchentoot:url-encode)
       ("synsetidtosumo" #'synset-id-to-sumo)
       ("trimcomment" #'trim-comment)
       ("getdocid" #'get-doc-id)
       ("converttag" #'convert-tag)
       ("validstatus" #'valid-status)
       ("isauthorizedapprove" #'is-authorized-to-approve/reject)
       ("isauthorizedvote" #'is-authorized-to-vote)
       ("isself" #'is-self)
       ("prettydate" #'pretty-print-iso-date)
       ("checked" #'checked)
       ("nchecked" (lambda (term list) (checked term list :test #'eq)))
       ("isarray" #'is-array)
       ("ismember" #'is-member)
       ("synsetworden" #'get-synset-word-en)
       ("synsetword" #'get-synset-word)
       ("synsetgloss" #'get-synset-gloss)
       ("synsetrel" #'is-synset-rel)
       ("wordrel" #'filter-sense-rel))
    (walk-directory
     (merge-pathnames *templates-directory* *basedir*)
     (lambda (f)
       (closure-template:compile-template
	:common-lisp-backend
	(read-file-into-string f)))
     :test (lambda (f)
             (alexandria:ends-with-subseq ".tmpl" (namestring f))))))


(setup-templates)

