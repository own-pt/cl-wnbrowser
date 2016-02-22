;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

;;; these are the relations used to build the directed graph
(defparameter *relations* '(
                            :|wn30_classifiedByRegion|
                            :|wn30_classifiedByTopic|
                            :|wn30_classifiedByUsage|
                            :|wn30_classifiesByRegion|
                            :|wn30_classifiesByTopic|
                            :|wn30_classifiesByUsage|
                            :|wn30_seeAlso|
                            :|wn30_hasInstance|
                            :|wn30_instanceOf|
                            :|wn30_entails|
                            :|wn30_similarTo|
                            :|wn30_attribute|
                            :|wn30_causes|
                            :|wn30_hypernymOf|
                            :|wn30_hyponymOf|
                            :|wn30_memberHolonymOf|
                            :|wn30_memberMeronymOf|
                            :|wn30_partHolonymOf|
                            :|wn30_partMeronymOf|
                            :|wn30_sameVerbGroupAs|
                            :|wn30_substanceHolonymOf|
                            :|wn30_substanceMeronymOf|))

(defun all-relations (id)
  "Return all directed edges spanning from vertex."
  (mapcan (lambda (r)
            (copy-list (getf (get-cached-document id) r))) *relations*))

(defun selected-relations (id relations)
  "Return all directed edges spanning from vertex."
  (mapcan (lambda (r)
            (copy-list (getf (get-cached-document id) r))) relations))

(defun supersenseOf (id)
  (selected-relations id '(:|wn30_hypernymOf| :|wn30_hasInstance|)))

(defun subsenseOf (id)
  (selected-relations id '(:|wn30_hyponymOf| :|wn30_instanceOf|)))

(defun all-bidirectional-relations (id)
  "Return all bidirectional edges spanning from vertex.  Meaning, if
  there is an edge from ID to ID' but not from ID' to ID, then ID' is
  not included in the result."
  (remove-if (lambda (r)
               (not (member id (all-relations r) :test #'equal)))
             (all-relations id)))

(defun source-vertices (vertex-source-fn edge-source-fn)
  "Return all source vertices that aren't isolated"
  (let ((vertices (funcall vertex-source-fn)))
    (multiple-value-bind (in-fn out-fn)
	(degrees vertices edge-source-fn)
      (mapcar (lambda (v)
		(list :id v
		      :gloss (car (getf (get-cached-document v) :|gloss_en|))))
	      (remove-if-not (lambda (v)
			       (and 
				(> (funcall out-fn v) 0)
				(= (funcall in-fn v) 0)))
			     vertices)))))

(defun isolated-vertices ()
  (let ((vertices (get-all-cached-ids))
        (result nil))
    (multiple-value-bind (in-fn out-fn)
        (degrees vertices  #'all-relations)
      (dolist (v vertices)
        (let ((in (funcall in-fn v))
              (out (funcall out-fn v)))
          (when (= 0 (+ in out))
            (let* ((synset (get-cached-document v))
                   (gloss (car (getf synset :|gloss_en|))))
              (push (list :id v :gloss gloss) result))))))
    result))

(defun search-paths (w1 w2 &key mode relations)
  (when (and w1 w2 mode relations)
    (let ((w1-synsets (if (eq mode :words) (search-word-in-cache w1) (list w1)))
          (w2-synsets (if (eq mode :words) (search-word-in-cache w2) (list w2)))
          (result nil))
      (when (and w1-synsets w2-synsets)
        (dolist (s1 w1-synsets)
          (let ((prev (dijkstra s1 (get-all-cached-ids) (lambda (x) (selected-relations x relations)))))
            (dolist (s2 w2-synsets)
              (let ((path (reconstruct-path prev s2)))
		(when path
		  (push s1 path)
		  (push (list :id1 s1 :id2 s2 :path path) result))))))
        (let* ((sorted-result
                (sort result #'< :key (lambda (x) (length (getf x :path)))))
               (len (length sorted-result)))
          (subseq sorted-result 0 (if (< len 10) len 10)))))))

(defparameter *clique-cache* nil)

(defun find-cliques ()
  (maximal-cliques (get-all-cached-ids) #'all-bidirectional-relations
                   (lambda (c)
                     (when (> (length c) 2)
                       (push c *clique-cache*)))))

(defun source-senses (type)
  (source-vertices (lambda () (get-all-cached-ids-by-type type)) #'supersenseOf))

;; (source-vertices #'get-all-noun-ids #'supersenseOf)
;; (source-vertices #'get-all-verb-ids #'supersenseOf)
