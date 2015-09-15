;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

;;; these are the relations used to build the directed graph
(defparameter *relations* '(
;;                            :|wn30_classifiedByRegion|
;;                            :|wn30_classifiedByTopic|
;;                            :|wn30_classifiedByUsage|
;;                            :|wn30_classifiesByRegion|
;;                            :|wn30_classifiesByTopic|
;;                            :|wn30_classifiesByUsage|
;;                            :|wn30_seeAlso|
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

(defun all-bidirectional-relations (id)
  "Return all bidirectional edges spanning from vertex.  Meaning, if
  there is an edge from ID to ID' but not from ID' to ID, then ID' is
  not included in the result."
  (remove-if (lambda (r)
               (not (member id (all-relations r) :test #'equal)))
             (all-relations id)))

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

(defun search-paths (w1 w2)
  (when (and w1 w2)
    (let ((w1-synsets (search-word-in-cache w1))
          (w2-synsets (search-word-in-cache w2))
          (result nil))
      (when (and w1-synsets w2-synsets)
        (dolist (s1 w1-synsets)
          (let ((prev (dijkstra s1 (get-all-cached-ids) #'all-relations)))
            (dolist (s2 w2-synsets)
              (let ((path (reconstruct-path prev s2)))
<<<<<<< HEAD
		(when path
		  (push s1 path)
		  (push (list :id1 s1 :id2 s2 :path path) result))))))
=======
                (when path
                  (push s1 path)
                  (push (list :id1 s1 :id2 s2 :path path) result))))))
>>>>>>> d0a19d1c7c913da6e9fd603e3d3db17341e6bfc3
        (let* ((sorted-result
                (sort result #'< :key (lambda (x) (length (getf x :path)))))
               (len (length sorted-result)))
          (subseq sorted-result 0 (if (< len 10) len 10)))))))

(defun search-paths-synsets (s1 s2)
  (let ((w1-synsets (list s1))
        (w2-synsets (list s2))
        (result nil))
    (when (and w1-synsets w2-synsets)
      (dolist (s1 w1-synsets)
        (let ((prev (dijkstra s1 (get-all-cached-ids) #'all-relations)))
          (dolist (s2 w2-synsets)
            (let ((path (reconstruct-path prev s2)))
              (when path
                (push s1 path)
                (push (list :id1 s1 :id2 s2 :path path) result))))))
      (let* ((sorted-result
              (sort result #'< :key (lambda (x) (length (getf x :path)))))
             (len (length sorted-result)))
        (subseq sorted-result 0 (if (< len 10) len 10))))))

(defparameter *clique-cache* nil)

(defun find-cliques ()
  (maximal-cliques (get-all-cached-ids) #'all-bidirectional-relations
                   (lambda (c)
                     (when (> (length c) 2)
                       (push c *clique-cache*)))))
