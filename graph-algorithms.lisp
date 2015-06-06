(in-package :cl-wnbrowser)

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
  (mapcan (lambda (r) (getf (get-cached-document id) r)) *relations*))

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

#|
(defun dijkstra (w1 w2)
  (let ((w1-synsets (search-word-in-cache w1))
        (w2-synsets (search-word-in-cache w2))
        (smallest-path-size MOST-POSITIVE-FIXNUM)
        (smallest-path nil)
        (smallest-path-s1 nil)
        (smallest-path-s2 nil))
    (format t "W1 (~a) ~a synsets~%" w1 (length w1-synsets))
    (format t "W2 (~a) ~a synsets~%" w2 (length w2-synsets))
    (dolist (b w1-synsets)
      (multiple-value-bind (dist prev)
          (dijkstra b (all-ids) #'all-relations)
        (dolist (f w2-synsets)
          (let ((path (reconstruct-path prev f)))
            (when (and path (< (length path) smallest-path-size))
              (setf smallest-path-size (length path))
              (setf smallest-path path)
              (setf smallest-path-s1 b)
              (setf smallest-path-s2 f))))))
    (format t "[~a] -> [~a]~%" (gloss-en1 smallest-path-s1) (gloss-en1 smallest-path-s2))
    (format t "Path = ~a~%" (mapcar #'word-en1 smallest-path))))
|#
