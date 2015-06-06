(in-package :cl-wnbrowser)

(defun get-synsets-from-api (&key rdf-type)
  (get-synset-ids "*" (make-drilldown :rdf-type rdf-type)
                  "0" "2000000"))

(defun process-rule (id test-fn)
  (let ((result nil))
    (dolist (s (get-cached-suggestions id))
      (when (>= (getf s :|sum_votes|) 0)
        (let* ((gloss (getf s :|params|))
               (split-gloss
                (split-sequence-if
                 (lambda (c) (member c '(#\' #\, #\. #\; #\- #\" #\Space)))
                 gloss :remove-empty-subseqs t)))
          (when (funcall test-fn split-gloss)
            (push (list :synset id :pt gloss) result)))))
    result))

;;; 1) todas as glosas de VERBOS que não tem palavras terminas por -ar,
;;; -er, -ir ou -or devem ser consideradas lixo;
(defun process-rule-1 (id)
  (process-rule id
                (lambda (split-gloss)
                  (= 0 
                     (count-if (lambda (w)
                                 (or (ends-with-subseq "ar" w)
                                     (ends-with-subseq "er" w)
                                     (ends-with-subseq "ir" w)
                                     (ends-with-subseq "ôr" w)
                                     (ends-with-subseq "or" w))) split-gloss)))))

;;; 2) todas as glosas de ADJETIVOS com palavras terminadas em (-ar, -er,
;;; -ir, -or) E (uma palavra terminada em "-os") devem ser consideradas
;;; lixo.
(defun process-rule-2 (id)
  (process-rule id
                (lambda (split-gloss)
                  (and (=
                        (count-if (lambda (w) (ends-with-subseq "os" w))
                                  split-gloss) 1)
                       (>=
                        (count-if (lambda (w)
                                    (or (ends-with-subseq "er" w)
                                        (ends-with-subseq "ar" w)
                                        (ends-with-subseq "ir" w)
                                        (ends-with-subseq "or" w)))
                                  split-gloss) 1)))))

;;; 3) todas as glosas de ADJETIVOS cuja primeira palavra for terminada em
;;; -a ou -os devem ser consideradas lixo.
(defun process-rule-3 (id)
  (process-rule id
                (lambda (split-gloss)
                  (let ((first (car split-gloss)))
                    (or (ends-with-subseq "a" first)
                        (ends-with-subseq "os" first))))))

;;; 4)todas as glosas de ADJETIVOS cuja primeira palavra for 'não' e a
;;; segunda palavra for termina em -a ou -os, devem ser consideradas lixo.
(defun process-rule-4 (id)
  (process-rule id
                (lambda (split-gloss)
                  (if (> (length split-gloss) 1)
                      (let ((first (first split-gloss))
                            (second (second split-gloss)))
                        (and (string-equal "não" first)
                             (or (ends-with-subseq "a" second)
                                 (ends-with-subseq "os" second))))
                      nil))))

(defun evaluate-glosses ()
  (list
   :rule1
    (mapcan #'process-rule-1 
            (get-synsets-from-api :rdf-type '("VerbSynset")))
   :rule2
    (mapcan #'process-rule-2
            (get-synsets-from-api :rdf-type '("AdjectiveSynset")))
   :rule3
    (mapcan #'process-rule-3 
            (get-synsets-from-api :rdf-type '("AdjectiveSynset")))
   :rule4
    (mapcan #'process-rule-4 
            (get-synsets-from-api :rdf-type '("AdjectiveSynset")))))
