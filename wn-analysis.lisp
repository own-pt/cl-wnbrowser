(in-package :cl-wnbrowser)

(defun get-synsets-from-api (&key rdf-type)
  (get-synset-ids "*" (make-drilldown :rdf-type rdf-type)
                  "0" "2000000"))

(defun process-rule (id test-fn stream)
  (dolist (s (execute-search
              (format nil "doc_id:~a" id)
              (make-drilldown-activity :action '("add-gloss-pt"))
              "search-activities" "0" "2000000" nil nil))
    (let* ((gloss (getf s :|params|))
           (gloss-en (car (getf (get-synset id) :|gloss_en|)))
           (user (getf s :|user|))
           (prov (getf s :|provenance|))
           (split-gloss
            (split-sequence-if
             (lambda (c) (member c '(#\' #\, #\. #\; #\- #\" #\Space)))
             gloss :remove-empty-subseqs t)))
      (when (funcall test-fn split-gloss)
        (format stream "~a~c~a~c~a~%" id #\Tab gloss #\Tab gloss-en)))))

;;; 1) todas as glosas de VERBOS que não tem palavras terminas por -ar,
;;; -er, -ir ou -or devem ser consideradas lixo;
(defun process-rule-1 (id stream)
  (process-rule id
                (lambda (split-gloss)
                  (= 0 
                     (count-if (lambda (w)
                                 (or (ends-with-subseq "ar" w)
                                     (ends-with-subseq "er" w)
                                     (ends-with-subseq "ir" w)
                                     (ends-with-subseq "or" w))) split-gloss)))
                stream))

;;; 2) todas as glosas de ADJETIVOS com palavras terminadas em (-ar, -er,
;;; -ir, -or) E (uma palavra terminada em "-os") devem ser consideradas
;;; lixo.
(defun process-rule-2 (id stream)
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
                                  split-gloss) 1)))
                stream))

;;; 3) todas as glosas de ADJETIVOS cuja primeira palavra for terminada em
;;; -a ou -os devem ser consideradas lixo.
(defun process-rule-3 (id stream)
  (process-rule id
                (lambda (split-gloss)
                  (let ((first (car split-gloss)))
                    (or (ends-with-subseq "a" first)
                        (ends-with-subseq "os" first))))
                stream))

;;; 4)todas as glosas de ADJETIVOS cuja primeira palavra for 'não' e a
;;; segunda palavra for termina em -a ou -os, devem ser consideradas lixo.
(defun process-rule-4 (id stream)
  (process-rule id
                (lambda (split-gloss)
                  (if (> (length split-gloss) 1)
                      (let ((first (first split-gloss))
                            (second (second split-gloss)))
                        (and (string-equal "não" first)
                             (or (ends-with-subseq "a" second)
                                 (ends-with-subseq "os" second))))
                      nil))
                stream))

(defun run()
  (with-open-file (stream "rule-1.csv" :direction :output :if-exists :supersede)
    (dolist (id (get-synsets-from-api :rdf-type '("VerbSynset")))
      (process-rule-1 id stream))))

  (with-open-file (stream "rule-2.csv" :direction :output :if-exists :supersede)
    (dolist (id (get-synsets-from-api :rdf-type '("AdjectiveSynset")))
      (process-rule-2 id stream))))

  (with-open-file (stream "rule-3.csv" :direction :output :if-exists :supersede)
    (dolist (id (get-synsets-from-api :rdf-type '("AdjectiveSynset")))
      (process-rule-3 id stream))))

  (with-open-file (stream "rule-4.csv" :direction :output :if-exists :supersede)
    (dolist (id (get-synsets-from-api :rdf-type '("AdjectiveSynset")))
      (process-rule-4 id stream))))

