(in-package :cl-wnbrowser)

#|Pedir para as pessoas avaliarem frases construídas a partir de
relações sempre me pareceu uma boa forma de termos avaliações. Por
exemplo, se X estiver em um synset for hiperônimo de outro synset com
Y e ambos forem nome, podemos apresentar a frase "X é um tipo de Y." e
perguntar apenas se a frase faz sentido. Se alguém dizer não, pode ser
que X ou Y estejam no synset errado.|#
(defun generate-all-hypernym-phrases ()
  (flet ((all-translated-nouns-with-hypernyms ()
           (get-synset-ids "wn30_hypernymOf:[* TO *] AND word_count_pt:[1 TO *]" (make-drilldown :rdf-type '("NounSynset")) "0" "2000000"))
         (is-noun (s)
           (member "NounSynset" (getf s :|rdf_type|) :test #'equal))
         (has-pt-words (s)
           (getf s :|word_pt|))
         (first-pt-word (s)
           (car (getf s :|word_pt|)))
         (hypernyms (s)
           (getf s :|wn30_hypernymOf|)))
  
  (dolist (s1 (mapcar #'get-synset (all-translated-nouns-with-hypernyms)))
    (dolist (s2 (mapcar #'get-synset (hypernyms s1)))
      (when (and (is-noun s2) (has-pt-words s2))
        (let ((w1 (first-pt-word s1))
              (w2 (first-pt-word s2)))
          (when (not (string-equal w1 w2))
              (format t "~a é um tipo de ~a?~%" w2 w1))))))))
