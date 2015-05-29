(in-package :cl-wnbrowser)

#|Pedir para as pessoas avaliarem frases construídas a partir de
relações sempre me pareceu uma boa forma de termos avaliações. Por
exemplo, se X estiver em um synset for hiperônimo de outro synset com
Y e ambos forem nome, podemos apresentar a frase "X é um tipo de Y." e
perguntar apenas se a frase faz sentido. Se alguém dizer não, pode ser
que X ou Y estejam no synset errado.|#
(defun fill-hypernyms ()
  (dolist (id (get-synset-ids "wn30_hypernymOf:[* TO *] AND word_count_pt:[1 TO *]" (make-drilldown :rdf-type '("NounSynset")) "0" "2000000"))
    (let* ((synset (get-synset id))
           (hypernyms (getf synset :|wn30_hypernymOf|)))
      (when hypernyms
        (dolist (h hypernyms)
          (let* ((h-synset (get-synset h))
                 (rdf-type (getf h-synset :|rdf_type|))
                 (h-word-pt (getf h-synset :|word_pt|)))
            (when (and (member "NounSynset" rdf-type :test #'equal)
                       h-word-pt)
              (format t "~a é um tipo de ~a?~%"
                      (car h-word-pt)
                      (car (getf synset :|word_pt|))))))))))
