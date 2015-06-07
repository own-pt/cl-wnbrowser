;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

#|Pedir para as pessoas avaliarem frases construídas a partir de
relações sempre me pareceu uma boa forma de termos avaliações. Por
exemplo, se X estiver em um synset for hiperônimo de outro synset com
Y e ambos forem nome, podemos apresentar a frase "X é um tipo de Y." e
perguntar apenas se a frase faz sentido. Se alguém dizer não, pode ser
que X ou Y estejam no synset errado.|#

(defun all-translated-nouns-with-hypernyms ()
  (let ((result nil))
    (maphash
     (lambda (key val)
       (let ((hypernyms (getf val :|wn30_hypernymOf|))
             (word-count-pt (getf val :|word_count_pt|)))
         (when (and hypernyms (> word-count-pt 0))
           (push key result))))
     *wn*)
    result))

(defun generate-all-hypernym-phrases ()
  (let ((result nil))
    (flet ((is-noun (s)
             (member "NounSynset" (getf s :|rdf_type|) :test #'equal))
           (has-pt-words (s)
             (getf s :|word_pt|))
           (first-pt-word (s)
             (car (getf s :|word_pt|)))
           (hypernyms (s)
             (getf s :|wn30_hypernymOf|)))
      
      (dolist (s1 (mapcar #'get-cached-document (all-translated-nouns-with-hypernyms)))
        (dolist (s2 (mapcar #'get-cached-document (hypernyms s1)))
          (when (and (is-noun s2) (has-pt-words s2))
            (let ((w1 (first-pt-word s1))
                  (w2 (first-pt-word s2)))
              (when (not (string-equal w1 w2))
                (push (list :id1 (getf s1 :|doc_id|)
                            :id2 (getf s2 :|doc_id|)
                            :w1 w1 :w2 w2) result)))))))
    result))
