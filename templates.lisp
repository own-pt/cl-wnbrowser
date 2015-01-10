;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun setup-templates ()
  (closure-template:with-user-functions
      (("issynset" #'is-synset)
       ("getrelatedsynsets" #'get-related-synsets)
       ("synsetworden" #'get-synset-word-en))
    (walk-directory
     (merge-pathnames *templates-directory* *basedir*)
     (lambda (f)
       (closure-template:compile-template
	:common-lisp-backend
	(read-file-into-string f))))))

(setup-templates)

