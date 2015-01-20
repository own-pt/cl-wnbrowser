;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun checked (term list)
  "Helper function to be called from the TEMPLATE code.  It returns
CHECKED in case TERM belongs to LISP; nil otherwise.  This is useful
in dealing with checkboxes."
  (if (find term list :test #'string=)
      "checked"
      nil))

(defun setup-templates ()
  (closure-template:with-user-functions
      (("issynset" #'is-synset)
       ("solrencode" #'solr-encode)
       ("checked" #'checked)
       ("getrelatednomlexes" #'get-related-nomlexes)
       ("getrelatedsynsets" #'get-related-synsets)
       ("synsetworden" #'get-synset-word-en))
    (walk-directory
     (merge-pathnames *templates-directory* *basedir*)
     (lambda (f)
       (closure-template:compile-template
	:common-lisp-backend
	(read-file-into-string f))))))

(setup-templates)

