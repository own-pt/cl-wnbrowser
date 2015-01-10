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

