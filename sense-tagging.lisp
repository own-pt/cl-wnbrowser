;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(in-package :cl-wnbrowser)

(defun get-sense-tagging-suggestion (file text word userid)
  (getf (call-rest-method
         "get-sense-tagging-suggestion"
         :parameters (list (cons "file" file)
                           (cons "text" text)
                           (cons "word" word)
                           (cons "userid" userid)))
        :|selection|))
  
  
(defun add-sense-tagging-suggestion (file text word userid selection comment)
  (call-rest-method
   "sense-tagging-process-suggestion"
   :parameters (list (cons "file" file)
                     (cons "text" text)
                     (cons "word" word)
                     (cons "userid" userid)
                     (cons "selection" selection)
                     (cons "comment" comment))))
