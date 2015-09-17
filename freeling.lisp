;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defparameter *fod* "/home/fcbr/fod.py")

(defun make-freeling-call (filename-in filename-out)
  (format nil "~a ~a ~a" *fod* filename-in filename-out))

(defun call-freeling (text)
  (let* ((tmp-in (cl-fad:with-output-to-temporary-file (foo)
                (format foo "~a" text)))
         (tmp-out (format nil "~a.out" tmp-in)))
    (trivial-shell:shell-command (make-freeling-call tmp-in tmp-out))
    (let ((obj (yason:parse (pathname tmp-out)
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      obj)))
      
    
