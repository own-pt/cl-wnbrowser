;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defparameter *words-suggestions* nil)
(defparameter *words-wn* nil)

;; add check synset-type

(defun cache-words (&key (core-only nil))
  (flet ((extract-words (k v)
           ))
    (setf *words-suggestions* (make-hash-table :test #'equal :size 200000))
    (setf *words-wn* (make-hash-table :test #'equal :size 150000))
    (maphash #'extract-words *wn*)))
  
