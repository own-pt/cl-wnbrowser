;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.


(defpackage #:cl-wnbrowser
  (:use :cl :closure-template :split-sequence
        :graph-algorithms
        :alexandria :parse-number)
  (:import-from :cl-fad :merge-pathnames-as-file
		:merge-pathnames-as-directory :walk-directory))
