;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(defpackage #:cl-wnbrowser
  (:use :cl :closure-template :split-sequence :alexandria :parse-number)
  (:import-from :cl-fad :merge-pathnames-as-file
		:merge-pathnames-as-directory :walk-directory))
