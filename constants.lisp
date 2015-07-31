;; -*- mode: common-lisp -*-

;; Copyright (c) 2015 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defparameter *breadcrumb-size* 10)

(defparameter *basedir*
  (make-pathname :directory
		 (pathname-directory
		  (asdf:component-pathname (asdf:find-system '#:cl-wnbrowser-bluemix)))))

(defparameter *github-access-token-url* "https://github.com/login/oauth/access_token")

(defparameter *github-user-api* "https://api.github.com/user")

(defparameter *wn30-schema-uri* "http://arademaker.github.com/wn30/schema/")

;;; (defparameter *ownpt-api-uri* "http://ownpt2-dev.mybluemix.net")
;;; (defparameter *ownpt-api-uri* "http://ownpt2.mybluemix.net")
(defparameter *ownpt-api-uri* "http://localhost:3000")

(defparameter *allegro-graph-url*
  "http://logics.emap.fgv.br:10035/repositories")

(defparameter *allegro-graph-repository* "wn30")

(defparameter *queries-directory* #p"queries/")

(defparameter *templates-directory* #p"templates/")

(defparameter *query/by-lexfile*
  (merge-pathnames-as-file *queries-directory* #p"by-lexfile.query"))
(defparameter *query/by-pos-pt*
  (merge-pathnames-as-file *queries-directory* #p"by-pos-pt.query"))
