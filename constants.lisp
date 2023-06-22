;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

(defparameter *breadcrumb-size* 15)

(defparameter *basedir*
  (make-pathname :directory
		 (pathname-directory
		  (asdf:component-pathname (asdf:find-system '#:cl-wnbrowser)))))

(defparameter *github-access-token-url* "https://github.com/login/oauth/access_token")

(defparameter *github-user-api* "https://api.github.com/user")

(defparameter *wn30-schema-uri* "http://arademaker.github.com/wn30/schema/")

;; (defparameter *ownpt-api-uri* "http://127.0.0.1:3000")

(defparameter *queries-directory* #p"queries/")

(defparameter *templates-directory* #p"templates/")

(defparameter *query/by-lexfile*
  (merge-pathnames-as-file *queries-directory* #p"by-lexfile.query"))
(defparameter *query/by-pos-pt*
  (merge-pathnames-as-file *queries-directory* #p"by-pos-pt.query"))

(defparameter *backend* 'es)


;;; you need to update this with the base fully qualified domain name
;;; for your website.  If the website is available at
;;; http://wnpt.brlcloud.com/wn/, this value should be set to
;;; "wnpt.brlcloud.com".
(defparameter *base-url* (uiop:getenv "OWNPT_BASE_URL"))

(defparameter *github-client-id* (uiop:getenv "GITHUB_CLIENT_ID"))
(defparameter *github-client-secret* (uiop:getenv "GITHUB_CLIENT_SECRET"))

(setf clesc::*es-authentication*
      (list (uiop:getenv "ES_USER") (uiop:getenv "ES_PASSWORD")))
(setf clesc::*es-endpoint*
      (uiop:getenv "ES_URL"))

;;; these are accounts that are allowed to approve or reject
;;; suggestions regardless of their vote. The var need the values
;;; separated by colon
(defparameter *approve-reject-authorized-accounts*
  (cl-strings:split (uiop:getenv "OWNPT_ACCOUNTS_ADMIN") ":"))

;;; these are the accounts that can vote.
(defparameter *vote-authorized-accounts*
  (cl-strings:split (uiop:getenv "OWNPT_ACCOUNTS_VOTE") ":"))
