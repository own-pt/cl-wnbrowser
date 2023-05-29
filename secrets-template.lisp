;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(in-package :cl-wnbrowser)

;;; you need to update this with the base fully qualified domain name
;;; for your website.  In the OpenWN-PT case, since the website is
;;; available at http://wnpt.brlcloud.com/wn/, this value should be
;;; set to "wnpt.brlcloud.com".
(defparameter *base-url* "<insert FQDN for website>")

;;; configure your github application with the proper URLs and put the
;;; appropriate values here.  The callback URL must end on
;;; /wn/callback.
;;; for production: github own-pt organization > seetings > oauth app 
(defparameter *github-client-id* "<insert github client id>")
(defparameter *github-client-secret* "<insert github client secret>")

;;; these are accounts that are allowed to approve or reject
;;; suggestions regardless of their vote.
(defparameter *approve-reject-authorized-accounts*
  '("githubuser1" "githubuser2" "..."))

;;; these are the accounts that can vote.
(defparameter *vote-authorized-accounts*
  '("githubuser3" "githubuser4" "..."))

;; databases for Elasticsearch-NLP service in the IBM Cloud (RIS-BRL)
(defparameter *password* "<PASSWORD>")
(defparameter *user* "admin")
(setf clesc::*es-endpoint* "<URL>")

(setf clesc::*debug-query-dsl* t)
(setf clesc::*es-authentication* (list *user* *password*))
