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

;;; put the same value that you used as the API_KEY environment
;;; variable when starting the node.js application.
(defparameter *ownpt-api-key* "<insert key for the API web services>")

;;; configure your github application with the proper URLs and put the
;;; appropriate values here.  The callback URL must end on
;;; /wn/callback.
(defparameter *github-client-id* "<insert github client id>")
(defparameter *github-client-secret* "<insert github client secret>")

;;; these are accounts that are allowed to approve or reject
;;; suggestions regardless of their vote.
(defparameter *approve-reject-authorized-accounts*
  '("githubuser1" "githubuser2" "..."))

;;; these are the accounts that can vote.
(defparameter *vote-authorized-accounts*
  '("githubuser3" "githubuser4" "..."))
