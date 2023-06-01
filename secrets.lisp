(in-package :cl-wnbrowser)

(defparameter *base-url* "localhost:8040")

;; for production: github own-pt organization > seetings > oauth app 
(defparameter *github-client-id* (uiop:getenv "GITHUB_CLIENT_ID"))
(defparameter *github-client-secret* (uiop:getenv "GITHUB_CLIENT_SECRET"))

;;
(defparameter *approve-reject-authorized-accounts*
  '("arademaker" "githubuser2" "..."))

(defparameter *vote-authorized-accounts*
  '("arademaker" "githubuser2" "..."))


;; Databases for Elasticsearch-NLP service in the IBM Cloud (RIS-BRL)
(defparameter *password* (uiop:getenv "PASSWORD"))
(defparameter *user* "admin")
(setf clesc::*es-endpoint* (uiop:getenv "ES_URL"))

(setf clesc::*debug-query-dsl* t)
(setf clesc::*es-authentication* (list *user* *password*))