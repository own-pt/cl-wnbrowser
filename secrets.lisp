(in-package :cl-wnbrowser)

(defparameter *base-url* "localhost:8040")

;; for production: github own-pt organization > seetings > oauth app 
(defparameter *github-client-id* "19ac9046042b97f88f88")
(defparameter *github-client-secret* "ea064ffdd1c28aebddc208b40ec04cc5ee440ca7")

;;
(defparameter *approve-reject-authorized-accounts*
  '("arademaker" "githubuser2" "..."))

(defparameter *vote-authorized-accounts*
  '("arademaker" "githubuser2" "..."))


;; Databases for Elasticsearch-NLP service in the IBM Cloud (RIS-BRL)
(defparameter *password* "cff6d76ac0361bcc518cab2de7ba9b88f667458de86fbf705516")
(defparameter *user* "admin")
(setf clesc::*es-endpoint*
      "https://5325493c-3489-4c1d-a81e-5db7cbaef410.8117147f814b4b2ea643610826cd2046.databases.appdomain.cloud:31366")

(setf clesc::*debug-query-dsl* t)
(setf clesc::*es-authentication* (list *user* *password*))
