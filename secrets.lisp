(in-package :cl-wnbrowser)

(defparameter *base-url* "localhost:8040")

;;; local dev
(defparameter *github-client-id* "<insert github client id>")
(defparameter *github-client-secret* "<insert github client secret>")

;;;
(defparameter *approve-reject-authorized-accounts*
  '("githubuser1" "githubuser2" "..."))

(defparameter *vote-authorized-accounts*
  '("githubuser1" "githubuser2" "..."))
