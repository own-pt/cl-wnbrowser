;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the LICENSE file.

(asdf:defsystem #:cl-wnbrowser-bluemix
    :serial t
    :depends-on (:drakma :flexi-streams :hunchentoot :yason :closure-template
                         :split-sequence :local-time :graph-algorithms
                         :alexandria :cl-csv :cl-ppcre :parse-number)
    :components (
		 (:file "packages")
		 (:file "utils")
                 (:file "secrets")
                 (:file "wn-cache")
                 (:file "graph-algorithms" :depends-on ("wn-cache"))
                 (:file "phrases"       :depends-on ("wn-cache"))
                 (:file "wn-analysis"   :depends-on ("wn-cache"))
		 (:file "constants"     :depends-on ("packages"))
                 (:file "github"        :depends-on ("constants"))
		 (:file "ownpt-api"     :depends-on ("utils" "constants"))
                 (:file "sense-tagging" :depends-on ("ownpt-api"))
		 (:file "templates"     :depends-on ("constants"))
		 (:file "agraph"        :depends-on ("constants"))
		 (:file "web-service"   :depends-on ("secrets" "templates" "ownpt-api" "github"))
))
