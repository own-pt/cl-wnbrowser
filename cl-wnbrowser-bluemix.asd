;; -*- mode: common-lisp -*-

;; copyright (c) 2015 The OpenWordNet-PT projetct
;; This program and the accompanying materials are made available
;; under the terms of the LICENSE file.

(asdf:defsystem #:cl-wnbrowser-bluemix
    :serial t
    :depends-on (:drakma :flexi-streams :hunchentoot :yason :closure-template
                         :split-sequence :local-time :graph-algorithms :fare-csv
                         :cl-fad :trivial-shell :mk-string-metrics
                         :alexandria :cl-csv :cl-ppcre :parse-number)
    :components (
		 (:file "packages")
		 (:file "utils")
                 (:file "secrets")
                 (:file "wn-cache")
                 (:file "freeling")
                 (:file "graph-algorithms" :depends-on ("wn-cache"))
                 (:file "phrases"       :depends-on ("wn-cache"))
                 (:file "wn-analysis"   :depends-on ("wn-cache"))
		 (:file "constants"     :depends-on ("packages"))
                 (:file "github"        :depends-on ("constants"))
		 (:file "ownpt-api"     :depends-on ("utils" "constants"))
                 (:file "sense-tagging" :depends-on ("ownpt-api"))
		 (:file "templates"     :depends-on ("constants"))
		 (:file "agraph"        :depends-on ("constants"))
                 (:file "corpora"       :depends-on ("ownpt-api"))
		 (:file "web-service"   :depends-on ("secrets" "templates" "ownpt-api" "github" "freeling"))
))
