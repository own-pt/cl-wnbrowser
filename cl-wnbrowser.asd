;; -*- mode: common-lisp -*-

;; copyright (c) 2015 The OpenWordNet-PT projetct
;; This program and the accompanying materials are made available
;; under the terms of the LICENSE file.

(asdf:defsystem #:cl-wnbrowser
    :serial t
    :depends-on (:drakma :flexi-streams :hunchentoot :yason :closure-template
                         :split-sequence :local-time :graph-algorithms :fare-csv
                         :cl-fad :trivial-shell :mk-string-metrics :cl-strings
                         :alexandria :cl-csv :cl-ppcre :parse-number :clesc
			 :uuid)
    :components ((:file "packages")
		 (:file "utils")
                 (:file "secrets")
		 (:file "backend")
		 (:file "constants"     :depends-on ("packages"))
                 (:file "github"        :depends-on ("constants"))
		 (:file "ownpt-api"     :depends-on ("backend" "utils" "constants"))
		 (:file "templates"     :depends-on ("constants"))
		 (:file "agraph"        :depends-on ("constants"))
		 (:file "web-service"   :depends-on ("secrets" "templates" "ownpt-api" "backend" "github"))))
