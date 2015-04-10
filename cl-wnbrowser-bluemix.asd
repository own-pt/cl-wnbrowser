;; -*- mode: common-lisp -*-

;; copyright (c) 2015 Fabricio Rosario (f@cp300.org)
;; This program and the accompanying materials are made available
;; under the terms of the MIT License which accompanies this
;; distribution (see LICENSE)

(asdf:defsystem #:cl-wnbrowser-bluemix
    :serial t
    :depends-on (:drakma :flexi-streams :hunchentoot :yason :closure-template
                         :split-sequence :local-time
                         :alexandria :cl-csv :cl-ppcre :parse-number)
    :components (
		 (:file "packages")
		 (:file "utils")
                 (:file "secrets")
		 (:file "constants"     :depends-on ("packages"))
                 (:file "github"        :depends-on ("constants"))
		 (:file "cloudant"      :depends-on ("utils" "constants"))
		 (:file "templates"     :depends-on ("constants"))
		 (:file "agraph"        :depends-on ("constants"))
		 (:file "web-service"   :depends-on ("secrets" "templates" "cloudant" "github"))
))
