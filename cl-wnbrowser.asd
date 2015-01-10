(asdf:defsystem #:cl-wnbrowser
    :serial t
    :depends-on (:drakma :flexi-streams :hunchentoot :yason :closure-template
		 :alexandria :cl-csv :cl-ppcre :parse-number)
    :components (
		 (:file "packages")
		 (:file "utils")
		 (:file "constants"     :depends-on ("packages"))
		 (:file "solr"          :depends-on ("utils" "constants"))
		 (:file "templates"     :depends-on ("constants"))
		 (:file "agraph"        :depends-on ("constants"))
		 (:file "web-service"   :depends-on ("templates"))
))
