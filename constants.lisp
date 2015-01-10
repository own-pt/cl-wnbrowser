(in-package :cl-wnbrowser)

(defparameter *basedir*
  (make-pathname :directory
		 (pathname-directory
		  (asdf:component-pathname (asdf:find-system '#:cl-wnbrowser)))))

(defparameter *wn30-schema-uri* "http://arademaker.github.com/wn30/schema/")

(defparameter *solr-endpoint-uri* "http://localhost:8983/solr")
(defparameter *solr-collection-id* "collection1")

(defparameter *allegro-graph-url* "http://logics.emap.fgv.br:10035/repositories")
(defparameter *allegro-graph-repository* "wn30")

(defparameter *queries-directory* #p"queries/")
(defparameter *templates-directory* #p"templates/")

(defparameter *query/by-lexfile* (merge-pathnames-as-file *queries-directory* #p"by-lexfile.query"))
(defparameter *query/by-pos-pt* (merge-pathnames-as-file *queries-directory* #p"by-pos-pt.query"))

(defparameter *facets* '(:|wn30_lexicographerFile| :|rdf_type|))

