# README #

This is CL-WNBROWSER pre-alpha, version 0.

### How do I get set up? ###

* SOLR (see CONSTANTS.LISP for configuration options);
* SBCL or CCL;
* For easier/faster setup, quicklisp is highly recommended.

### License ###

The CL-WNBROWSER distribution is available under the MIT License.  See
the file LICENSE for details.

### SOLR setup ###

* You need to convert your triples to SOLR documents.  This
  transformation isn't formally documented yet, but you can see the
  code at
  https://github.com/arademaker/wordnet-editor/blob/master/solr.lisp
