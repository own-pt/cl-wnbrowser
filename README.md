# README

This is CL-WNBROWSER pre-alpha, version 0.

## How do I get set up?

* SOLR (see CONSTANTS.LISP for configuration options);
* SBCL (tested with 1.2.2 and 1.1.14) or CCL (tested with 1.10);
* For easier/faster setup, quicklisp is highly recommended.

### SOLR setup

* We tested with SOLR 4.10.3, Jetty integration;

* You need to convert your triples to SOLR documents.  This
  transformation isn't formally documented yet, but you can see the
  code at
  https://github.com/arademaker/wordnet-editor/blob/master/solr.lisp

### Starting up the web server

* This package uses [Hunchentoot](http://weitz.de/hunchentoot/).  To
  start the server, switch to the CL-WNBROWSER package and execute
  (START-SERVER *port*).  See that function for more details.

* It is recommended that you run Hunchentoot under a proxy.

## Related Projects

- This browser can be used for browsing and searching any wordnet, but
  it was specially developed to
  [openWordnet-PT](http://github.com/arademaker/openWordnet-PT).

## License

The CL-WNBROWSER distribution is available under the MIT License.  See
the file LICENSE for details.

