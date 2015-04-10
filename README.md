# README #

This is CL-WNBROWSER pre-alpha, version 0.  It is currently running at http://logics.emap.fgv.br/wn/

See also the OpenWordnet-PT site at https://github.com/arademaker/openWordnet-PT/

### How do I get set up? ###

* Cloudant (see CONSTANTS.LISP for configuration options);
* SBCL (tested with 1.2.2 and 1.1.14) or CCL (tested with 1.10);
* For easier/faster setup, quicklisp is highly recommended.

### License ###

The CL-WNBROWSER distribution is available under the MIT License.  See
the file LICENSE for details.

### Cloudant setup ###

* You need to convert your triples to Cloudant documents.  This
  transformation isn't formally documented yet, but you can see the
  code at
  https://github.com/arademaker/wordnet-editor/blob/master/couchdb.lisp

### Starting up the web server ###

* This package uses [Hunchentoot](http://weitz.de/hunchentoot/).  To
  start the server, switch to the **`CL-WNBROWSER`** package and execute
  **`(START-SERVER port)`**.  See that function for more details.

* It is recommended that you run Hunchentoot under a proxy.

### Updating the application ###

* If lisp files changed, you can try loading them again.  This may not
  work, given the dependencies between the different files.  The
  safest option is to simply reload the application.

* If only templates changed, you can safely update the application by
  switching to the **`CL-WNBROWSER`** package and executing the
  **`(SETUP-TEMPLATES)`** method.

### Updating the statistics ###

* Got to http://logics.emap.fgv.br/wn/update-stats .  The server will
  perform the update and redirect to the statistics page.

* Optionally you can pass the header **`Accept: application/json`** and the
  server will reply with a status code in JSON.
