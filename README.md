# README #

This is CL-WNBROWSER version 3.0.  It is currently running at http://wn.mybluemix.net/

See also the OpenWordnet-PT project: https://github.com/own-pt/openWordnet-PT

### How do I get set up? ###

* SBCL (tested up to 1.3.0) or CCL (tested with 1.10);
* For easier/faster setup, quicklisp is highly recommended.

### License ###

See the file LICENSE for details.

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
