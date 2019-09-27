(in-package :cl-user)

(print ">>> Building system....")

(load (merge-pathnames "cl-wnbrowser-bluemix.asd" *build-dir*))

(ql:quickload :cl-wnbrowser-bluemix)
(ql:quickload :yason)

(let* ((vcap (yason:parse (heroku-getenv "VCAP_SERVICES")))
       (es-services (car (gethash "databases-for-elasticsearch" vcap)))
       (https (gethash "https"
		       (gethash "connection"
				(gethash "credentials" es-services))))
       (host (car (gethash "hosts" https)))
       (hostname (gethash "hostname" host))
       (port     (gethash "port" host))
       (authentication (gethash "authentication" https))
       (user (gethash "username" authentication))
       (password (gethash "password" authentication)))
  
  (setf clesc::*es-endpoint* (format NIL "https://~a:~a" hostname port)
	clesc::*es-authentication* (list user password)))
;;; Redefine / extend heroku-toplevel here if necessary.

(setf cl-wnbrowser::*basedir* "/home/vcap/app/")
(setf cl-wnbrowser::*base-url* "http://dhbb.mybluemix.net/")

(defun initialize-application ()
  (cl-wnbrowser::publish-static-content "/home/vcap/app/"))

(print ">>> Done building system")
