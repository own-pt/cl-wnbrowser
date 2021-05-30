(in-package :cl-user)

(print ">>> Building system....")
(load (merge-pathnames "cl-wnbrowser.asd" *build-dir*))

(ql:quickload :cl-wnbrowser)

(defun initialize-application ()
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

  (let* ((vcap (yason:parse (heroku-getenv "VCAP_APPLICATION")))
	 (url (car (gethash "application_uris" vcap))))
    (setf cl-wnbrowser::*base-url* (format nil "~a" url)))

  (let ((root-accounts (split-sequence:split-sequence #\, (heroku-getenv "ROOT_ACCOUNTS")))
	(vote-accounts (split-sequence:split-sequence #\, (heroku-getenv "VOTE_ACCOUNTS"))))
    (setf cl-wnbrowser::*approve-reject-authorized-accounts* root-accounts
	  cl-wnbrowser::*vote-authorized-accounts*           vote-accounts))

  (setf cl-wnbrowser::*basedir* "/home/vcap/app/")
  (setf cl-wnbrowser::*github-client-id* (heroku-getenv "GITHUB_CLIENT_ID"))
  (setf cl-wnbrowser::*github-client-secret* (heroku-getenv "GITHUB_CLIENT_SECRET"))
  (cl-wnbrowser::publish-static-content cl-wnbrowser::*basedir*))

(print ">>> Done building system")
