(in-package :cl-wnbrowser)

;;; https://github.com/skypher/cl-oauth/blob/master/src/util/query-string.lisp
(defun query-string->alist (query-string)
  ;; TODO: doesn't handle leading ?
  (check-type query-string string)
  (let* ((kv-pairs (remove "" (split-sequence #\& query-string) :test #'equal))
         (alist (mapcar (lambda (kv-pair)
                          (let ((kv (split-sequence #\= kv-pair)))
                            (cons (first kv) (second kv))))
                        kv-pairs)))
        alist))

(defun get-access-token (code)
    (let ((stream (drakma:http-request
                   *github-access-token-url*
                   :method :post
                   :external-format-out :utf-8
                   :parameters (list
                                (cons "client_id" *github-client-id*)
                                (cons "client_secret" *github-client-secret*)
                                (cons "code" code)
                                (cons "accept" "json"))
                   :want-stream nil)))
      (cdr (assoc "access_token"
             (query-string->alist (flexi-streams:octets-to-string stream))
             :test #'equal))))

(defun get-user (access-token)

      (let ((stream (drakma:http-request
                   *github-user-api*
                   :method :get
                   :external-format-out :utf-8
                   :parameters (list (cons "access_token" access-token))
                   :want-stream t)))
        (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
        (let ((obj (yason:parse stream
                                :object-as :plist
                                :object-key-fn #'make-keyword)))
          (close stream)
          obj)))

(defun get-user-login (user)
  (getjso "login" user))
