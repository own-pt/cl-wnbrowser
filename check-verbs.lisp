;; -*- mode: common-lisp -*-

;; Copyright (c) 2015,2016 The OpenWordNet-PT project
;; This program and the accompanying materials are made available
;; under the terms described in the LICENSE file.

(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :alexandria)
(ql:quickload :split-sequence)

(defparameter *verbs* nil)
(with-open-file (stream "gold_standard.txt")
  (do ((line (read-line stream nil)
             (read-line stream nil)))
      ((null line))
    (push (car (split-sequence:split-sequence #\, line)) *verbs*)))

(print (length *verbs*))

;; (setf drakma:*header-stream* *standard-output*)

(defvar *server* "wnpt.brlcloud.com")

(defun search-activities (term)
    (let* ((stream (drakma:http-request
                   (format nil "http://~a/wn/search-activities?sf=&so=&term=~a&start=0&fq_action=add-word-pt" *server* term)
		   :external-format-out :utf-8
                   :accept "application/json"
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :alist
			    :object-key-fn #'alexandria:make-keyword)))
      (close stream)
      obj)))

(defun search-wn (term)
    (let* ((stream (drakma:http-request
                   (format nil "http://~a/wn/search?term=word_pt:~a" *server* term)
		   :external-format-out :utf-8
                   :accept "application/json"
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :alist
			    :object-key-fn #'alexandria:make-keyword)))
      (close stream)
      obj)))

(dolist (v *verbs*)
  (let ((wn (cdr (assoc :NUMFOUND (search-wn v))))
        (ac (cdr (assoc :NUMFOUND (search-activities v)))))
    (when (and (= 0 wn) (= 0 ac))
      (format t "!!! ~a not in OWN-PT~%" v))
    (when (and (= wn 0) (> ac 0))
      (format t "~a in SUGGESTIONS.~%" v))))
