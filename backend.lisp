(in-package :cl-wnbrowser)

(defgeneric get-synset (backend id))

(defgeneric get-suggestions (backend id))

(defgeneric get-comments (backend id))

(defgeneric add-comment (backend id doc_type text login))

(defgeneric delete-comment (backend id))
;;;

(defun get-synset-word-en (id)
  "Returns the FIRST entry in the word_en property for the given SYNSET-ID"
  (car (getf (get-synset *backend* id) :|word_en|)))

(defun get-synset-word (id)
  "If the synset has word_pt entries, returns the first one; otherwise
returns the first entry in word_en."
  (let* ((synset (get-synset *backend* id))
         (word-pt (car (getf synset :|word_pt|)))
         (word-en (car (getf synset :|word_en|))))
    (if word-pt word-pt word-en)))

(defun get-synset-gloss (id)
  "If the synset has word_pt entries, returns the first one; otherwise
returns the first entry in word_en."
  (let* ((synset (get-synset *backend* id))
         (gloss-pt (car (getf synset :|gloss_pt|)))
         (gloss-en (car (getf synset :|gloss_en|))))
    (if gloss-pt gloss-pt gloss-en)))

;;;
(defmethod get-synset ((backend (eql 'es)) id)
  (let ((yason:*parse-object-as* :plist)
	(yason:*parse-object-key-fn* #'make-keyword))
    (getf (clesc:es/get "wn" "_doc" id) ':|_source|)))

(defmethod get-suggestions ((backend (eql 'es)) id)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (hits (getf
		(getf (clesc:es/search "suggestion" :terms `(("doc_id" ,id)
							     ("type" "suggestion")))
		      :|hits|)
		:|hits|))
	 (suggestions (mapcar (lambda (hit) (getf hit :|_source|)) hits)))
    (mapcar #'sugestion+votes suggestions)))


(defun sugestion+votes (suggestion)
  (let*((yason:*parse-object-as* :plist)
	(yason:*parse-object-key-fn* #'make-keyword)
	(id (getf suggestion :|id|))
	(hits (getf
	       (getf (clesc:es/search "votes" :terms `(("suggestion_id" ,id)))
		     :|hits|)
	       :|hits|))
	(votes (mapcar (lambda (hit) (getf hit :|_source|)) hits)))
    (loop
       for vote in votes
       with positive = 0
       with negative = 0
       with total = 0
       with positive-votes = '()
       with negative-votes = '() 
       do
	 (let ((value (getf vote :|value|)))
	   (incf total value)
	   (cond ((equal 1 value)
		  (incf positive value)
		  (push vote positive-votes))
		 ((equal -1 value)
		  (incf negative value)
		  (push vote negative-votes))))
       finally (return `(,@suggestion
			 :|votes| (:|positive| ,positive :|negative| ,negative :|total| ,total
				    :|positive_votes| ,positive-votes :|negative_votes| ,negative-votes))))))

(defmethod get-comments ((backend (eql 'es)) id)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (hits (getf
		(getf (clesc:es/search "suggestion" :terms `(("doc_id" ,id)
							     ("type" "comment")))
		      :|hits|)
		:|hits|))
	 (comments (mapcar (lambda (hit) (getf hit :|_source|)) hits)))
    comments))


(defun now ()
  (let ((timestamp (local-time:now)))
    (+ (* 86400000 (+ 11017 (local-time:day-of timestamp)))
       (* 1000 (local-time:sec-of timestamp))
       (local-time:nsec-of timestamp))))


(defmethod add-comment ((backend (eql 'es)) synset-id doc_type text login)
  (let* ((id (print-object (uuid:make-v4-uuid) nil))
	 (comment (alexandria:alist-hash-table
		   `(("date" . ,(now))
		     ("doc_id" . ,synset-id)
		     ("doc_type" . ,doc_type)
		     ("type" . "comment")
		     ("action" . "comment")
		     ("user" . ,login)
		     ("params" . ,text)
		     ("status" . "new")
		     ("provenance" . "web")
		     ;; ("tags" . (localGetTags params)) TODO
		     ("id" . ,id)))))
    (clesc:es/add "suggestion" "suggestion" comment :id id)))

(defmethod defmethod add-comment ((backend (eql 'es)) id))
