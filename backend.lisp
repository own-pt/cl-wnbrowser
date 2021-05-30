(in-package :cl-wnbrowser)

;; generics to used as backend

(defgeneric get-synset (backend id))

(defgeneric get-suggestions (backend id))

(defgeneric get-comments (backend id))

(defgeneric add-comment (backend id doc_type text login))

(defgeneric delete-comment (backend user id))

(defgeneric add-suggestion (backend id doc_type type params login))

(defgeneric delete-suggestion (backend user id))

(defgeneric accept-suggestion (backend id))

(defgeneric reject-suggestion (backend id))

(defgeneric delete-vote (backend id))

(defgeneric add-vote (backend id user value))

(defgeneric execute-search (backend term &key search-field rdf-type lex-file word-count-pt word-count-en
					   frame start limit))

(defgeneric search-activities (backend term &key sum_votes num_votes type tags action status
					      doc_type provenance user start limit so sf))

;; ES aux

(defun now ()
  "It returns the number of milliseconds elapsed since January 1,
1970, 00:00:00 UTC."
  (let ((timestamp (local-time:now)))
    (+
     (* 1000 (local-time:timestamp-to-unix timestamp))
     (local-time:timestamp-millisecond timestamp))))

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


(defun suggestion+votes (suggestion)
  (let*((yason:*parse-object-as* :plist)
	(yason:*parse-object-key-fn* #'make-keyword)
	(id (getf suggestion :|id|))
	(hits (getf
	       (getf (clesc:es/search "votes"
				      :terms `(("suggestion_id" ,id))
				      :size 100)
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

(defun get-tags (str)
  (mappend #'(lambda (x)
		(cond ((starts-with #\# x) (list (concatenate 'string "HASH" (subseq x 1))))
		      ((starts-with #\@ x) (list (concatenate 'string "AT" (subseq x 1))))))
	    (cl-ppcre:split "[\\s\.\,\"\'\:\;]+" str)))

;;; ES backend

(defmethod get-synset ((backend (eql 'es)) id)
  (let ((yason:*parse-object-as* :plist)
	(yason:*parse-object-key-fn* #'make-keyword))
    (getf (clesc:es/get "wn" "_doc" id) ':|_source|)))

(defmethod get-suggestions ((backend (eql 'es)) id)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (hits (getf
		(getf (clesc:es/search "suggestion" :terms `(("doc_id" ,id)
							     ("type" "suggestion"))
				       :size 100 :fields-order '(("date" "desc")))
		      :|hits|)
		:|hits|))
	 (suggestions (mapcar (lambda (hit) (getf hit :|_source|)) hits)))
    (mapcar #'suggestion+votes suggestions)))

(defmethod get-comments ((backend (eql 'es)) id)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (hits (getf
		(getf (clesc:es/search "suggestion" :terms `(("doc_id" ,id)
							     ("type" "comment"))
				       :size 100 :fields-order '(("date" "desc")))
		      :|hits|)
		:|hits|))
	 (comments (mapcar (lambda (hit) (getf hit :|_source|)) hits)))
    comments))

;; comments
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
		     ("tags" . ,(get-tags text))
		     ("id" . ,id)))))
    (clesc:es/add "suggestion" "suggestion" comment :id id :refresh "wait_for")))


(defmethod delete-comment ((backend (eql 'es)) user id)
  (let* ((db "wnproposedchanges")
	 (provenance "web")
	 (suggestion (gethash "_source" (clesc:es/get "suggestion" "suggestion" id)))
	 (value (format nil "~a(~a)"
			(gethash "action" suggestion)
			(gethash "params" suggestion))))
    (clesc:es/delete "suggestion" "suggestion" id :refresh "wait_for")
    (register-audit db "delete-comment" (gethash "doc_id" suggestion) "comment" value user provenance)))

;; suggestions
(defmethod add-suggestion ((backend (eql 'es)) synset-id doc-type suggestion-type params login)
  (let* ((suggestion-id (print-object (uuid:make-v4-uuid) nil))
	 (db "wnproposedchanges")
	 (provenance "web")
	 (type "suggestion")
	 (value (format nil "~a(~a)" suggestion-type params))
	 (suggestion (alexandria:alist-hash-table
		      `(("date" . ,(now))
			("doc_id" . ,synset-id)
			("doc_type" . ,doc-type)
			("type" . ,type)
			("action" . ,suggestion-type)
			("params" . ,params)
			("user" . ,login)
			("status" . "new")
			("provenance" . ,provenance)
			("id" . ,suggestion-id)))))
    (clesc:es/add "suggestion" "suggestion" suggestion :id suggestion-id :refresh "wait_for")
    (register-audit db "add-suggestion" synset-id type value login provenance)
    suggestion-id))

(defmethod delete-suggestion ((backend (eql 'es)) user id)
  (let* ((db "wnproposedchanges")
	 (provenance "web")
	 (suggestion (gethash "_source" (clesc:es/get "suggestion" "suggestion" id)))
	 (value (format nil "~a(~a)"
			(gethash "action" suggestion)
			(gethash "params" suggestion))))
    (clesc:es/delete "suggestion" "suggestion" id :refresh "wait_for")
    (register-audit db "delete-suggestion" (gethash "doc_id" suggestion) "suggestion" value user provenance)))

;; audit
(defun register-audit (db action doc-id field value user provenance)
  (let* ((audit-id (print-object (uuid:make-v4-uuid) nil))
	 (audit (alexandria:alist-hash-table
		 `(("date" . ,(now))
		   ("db" . ,db)
		   ("action" . ,action)
		   ("doc_id" . ,doc-id)
		   ("field" . ,field)
		   ("value" . ,value)
		   ("user" . ,user)
		   ("provenance" . ,provenance)
		   ("id" . ,audit-id)))))
    (clesc:es/add "audit" "audit" audit :id audit-id)))


;; vote
(defmethod add-vote ((backend (eql 'es)) suggestion-id user value)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (user-votes (getf
		      (getf (clesc:es/search "votes" :terms `(("suggestion_id" ,suggestion-id) ("user" ,user)))
			    :|hits|)
		      :|hits|)))
    (if user-votes
	'("error" "user-already-voted")
	(let* ((vote-id (print-object (uuid:make-v4-uuid) nil))
	       (db "wnvotes")
	       (vote (alexandria:alist-hash-table
		      `(("id" . ,vote-id)
			("date" . ,(now))
			("suggestion_id" . ,suggestion-id)
			("user" . ,user)
			("value" . ,value)))))
	  (clesc:es/add "votes" "votes" vote :id vote-id)
	  (register-audit db "add-vote" suggestion-id "vote" value user "web")
	  `("status" "vote-added" "id" ,vote-id)))))

(defmethod delete-vote ((backend (eql 'es)) id)
  (clesc:es/delete "votes" "votes" id))

;; search
(defmethod execute-search ((backend (eql 'es)) term &key search-field rdf-type lex-file word-count-pt word-count-en
						      frame start limit)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (filters (append
		   (when rdf-type (mapcar (lambda (x) `("rdf_type" ,x))  rdf-type))
		   (when lex-file (mapcar (lambda (x) `("wn30_lexicographerFile" ,x))  lex-file))
		   (when word-count-pt (mapcar (lambda (x) `("word_count_pt" ,x))  word-count-pt))
		   (when word-count-en (mapcar (lambda (x) `("word_count_en" ,x))  word-count-en))
		   (when frame (mapcar (lambda (x) `("wn30_frame" ,x))  frame))))
	 (result (clesc:es/search "wn"
				  :text (if (equal "all" search-field)
					    (if (emptyp term)
						term)
					    term)
				  :search-field (unless (equal "all" search-field) search-field)
				  :string (if (and (equal "all" search-field) (not (emptyp term))) term)
				  :size limit :terms filters :from start
				  :facets '("rdf_type" "wn30_lexicographerFile" "wn30_frame"
					    "word_count_pt" "word_count_en")
				  :agg-size 100))
	 (hits-1 (getf result :|hits|))
	 (hits-2 (getf hits-1 :|hits|))
	 (docs (mapcar (lambda (hit) (getf hit :|_source|)) hits-2))
	 (total (getf (getf hits-1 :|total|) :|value|))
	 (aggregations (getf result :|aggregations|))
	 (facets (mapcar #'(lambda (buckets)
	 		     (if (listp buckets)
	 			 (mapcar (lambda (bucket)
					   (list :|name| (format nil "~a" (getf bucket :|key|))
	 					 :|count| (getf  bucket :|doc_count|)))
					 (getf buckets :|buckets|))
	 			 buckets))
	 		 aggregations)))
    (values docs total facets nil)))

(defmethod search-activities ((backend (eql 'es)) term
			      &key sum_votes num_votes type tags action status
				doc_type provenance user start limit so sf)
  (let* ((yason:*parse-object-as* :plist)
	 (yason:*parse-object-key-fn* #'make-keyword)
	 (filters (append
		   (when sum_votes (mapcar (lambda (x) `("sum_votes" ,x)) sum_votes))
		   (when num_votes (mapcar (lambda (x) `("vote_score" ,x)) num_votes))
		   (when type (mapcar (lambda (x) `("type" ,x)) type))
		   (when tags (mapcar (lambda (x) `("tags" ,x)) tags))
		   (when action (mapcar (lambda (x) `("action" ,x)) action))
		   (when status (mapcar (lambda (x) `("status" ,x)) status))
		   (when doc_type (mapcar (lambda (x) `("doc_type" ,x)) doc_type))
		   (when provenance (mapcar (lambda (x) `("provenance" ,x)) provenance))
		   (when user (mapcar (lambda (x) `("user" ,x)) user))))
	 (sort   (if (not (emptyp sf)) `((,sf ,(if (equal so "") "desc" so))) '(("date" "desc"))))
	 (result (clesc:es/search "suggestion"
				  :text term ; (unless (equal "all" search-field) term)
				  :search-field "params"
				  ; :string (if (equal "all" search-field) term)
				  :size limit :terms filters :from start :fields-order sort
				  :facets '("type" "action" "status" "doc_type" "user"
					    "provenance" "tags" "sum_votes" "vote_score")
				  :agg-size 300))
	 (hits-1 (getf result :|hits|))
	 (hits-2 (getf hits-1 :|hits|))
	 (docs (mapcar (lambda (hit) (getf hit :|_source|)) hits-2))
	 (total (getf (getf hits-1 :|total|) :|value|))
	 (aggregations (getf result :|aggregations|))
	 (facets (mapcar #'(lambda (buckets)
	 		     (if (listp buckets)
	 			 (mapcar (lambda (bucket)
					   (list :|name| (format nil "~a" (getf bucket :|key|))
	 					 :|count| (getf  bucket :|doc_count|)))
					 (getf buckets :|buckets|))
	 			 buckets))
	 		 aggregations)))
    (values docs total facets nil)))
