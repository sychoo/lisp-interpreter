(defun do-repl (scanner)
  (format *standard-output* "calc> ")
  (let* ((token (peek-token scanner))
	 (type (token-type token)))
;;    (case type
;;	  (EOF nil)
;;	  (t (let ((expr (parse-terminated-expr scanner)))
;;	       (format *standard-output* 
;;		       "~a~%" 
;;		       (eval-expr expr)))
;;	       (do-repl scanner)))))
    (format *standard-output*
	    "~a~%"
	    token)
    (do-repl (advance-token scanner))))

(defun repl ()
  (let ((scanner (make-scanner)))
    (init-scanner scanner *standard-input*)
    (do-repl scanner)))
