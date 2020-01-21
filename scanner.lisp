(defun peek-c (stream)
  (peek-char nil stream nil))

(defun read-c (stream)
  (read-char stream nil))

(defun whitespace-char-p (c)
  (case c 
	((#\Space #\Newline #\Tab) t)
	(t nil)))

(defun int-const-token (lexeme-list)
  (values 'INT-CONST (parse-integer (coerce (reverse lexeme-list) 
					    'string))))

(defun scan-int-const (stream lexeme-list)
  (let ((first (peek-c stream)))
    (cond ((null first) (int-const-token lexeme-list))
	  ((digit-char-p first) 
   (read-c stream)
	   (scan-int-const stream (cons first lexeme-list)))
	  (t (int-const-token lexeme-list)))))

(defun check-reserved (lexeme-list)
  (let ((reserved '(DIV MOD WRITE WRITELN IF THEN ELSE 
			WHILE DO BEGIN END AND OR NOT TRUE FALSE))
	(lexeme (intern (string-upcase (coerce (reverse lexeme-list) 
					       'string)))))
    (if (member lexeme reserved)
	lexeme
      (values 'IDENT lexeme))))

(defun scan-ident (stream lexeme-list)
  (let ((first (peek-c stream)))
    (cond ((null first) (check-reserved lexeme-list))
	  ((alphanumericp first)
	   (read-c stream)
	   (scan-ident stream (cons first lexeme-list)))
	  (t (check-reserved lexeme-list)))))

(defun scan-colon (stream)
  (case (peek-c stream) 
	(#\= (read-c stream) 'ASSIGN)
	(t 'COLON)))

(defun scan-lt (stream)
  (case (peek-c stream) 
	(#\= (read-c stream) 'LE)
	(#\> (read-c stream) 'NE)
	(t 'LT)))

(defun scan-gt (stream)
  (case (peek-c stream) 
	(#\= (read-c stream) 'GE)
	(t 'GT)))

(defun scan (stream)
  (let ((first (read-c stream)))
    (cond ((null first) nil)
	  ((whitespace-char-p first) (scan stream))
	  ((digit-char-p first) (scan-int-const stream (list first)))
	  ((alpha-char-p first) (scan-ident stream (list first)))
	  (t (ecase first
		    (#\+ 'PLUS)
		    (#\- 'MINUS)
		    (#\* 'TIMES)
		    (#\( 'LPAREN)
		    (#\) 'RPAREN)
		    (#\; 'SEMICOLON)
		    (#\) 'RPAREN)
		    (#\= 'EQ)
		    (#\< (scan-lt stream))
		    (#\> (scan-gt stream))
		    (#\: (scan-colon stream)))))))
