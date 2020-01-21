(defstruct var-expr name)
(defstruct bool-const-expr value)
(defstruct int-const-expr value)
(defstruct unary-expr op rand)
(defstruct binary-expr op left right)

(defstruct assign-stmt lval rval)
(defstruct write-stmt arg width)
(defstruct writeln-stmt arg width)
(defstruct if-stmt test body)
(defstruct if-else-stmt test if-part else-part)
(defstruct while-stmt test body)
(defstruct empty-stmt)
(defstruct cmpd-stmt stmts)
(defstruct program stmts)

(defun binary (op) 
  (lambda (x y z) (make-binary-expr :op op :left x :right z)))

(defun unary (op) 
  (lambda (x y) (make-unary-expr :op op :rand y)))

(define-parser *prog-parser* 
  (:muffle-conflicts (1 0))
  (:start-symbol prog)
  (:terminals (INT-CONST TRUE FALSE IDENT
	       PLUS MINUS 
	       TIMES DIV MOD
	       LPAREN RPAREN
	       ASSIGN
	       COLON SEMICOLON PERIOD COMMA
	       WRITE WRITELN
	       IF THEN ELSE  
	       WHILE DO 
	       BEGIN END
	       AND OR NOT
	       LT LE GT GE EQ NE))
  (prog
   (stmt-list (lambda (a) (make-program :stmts a))))
  (stmt-list
   (stmt #'list)
   (stmt SEMICOLON stmt-list (lambda (a b c) (cons a c))))
  (stmt
   (assign-stmt #'identity)
   (write-stmt #'identity)
   (writeln-stmt #'identity)
   (if-stmt #'identity)
   (if-else-stmt #'identity)
   (while-stmt #'identity)
   (cmpd-stmt #'identity)
   (empty-stmt #'identity))
  (cmpd-stmt 
   (BEGIN stmt-list END (lambda (a b c) (make-cmpd-stmt :stmts b))))
  (empty-stmt
   ((lambda () (make-empty-stmt))))
  (assign-stmt
   (IDENT ASSIGN expr 
	  (lambda (a b c) (make-assign-stmt :lval a :rval c))))
  (if-stmt 
   (IF expr THEN stmt 
       (lambda (a b c d) (make-if-stmt :test b :body d))))
  (if-else-stmt 
   (IF expr THEN stmt ELSE stmt 
       (lambda (a b c d e f) (make-if-else-stmt :test b 
						:if-part d
						:else-part f))))
  (while-stmt
   (WHILE expr DO stmt 
	  (lambda (a b c d) (make-while-stmt :test b
					     :body d))))
  (write-stmt
   (WRITE LPAREN expr RPAREN 
	  (lambda (a b c d) (make-write-stmt :arg c :width nil)))
   (WRITE LPAREN expr COLON expr RPAREN 
	  (lambda (a b c d e f) (make-write-stmt :arg c :width e))))
  (writeln-stmt
   (WRITELN LPAREN RPAREN
	    (lambda (a b c) (make-writeln-stmt :arg nil :width nil)))
   (WRITELN LPAREN expr RPAREN 
	    (lambda (a b c d) (make-writeln-stmt :arg c :width nil)))
   (WRITELN LPAREN expr COLON expr RPAREN 
	    (lambda (a b c d e f) (make-writeln-stmt :arg c :width e))))
  (expr 
   (or-expr #'identity))
  (or-expr
   (and-expr #'identity)
   (or-expr OR and-expr (binary'OR)))
  (and-expr 
   (relational-expr #'identity)
   (and-expr AND relational-expr (binary 'AND)))
  (relational-expr
   (additive-expr #'identity)
   (additive-expr LT additive-expr (binary 'LT))
   (additive-expr LE additive-expr (binary 'LE))
   (additive-expr GT additive-expr (binary 'GT))
   (additive-expr GE additive-expr (binary 'GE))
   (additive-expr EQ additive-expr (binary 'EQ))
   (additive-expr NE additive-expr (binary 'NE)))
  (additive-expr
   (multiplicative-expr #'identity)
   (additive-expr PLUS multiplicative-expr (binary 'PLUS))
   (additive-expr MINUS multiplicative-expr (binary 'MINUS)))
  (multiplicative-expr
   (unary-expr #'identity)
   (multiplicative-expr TIMES unary-expr (binary 'TIMES))
   (multiplicative-expr DIV unary-expr (binary 'DIV))
   (multiplicative-expr MOD unary-expr (binary 'MOD)))
  (unary-expr
   (primary-expr #'identity)
   (PLUS unary-expr (lambda (x y) y))
   (MINUS unary-expr (unary 'MINUS))
   (NOT unary-expr (unary 'NOT)))
  (primary-expr
   (INT-CONST (lambda (a) (make-int-const-expr :value a)))
   (IDENT (lambda (a) (make-var-expr :name a)))
   (TRUE (lambda (a) (make-bool-const-expr :value 'TRUE)))
   (FALSE (lambda (a) (make-bool-const-expr :value 'FALSE)))
   (LPAREN expr RPAREN (lambda (a b c) b))))
