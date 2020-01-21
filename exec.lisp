(defun lval-lookup (var env)
  (assoc var env))

(defun rval-lookup (var env)
  (let ((lval (lval-lookup var env)))
    (if (null lval) 
	nil
      (cadr lval))))

(defun assign (var val env)
  (let ((lval (lval-lookup var env)))
    (if (null lval)
	(cons (list var val) env)
      (progn
	(setf (cadr lval) val)
	env))))

(defun exec-stmt-list (stmts env)
  (if (null stmts)
      env
    (exec-stmt-list (cdr stmts) (exec-stmt (car stmts) env))))

(defun make-write-format (expr env)
  (if (null expr)
      "~8D"
    (format nil "~~~aD" (eval-expr expr env))))

(defun make-writeln-format (expr env)
  (if (null expr)
      "~8D~%"
    (format nil "~~~aD~~%" (eval-expr expr env))))

(defun exec-stmt (stmt env)
  (etypecase 
   stmt
   (write-stmt (progn
		 (format *standard-output* 
			 (make-write-format (write-stmt-width stmt) env)
			 (eval-expr (write-stmt-arg stmt) env))
		 env))
   (writeln-stmt (if (null (writeln-stmt-arg stmt))
		     (progn (format *standard-output* "~%") env)
		   (progn (format *standard-output* 
				  (make-writeln-format (writeln-stmt-width stmt) env)
				  (eval-expr (writeln-stmt-arg stmt) env))
			  env)))
   (assign-stmt (assign (assign-stmt-lval stmt)
			(eval-expr (assign-stmt-rval stmt) env) env))
   (if-stmt (if (eq (eval-expr (if-stmt-test stmt) env) 'TRUE)
		(exec-stmt (if-stmt-body stmt) env)
	      env))
   (if-else-stmt (if (eq (eval-expr (if-else-stmt-test stmt) env) 'TRUE)
		     (exec-stmt (if-else-stmt-if-part stmt) env)
		   (exec-stmt (if-else-stmt-else-part stmt) env)))
   (cmpd-stmt (exec-stmt-list (cmpd-stmt-stmts stmt) env))
   (while-stmt (if (eq (eval-expr (while-stmt-test stmt) env) 'TRUE)
		   (exec-stmt stmt (exec-stmt (while-stmt-body stmt) env))
		 env))
   (empty-stmt env)))

(defun exec-program (prog env)
  (exec-stmt-list (program-stmts prog) env))
