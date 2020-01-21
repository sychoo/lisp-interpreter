(load 'yacc)
(use-package 'yacc)

(load 'scanner)
(load 'parser)
(load 'eval)
(load 'exec)

(defun interp (file)
  (let ((stream (open file)))
    (exec-program (parse-with-lexer (lambda () (scan stream))
				    *prog-parser*)
		  nil)))

(interp (car *args*))
