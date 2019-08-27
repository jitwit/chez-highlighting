(import (chezscheme)
	(chez euler)
	(chez patricia)
	(chez kd)
	(chez patricia-set)
	(srfi :1)
	(srfi :13)
	(srfi :14))

(define default-libs
  '((chezscheme)
    (chez euler)
    (chez patricia)
    (chez kd)
    (chez patricia-set)
    (rnrs)
    (srfi :1)
    (srfi :13)
    (srfi :14)))

(define (analyze-procedure symbol)
  (call/cc
    (lambda (k)
      (with-exception-handler
	  (lambda (e) (k '()))
	(lambda ()
	  (let ((arity (procedure-arity-mask (eval symbol))))
	    `((,symbol . ,(max 0 (+ -2 (bitwise-length arity)))))))))))

(define (analyze-syntax symbol)
  (call/cc
    (lambda (k)
      (with-exception-handler
	  (lambda (e)
	    (k (case symbol
		 ((let let* letrec define lambda let-values define-values
		       library define-record-type fields else => if)
		  '())
		 ((cond import export or and)
		  `((,symbol . 0)))
		 (else `((,symbol . 1))))))
	(lambda ()
	  (eval symbol)
	  '())))))

(define (decide-highlighting-rules symbols)
  (let* ((procedures (fold-right append '() (map analyze-procedure symbols)))
	 (syntax-forms (fold-right append '() (map analyze-syntax symbols))))
    (display
	`(progn
	  (scheme-add-keywords 'font-lock-builtin-face
			       ',procedures)
	  (scheme-add-keywords 'font-lock-keyword-face
			       ',syntax-forms)))))

(define (make-highlighting-rules libraries file)
  (let ((table (make-eq-hashtable)))
    (for-each (lambda (lib)
		(format #t "~a~%" lib)
		(for-each (lambda (symbol)
			    (hashtable-set! table symbol #t))
			  (library-exports lib)))
	      libraries)
    (when (file-exists? file)
      (delete-file file))
    (with-output-to-file file
	(lambda ()
	  (decide-highlighting-rules (vector->list
				       (hashtable-keys table)))))))

(define (run)
  (make-highlighting-rules default-libs "chezscheme.el"))


