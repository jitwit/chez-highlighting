(import (chezscheme))

(library-exports '(chezscheme))

(define (analyze-procedure symbol)
  (call/cc
    (lambda (k)
      (with-exception-handler
	  (lambda (e)
	    (k '()))
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
		       library define-record-type fields else =>)
		  '())
		 ((cond import export)
		  `((,symbol . 0)))
		 (else `((,symbol . 1))))))
	(lambda ()
	  (eval symbol)
	  '())))))

(define (make-highlighting-rules library file)
  (let* ((exports (library-exports library))
	 (procedures (fold-right append '() (map analyze-procedure exports)))
	 (syntax-forms (fold-right append '() (map analyze-syntax exports))))
    (when (file-exists? file)
      (delete-file file))
    (with-output-to-file file
      (lambda ()
	(display `(progn
		   (scheme-add-keywords 'font-lock-keyword-face
					',syntax-forms)
		   (scheme-add-keywords 'font-lock-builtin-face
					;; 		    'font-lock-function-name-face
					',procedures)))))))





