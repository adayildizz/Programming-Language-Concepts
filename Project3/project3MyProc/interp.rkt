#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here
      (stack-exp () (stack-val '()))

      (stack-push-exp (exp1 exp2)
                      (let ((oldstack (expval->stack (value-of exp1 env)))
                            (num (expval->num (value-of exp2 env))))
                        (stack-val (cons num oldstack))))
      (stack-pop-exp (exp)
                     (let ((oldstack (expval->stack (value-of exp env))))
                        (stack-val (stack-pop-exp-helper oldstack))))
      (stack-peek-exp (exp)
                      (let ((stack (expval->stack (value-of exp env))))
                        (num-val (if (stackEmpty? stack) 2813
                                       (car stack)))))

      ;; These are to do. '() should be replaced by actual function
      (stack-push-multi-exp (exp exps)
                            (let ((stack (expval->stack (value-of exp env)))
                                  (nums (map (lambda (exp) (expval->num (value-of exp env))) exps)))
                              (stack-val (stack-push-multi-exp-helper stack nums))))

      (stack-pop-multi-exp (exp1 exp2)
                            (let ((stack (expval->stack (value-of exp1 env)))
                                  (num (expval->num (value-of exp2 env))))
                              (stack-val (stack-pop-multi-exp-helper stack num))))

      (stack-merge-exp (exp1 exp2)
                        (let ((stack1 (expval->stack (value-of exp1 env)))
                              (stack2 (expval->stack (value-of exp2 env))))
                          (stack-val (stack-merge-exp-helper stack1 stack2))))
      
      ;;-------------------------------------------------
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define (stackEmpty? stack)
  (if (null? stack)
      (begin (display "Warning: Stack is empty!")
             (newline)
             #t)
      #f))
(define (stack-pop-exp-helper oldstack)
  (if (stackEmpty? oldstack) oldstack
                                       (cdr oldstack)))
(define (stack-pop-multi-exp-helper stack num)
  (if (zero? num)
      stack
      (stack-pop-multi-exp-helper (stack-pop-exp-helper stack) (- num 1))))
(define (stack-push-multi-exp-helper stack nums)
  (if (null? nums)
      stack
      (stack-push-multi-exp-helper (cons (car nums) stack) (cdr nums)))
  )
(define (stack-merge-exp-helper stack1 stack2)
  (if (null? stack2)
      stack1
      (stack-merge-exp-helper (cons (car stack2) stack1) (cdr stack2))
   )
  )
;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
