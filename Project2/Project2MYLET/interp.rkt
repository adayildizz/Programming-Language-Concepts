#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ((= op 3) (cons num1top (* num2 num1bot)))
                              (else (cons (- num1top (* num2 num1bot)) num1bot))
       
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) 
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) 
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num2top num1bot)) (* num1bot num2bot) ))
         
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                       
                        (let ((num1top (car num1))
                          )
                          (if (zero? num1top)
                              (bool-val #t)
                              (bool-val #f))))

                        )))

      (rational-exp (exp1 exp2)
                   
                        (if (zero? exp2)
                            (eopl:error 'zero-division-error "NOOO")
                            (rational-val (cons exp1 exp2))))

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

 

      (list-exp () (list-val '()))

      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num (expval->num val1))
                        (lst (expval->list val2)))
                    
                    (list-val
                     (cons  num lst))
                    
                    )))

      (mul-exp (exp1)
               (let ((val1 (value-of exp1 env))
                      )
                 (let ((lst (expval->list val1)))

                   (num-val
                     (if (null? lst)
                         0
                         (mult-helper lst 1)
                          )
                     )
                   )))

      (min-exp (exp1)
               (let ((val1 (value-of exp1 env))
                      )
                 (let ((lst (expval->list val1)))

                   (if (null? lst)
                       (num-val -1)
                       (num-val (min-helper lst (car lst))))
                     
                   )))

      (if-elif-exp (exp1 exp2 exp3 exp4 exp5)
              (let ((val1 (value-of exp1 env))
                    (val3 (value-of exp3 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (if (expval->bool val3)
                        (value-of exp4 env)
                        (value-of exp5 env)
                        )

                    )))
      (simpl-exp (exp)
                 (let ((num (expval->rational (value-of exp env))))
                   (if (number? num)
                       (num-val num)
                       (rat-simplify exp env))))
      

      ;; -----------------------


      )))

(define (mult-helper lst init) (if (null? lst)
                                         init
                                         (mult-helper (cdr lst) (* init (car lst)))
        
                                      )
        )
(define (min-helper lst init) (if (null? lst) 
                                  init
                                  (if (<   (car lst) init)
                                      (min-helper (cdr lst) ( car lst))
                                      (min-helper (cdr lst) init))))
(define (gcd-calc a b)
  (if (= b 0)
      a
      (gcd-calc b (modulo a b))))
(define (rat-simplify rat env)
  (let ((val1 (value-of rat env)))
       (let ((num1 (expval->rational val1)))
         (let ((num1top (car num1))
              (num1bot (cdr num1)))
           (let ((gcd (gcd-calc num1top num1bot)))
             (rational-val (cons (/ num1top gcd) (/ num1bot gcd))))))))
         
         
                    
                               
