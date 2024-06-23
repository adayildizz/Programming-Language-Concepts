(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  (require racket/vector)
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
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

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        (newvector-exp (exp1 exp2)
                    (let ((length (expval->num (value-of exp1 env)))
                             (value  (value-of exp2 env)))
                       (vec-val (new-vec length value)))
                      )

        (updatevector-exp (exp1 exp2 exp3)
                       (let ((a-vec (expval->vec (value-of exp1 env)))
                                (idx (expval->num (value-of exp2 env)))
                                (value  (value-of exp3 env)))
                             (update-vec a-vec idx value)
                          ))

        (readvector-exp (exp1 exp2)
                         (let ((a-vec (expval->vec (value-of exp1 env)))
                                (idx (expval->num (value-of exp2 env))))
                           ( read-vec a-vec idx)))

       (lengthvector-exp (exp1)
                           (let ((a-vec (expval->vec (value-of exp1 env))))
                             (cases vec a-vec
                               (vectorr (head length)
                                       (num-val length)))
                               ))

        (swapvector-exp (exp1 exp2 exp3)
                         (let ((a-vec (expval->vec (value-of exp1 env)))
                                (idx1 (expval->num (value-of exp2 env)))
                                (idx2 (expval->num (value-of exp3 env))))
                           (let ((the-vector (cases vec a-vec
                                                    (vectorr (head length)
                                                         (deref head)))))
                            (let ((temp (vector-ref the-vector idx1)))
                              (vector-set! the-vector idx1 (vector-ref the-vector idx2))
                              (vector-set! the-vector idx2 temp)))))
        (copyvector-exp (exp)
                        (let ((a-vec (expval->vec (value-of exp env))))
                          (let ((vectortobecopied (cases vec a-vec
                                                    (vectorr (head length)
                                                         (deref head)))))
                            (let ((copied-vector (vectorr (newref (vector-copy vectortobecopied)) (vector-length vectortobecopied) )))
                              (vec-val copied-vector)))))

        (vec-mult-exp (exp1 exp2)
                      (let ((vect1 (expval->vec (value-of exp1 env)))
                                (vect2 (expval->vec (value-of exp2 env))))
                        (vect-mul-helper-1 vect1 vect2)
                      ))

        (newqueue-exp (exp1)
                      (let ((length (expval->num (value-of exp1 env))))
                      
                            (queue-val (a-queue (new-vec length 0) (newref 0) length (newref 0)))))
                      
        
        (enqueue-exp (exp1 exp2)
                      (let ((aqueue (expval->queue (value-of exp1 env)))
                             (val (value-of exp2 env)))
                             (enqueue-helper aqueue val))
                        )
                        
                      
        
        (dequeue-exp (exp1)
                      (let ((aqueue (expval->queue (value-of exp1 env)))) 
                             (dequeue-helper aqueue))
                      )
        
        ( queue-size-exp (exp1)
                       (let ((aqueue (expval->queue (value-of exp1 env))))
                         (cases queue aqueue
                           (a-queue (theVector start size load)
                                    (num-val (deref load)))))
                      )

        ( peek-queue-exp (exp1)
                         (let ((aqueue (expval->queue (value-of exp1 env))))
                           (cases queue aqueue
                             (a-queue (theVector start size load)
                                      (if (= (deref load) 0)
                                          (num-val -1)
                                          (read-vec theVector (deref start)))))))
                                          
                                          
        
        ( queue-empty-exp (exp1)
                      (let ((aqueue (expval->queue (value-of exp1 env))))
                           (cases queue aqueue
                             (a-queue (theVector start size load)
                                      (if (= (deref load) 0)
                                          (bool-val #t)
                                          (bool-val #f)))))
                      )
        
        ( print-queue-exp (exp1)
                      (let ((aqueue (expval->queue (value-of exp1 env)))) 
                             (print-queue-helper-1 aqueue))
                      )
        
        

        
        
        
        
         

        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
 (define new-vec
    (lambda (length value)
      (let ((a-vector (make-vector length value)))
        (let
            ((new-vector  (newref a-vector)))
          (vectorr new-vector length)))))
  
  (define update-vec
    (lambda (vectrr idx value)
      (cases vec vectrr
        (vectorr (head length)
          (vector-set! (deref  head) idx value)))))
  (define read-vec  
    (lambda (vectrr idx)
      (cases vec vectrr
        (vectorr (head length)
          (vector-ref (deref head) idx))
    )))
  (define vect-mul-helper-1
    (lambda (vect1 vect2)
      (cases vec vect1
        (vectorr (head1 length1)
                  (cases vec vect2
                    (vectorr (head2 length2)
                              (if (not (= length1 length2))
                                  (eopl:error 'vec-mult "Vectors are in different length")
                                  (vect-mul-helper-2 vect1 vect2 (new-vec length1 0) length1 0))))))))
  (define vect-mul-helper-2
    (lambda (vect1 vect2 vector3 length index)
      (if (= index length)
          (vec-val vector3)
          (begin
          (update-vec vector3 index ( num-val ( * (expval->num (read-vec vect1 index)) (expval->num (read-vec vect2 index)))))
          (vect-mul-helper-2 vect1 vect2 vector3 length (+ 1 index))))))
                                  

  (define enqueue-helper
    (lambda (aqueue val)
      (cases queue aqueue
        (a-queue (theVector start size load)
                 (if (= size load)
                     (eopl:error 'queue-enqueue! "Queue is full")
                     (begin (update-vec theVector (modulo (+ (deref load) (deref start)) size) val)
                            (setref! load (+ (deref load) 1))))))))
  (define dequeue-helper
    (lambda (aqueue)
      (cases queue aqueue
        (a-queue (theVector start size load)
                 (if (= (deref load) 0)
                     (num-val -1)
                     (begin (let ((value (read-vec theVector (deref start))))
                                  (setref! load (- (deref load) 1))
                                  (setref! start (modulo (+ (deref start) 1) size))
                                  value)))))))
   (define print-queue-helper-1
     (lambda (aqueue)
      (cases queue aqueue
        (a-queue (theVector start size load)
                 (print-queue-helper-2 theVector (deref start) size (deref load) (deref load))))))
  
   (define print-queue-helper-2
     (lambda (theVector start size load iter)
       (if (= iter 1)
           (display (expval->num (read-vec theVector (modulo start size) )))
           (begin (display (expval->num (read-vec theVector (modulo start size) )))
                  (display ", ")
                  (print-queue-helper-2 theVector (+ 1 start) size load (- iter 1))))))
           
                              
                                  
                            
    

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
