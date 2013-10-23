#lang typed/racket

(define-type STLC-Type<%>
  (Class))

(: stlc-type% STLC-Type<%>)
(define stlc-type%
  (class object%
    (super-new)
    (inspect #f)))


(define-type Arrow-Type<%>
  (Class #:implements STLC-Type<%>
         (init [arg-type (Instance STLC-Type<%>)]
               [result-type (Instance STLC-Type<%>)])
         (field [arg-type (Instance STLC-Type<%>)]
               [result-type (Instance STLC-Type<%>)])))

(: arrow-type% Arrow-Type<%>)
(define arrow-type%
  (class stlc-type%
    (super-new)
    (inspect #f)
    (init-field arg-type result-type)))


(define-type Boolean-Type<%>
  (Class #:implements STLC-Type<%>))

(: boolean-type% Boolean-Type<%>)
(define boolean-type%
  (class stlc-type%
    (super-new)
    (inspect #f)))


(define-type Type-Env<%>
  (Class
   (init [lu (Symbol -> (Instance STLC-Type<%>)) #:optional])
   (field [lu (Symbol -> (Instance STLC-Type<%>))])
   [lookup (Symbol -> (Instance STLC-Type<%>))]
   [extend (Symbol (Instance STLC-Type<%>) -> (Instance Type-Env<%>))]))
    
(: type-env% Type-Env<%>)
(define type-env%
  (class object%
    (super-new)
    (init-field (lu (lambda (x) (error "untyped identifier"))))
    (define/public (lookup x) (lu x))
    (define/public (extend name type)
      (make-object type-env%
        (lambda (x) 
          (if (symbol=? x name)
              type
              (lookup x)))))))



(define-type STLC-Term<%>
  (Class [subst ((Instance STLC-Term<%>)
                 Symbol
                 ->
                 (Instance STLC-Term<%>))]
         [eval (-> (Instance STLC-Term<%>))]
         [typecheck-in-env ((Instance Type-Env<%>) 
                            -> 
                            (Option (Instance STLC-Type<%>)))]
         [typecheck (-> (Option (Instance STLC-Type<%>)))]))

(: stlc-term% STLC-Term<%>)
(define stlc-term% 
  (class object% 
    (super-new)
    (define/public (subst what for) (error "cannot use raw stlc-term%"))
    (define/public (eval) (error "cannot use raw stlc-term%"))
    (define/public (typecheck-in-env env) (error "cannot use raw stlc-term%"))
    (define/public (typecheck) 
      (send this typecheck-in-env (new type-env%)))))

(define-type STLC-Bool<%>
  (Class #:implements STLC-Term<%>
         (init [value Boolean])
         (field (value Boolean))))

(: stlc-bool% STLC-Bool<%>)
(define stlc-bool%
  (class stlc-term%
    (super-new)
    (init-field value)
    (define/override (subst what for)
      this)
    (define/override (eval) this)
    (define/override (typecheck-in-env env)
      (new boolean-type%))))


(define-type STLC-App<%>
  (Class #:implements STLC-Term<%>
         (init [fun (Instance STLC-Term<%>)]
               [arg (Instance STLC-Term<%>)])
         (field [fun (Instance STLC-Term<%>)]
               [arg (Instance STLC-Term<%>)])))
               
(: stlc-app% STLC-App<%>)
(define stlc-app% 
  (class stlc-term%
    (super-new)
    (init-field fun arg)
    (define/override (subst what for)
      (new stlc-app%
        [fun (send fun subst what for)]
        [arg (send arg subst what for)]))
    (define/override (eval) 
      (define fun-eval (send fun eval))
      (if (is-a? fun-eval stlc-lambda%)
          (let ([name (get-field name fun-eval)]
                [body (get-field body fun-eval)])
            (send+ body (subst arg name) (eval)))
          (error "invalid application")))
    (define/override (typecheck-in-env env)
      (let ([fun-type (send fun typecheck-in-env env)]
            [arg-type (send arg typecheck-in-env env)])
        (if (and (is-a? fun-type arrow-type%)
                 (equal? (get-field arg-type fun-type)
                         arg-type))
            (get-field result-type fun-type)
            #f)))))

(define-type STLC-Lambda<%>
  (Class #:implements STLC-Term<%>
         (init [name Symbol]
               [type (Instance STLC-Type<%>)]
               [body (Instance STLC-Term<%>)])
         (field [name Symbol]
                [type (Instance STLC-Type<%>)]
                [body (Instance STLC-Term<%>)])))

(: stlc-lambda% STLC-Lambda<%>)
(define stlc-lambda% 
  (class stlc-term%
    (super-new)
    (init-field name type body)
    (define/override (subst what for)
      (if (symbol=? for name)
          this
          (new stlc-lambda% 
            [name name]
            [type type]
            [body (send body subst what for)])))
    (define/override (eval) 
      (if (send this typecheck)
          this
          (error "type error")))
    (define/override (typecheck-in-env env)
      (define extended-env (send env extend name type))
      (define body-type (send body typecheck-in-env extended-env))
      (if body-type
          (make-object arrow-type% type body-type)
          #f))))

;(define stlc-local% 
;  (class stlc-term%
;    (super-new)
;    (init-field name named-expr body)
;    (define/override (subst what for)
;      (make-object stlc-local%
;        name 
;        (send named-expr subst what for)
;        (if (symbol=? for name)
;            body
;            (send body subst what for))))        
;    (define/override (eval)
;      (send+ body (subst named-expr name) (eval)))
;    (define/override (typecheck-in-env env)
;      (send body 
;            typecheck-in-env 
;            (send env extend name (send named-expr typecheck-in-env env))))))
;
;(define stlc-var% 
;  (class stlc-term%
;    (super-new)
;    (init-field name)
;    (define/override (subst what for)
;      (if (symbol=? for name)
;          what
;          this))
;    (define/override (eval) (error "unbound identifier"))
;    (define/override (typecheck-in-env env)
;      (send env lookup name))))
