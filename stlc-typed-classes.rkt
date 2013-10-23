#lang typed/racket

(define-type STLC-Type<%>
  (Class [type-apply ((Option (Instance STLC-Type<%>)) 
                      -> 
                      (Option (Instance STLC-Type<%>)))]))

(: stlc-type% STLC-Type<%>)
(define stlc-type%
  (class object%
    (super-new)
    (inspect #f)
    (define/public (type-apply to) #f)))


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
    (init-field arg-type result-type)
    (define/override (type-apply to)
      (and to ; arg-type is always constructed by parse-type
              ; ie arg-type is never #f
              ; so this check should not actually be necessary
           (equal? to arg-type)
           result-type))))



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
   [extend (Symbol (Option (Instance STLC-Type<%>)) -> (Instance Type-Env<%>))]))

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
         [apply ((Instance STLC-Term<%>) -> (Instance STLC-Term<%>))]
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
    (define/public (apply to) (error "cannot apply a a non lambda term"))
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
      (send+ fun (eval) (apply (send arg eval))))    
    (define/override (typecheck-in-env env)
      (define fun-type (send fun typecheck-in-env env))
      (and fun-type
          (send fun-type type-apply (send arg typecheck-in-env env))))))
                 
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
    (define/override (apply to)
      (send+ body (subst to name) (eval)))
    (define/override (eval) this)
    (define/override (typecheck-in-env env)
      (define extended-env (send env extend name type))
      (define body-type (send body typecheck-in-env extended-env))
      (and body-type
          (new arrow-type% [arg-type type] [result-type body-type])))))

(define-type STLC-Local<%>
  (Class #:implements STLC-Term<%>
         (init [name Symbol]
               [named-expr (Instance STLC-Term<%>)]
               [body (Instance STLC-Term<%>)])
         (field [name Symbol]
                [named-expr (Instance STLC-Term<%>)]
                [body (Instance STLC-Term<%>)])))

(: stlc-local% STLC-Local<%>)
(define stlc-local% 
  (class stlc-term%
    (super-new)
    (init-field name named-expr body)
    (define/override (subst what for)
      (new stlc-local%
           [name name] 
           [named-expr (send named-expr subst what for)]
           [body (if (symbol=? for name)
                     body
                     (send body subst what for))]))        
    (define/override (eval)
      (send+ body (subst named-expr name) (eval)))
    (define/override (typecheck-in-env env)
      (send body 
            typecheck-in-env 
            (send env extend name (send named-expr typecheck-in-env env))))))

(define-type STLC-Var<%>
  (Class #:implements STLC-Term<%>
         (init [name Symbol])
         (field [name Symbol])))

(: stlc-var% STLC-Var<%>)
(define stlc-var% 
  (class stlc-term%
    (super-new)
    (init-field name)
    (define/override (subst what for)
      (if (symbol=? for name)
          what
          this))
    (define/override (eval) (error "unbound identifier"))
    (define/override (typecheck-in-env env)
      (send env lookup name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: parse (Any -> (Instance STLC-Term<%>)))
(define (parse sexpr)
  (match (cast sexpr Sexp)
    ['true (new stlc-bool% [value true])]
    ['false (new stlc-bool% [value false])]
    [(list fun arg) (new stlc-app% 
                         [fun (parse fun)]
                         [arg (parse arg)])]
    [(list 'lambda (list (? symbol? name) ': type) body)
     (new stlc-lambda% 
          [name name] 
          [type (parse-type type)]
          [body (parse body)])]
    [(list 'local (list (? symbol? name) named-expr) body)
     (new stlc-local% 
          [name name] 
          [named-expr (parse named-expr)]
          [body (parse body)])]
    [(? symbol? name) (new stlc-var% [name name])]
    [else (error "bad syntax")]))

(: parse-type (Sexp -> (Instance STLC-Type<%>)))
(define (parse-type t)
  (match t
    ['bool (new boolean-type%)]
    [(list arg-type '-> result-type) 
     (new arrow-type% 
          [arg-type (parse-type arg-type)]
          [result-type (parse-type result-type)])]
    [else (error "bad type syntax")]))

(: string->sexpr (String -> Any))
(define (string->sexpr str)
  (read (open-input-string str)))

(: parse-string (String -> (Instance STLC-Term<%>)))
(define (parse-string str) (parse (string->sexpr str)))
