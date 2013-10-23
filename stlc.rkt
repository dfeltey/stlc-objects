#lang racket
(require rackunit
         rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define is-a-type? (lambda (o) (is-a? o stlc-type%)))
;
;(define type-env%-contract
;  (class/c
;   (init-field
;    (lu (-> symbol? (is-a?/c stlc-type%))))
;   (lookup any/c)
;   (extend any/c)))
;
;(provide (contract-out 
;                       [type-env% type-env%-contract]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define stlc-type%
  (class object%
    (super-new)
    (inspect #f)))

(define arrow-type%
  (class stlc-type%
    (super-new)
    (inspect #f)
    (init-field arg-type result-type)))

(define boolean-type%
  (class stlc-type%
    (super-new)
    (inspect #f)))

(define stlc-term% 
  (class object% 
    (super-new)
    (abstract subst)
    (abstract eval)
    (abstract typecheck-in-env)
    (define/public (typecheck) 
      (send this typecheck-in-env (new type-env%)))))


(define stlc-bool%
  (class stlc-term%
    (super-new)
    (init-field value)
    (define/override (subst what for)
      this)
    (define/override (eval) this)
    (define/override (typecheck-in-env env)
      (new boolean-type%))))

(define stlc-app% 
  (class stlc-term%
    (super-new)
    (init-field fun arg)
    (define/override (subst what for)
      (make-object stlc-app%
        (send fun subst what for)
        (send arg subst what for)))
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



(define stlc-lambda% 
  (class stlc-term%
    (super-new)
    (init-field name type body)
    (define/override (subst what for)
      (if (symbol=? for name)
          this
          (make-object stlc-lambda% 
            name
            type
            (send body subst what for))))
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


(define stlc-local% 
  (class stlc-term%
    (super-new)
    (init-field name named-expr body)
    (define/override (subst what for)
      (make-object stlc-local%
        name 
        (send named-expr subst what for)
        (if (symbol=? for name)
            body
            (send body subst what for))))        
    (define/override (eval)
      (send+ body (subst named-expr name) (eval)))
    (define/override (typecheck-in-env env)
      (send body 
            typecheck-in-env 
            (send env extend name (send named-expr typecheck-in-env env))))))

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

(define (parse sexpr)
  (match sexpr
    ['true (make-object stlc-bool% true)]
    ['false (make-object stlc-bool% false)]
    [(list fun arg) (make-object stlc-app% (parse fun) (parse arg))]
    [(list 'lambda (list (? symbol? name) ': type) body)
     (make-object stlc-lambda% name (parse-type type) (parse body))]
    [(list 'local (list (? symbol? name) named-expr) body)
     (make-object stlc-local% name (parse named-expr) (parse body))]
    [(? symbol? name) (make-object stlc-var% name)]
    [else (error "bad syntax")]))

(define (parse-type t)
  (match t
    ['bool (new boolean-type%)]
    [(list arg-type '-> result-type) 
     (make-object arrow-type% (parse-type arg-type) (parse-type result-type))]
    [else (error "bad type syntax")]))

(define (string->sexpr str)
  (read (open-input-string str)))

(define (parse-string str) (parse (string->sexpr str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




