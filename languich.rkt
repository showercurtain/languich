#lang racket
(require racket/pretty)

(provide (all-defined-out))

(struct env-pad (local-pad parent-pad) #:transparent #:mutable)
(struct lamb (params argsname body parent-pad) #:transparent #:mutable)

(define (evaluate expr [env (new-parent-env)])
  (match expr
    [(? number? n) n]
    [(? string? n) n]
    [(? boolean? n) n]
    [(? procedure? n) (n env)]
    ;    [`(+ ,arg1 ,arg2 ,arg3 ...) (pretty-print `(arg1 ,arg1 arg2 ,arg2 arg3 ,arg3))]
    [`(+ ,arg1 ,arg2 ,arg3 ...)
     (foldl + (evaluate arg1 env) (map (lambda (arg-expr) (evaluate arg-expr env)) (cons arg2 arg3)))]
    [`(* ,arg1 ,arg2 ,arg3 ...)
     (foldl * (evaluate arg1 env) (map (lambda (arg-expr) (evaluate arg-expr env)) (cons arg2 arg3)))]
    [`(/ ,arg1 ,arg2 ,arg3 ...)
     (/ (evaluate arg1 env) (foldl * (evaluate arg2 env) (map (lambda (arg-expr) (evaluate arg-expr env)) arg3)))]
    [`(- ,arg1 ,arg2 ,arg3 ...)
     (- (evaluate arg1 env) (foldl + (evaluate arg2 env) (map (lambda (arg-expr) (evaluate arg-expr env)) arg3)))]
    [`(% ,arg1 ,arg2) (remainder (evaluate arg1 env) (evaluate arg2 env))]

    ;[`(,arg1 * ,arg2) (* (evaluate arg1 env) (evaluate arg2 env))]
    [`(if ,condit ,t-case) (if (evaluate condit env) (evaluate t-case env) #f)]
    [`(if ,condit ,t-case ,f-case) (if (evaluate condit env) (evaluate t-case env) (evaluate f-case env))]
    [`(<= ,arg1 ,arg2) (<= (evaluate arg1 env) (evaluate arg2 env))]
    [`(>= ,arg1 ,arg2) (>= (evaluate arg1 env) (evaluate arg2 env))]
    [`(== ,arg1 ,arg2) (= (evaluate arg1 env) (evaluate arg2 env))]
    [`(print ,arg1 ...) (last (map (lambda (arg-expr) (display (evaluate arg-expr env))) arg1))]
    [`(println ,arg1 ...) 
        (let
          ([output (last (map (lambda (arg-expr) (display (evaluate arg-expr env))) arg1))])
          (display "\n")
          output)]
    [(? symbol? n) (lookup-env env n)]
    [`(var ,name) (add-env env name)]
    [`(var ,name ,value) (add-env env name (evaluate value env))]
    [`(set ,name ,value) (set-env env name (evaluate value env))]
    [`(exec ,expr ...) (last (map (lambda (arg) (evaluate arg env)) expr))]
    [`(lambda (,params ... ,argsname *) ,body) (lamb params argsname body env)]
    [`(lambda (,params ...) ,body) (lamb params null body env)]
    [`(,fexpr ,args ...) (let* ([fval (evaluate fexpr env)]
                                [argv (map (lambda (arg) (evaluate arg env)) args)]
                                [body-env (new-env (lamb-parent-pad fval))])
                           (if (not (null? (lamb-argsname fval)))
                              (add-env body-env (lamb-argsname fval) (list-tail argv (length (lamb-params fval)))) #f)
                           (map (lambda (varname value) (add-env body-env varname value))
                              (reverse (lamb-params fval))
                              (list-tail (reverse argv) (- (length argv) (length (lamb-params fval)))))
                           (evaluate (lamb-body fval) body-env))
                           ]
    ))

(define (new-env [parent-pad null]) (env-pad (make-hash) parent-pad))

(define (new-parent-env) (begin
  ; all them builtins go here
  (define env (new-env))
  (add-env env 'asdf (lamb '() null (lambda (env) (display "asdfghjk")) env))
  (add-env env 'looky-here (lamb '(text) null (lambda (env) (print (lookup-env env 'text))) env))
  env
))
(define (lookup-env env name)
  (hash-ref (env-pad-local-pad env) name
            (lambda () (if (null? (env-pad-parent-pad env))
                           (error "Undefined variable" name)
                           (lookup-env (env-pad-parent-pad env) name)))))

(define (add-env env name [value null])
  (hash-set! (env-pad-local-pad env) name value))

(define (set-env env name value)
  (hash-update! (env-pad-local-pad env) name (lambda (x) value)
                (lambda () (if (null? (env-pad-parent-pad env))
                               (error "Undefined variable" name)
                               (set-env (env-pad-parent-pad env) name value)))))

;(define (shell [env (new-env)]) (begin (display "> ") (evaluate (read-line (current-input-port) 'any) env) (shell env)))

;(shell)