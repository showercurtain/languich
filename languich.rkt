#lang racket
(require racket/pretty)

(provide (all-defined-out))

'(var a (lambda (text) (print "asdf" text)))

(define (evaluate expr env)
  (match expr
    [(? number? n) n]
    [(? string? n) n]
    [(? boolean? n) n]
    ;    [`(+ ,arg1 ,arg2 ,arg3 ...) (pretty-print `(arg1 ,arg1 arg2 ,arg2 arg3 ,arg3))]
    [`(+ ,arg1 ,arg2 ,arg3 ...)
     (foldl + (evaluate arg1 env) (map (lambda (arg-expr) (evaluate arg-expr env)) (cons arg2 arg3)))]
    [`(* ,arg1 ,arg2 ,arg3 ...)
     (foldl * (evaluate arg1 env) (map (lambda (arg-expr) (evaluate arg-expr env)) (cons arg2 arg3)))]
    [`(/ ,arg1 ,arg2 ,arg3 ...)
     (/ (evaluate arg1 env) (foldl * (evaluate arg2 env) (map (lambda (arg-expr) (evaluate arg-expr env)) arg3)))]
    [`(- ,arg1 ,arg2 ,arg3 ...)
     (- (evaluate arg1 env) (foldl + (evaluate arg2 env) (map (lambda (arg-expr) (evaluate arg-expr env)) arg3)))]

    ;[`(,arg1 * ,arg2) (* (evaluate arg1 env) (evaluate arg2 env))]
    [`(if ,condit ,t-case) (if (evaluate condit env) (evaluate t-case env) #f)]
    [`(if ,condit ,t-case ,f-case) (if (evaluate condit env) (evaluate t-case env) (evaluate f-case env))]
    [`(<= ,arg1 ,arg2) (<= (evaluate arg1 env) (evaluate arg2 env))]
    [`(>= ,arg1 ,arg2) (>= (evaluate arg1 env) (evaluate arg2 env))]
    [`(== ,arg1 ,arg2) (= (evaluate arg1 env) (evaluate arg2 env))]
    [`(print ,arg1 ...) (last (map (lambda (arg-expr) (print (evaluate arg-expr env))) arg1))]
    [`(println ,arg1 ...) (last (map (lambda (arg-expr) (println (evaluate arg-expr env))) arg1))]
    [(? symbol? n) (lookup-env env n)]
    [`(var ,name ,value) (set-env env name (evaluate value env))]
    [`(exec ,expr ...) (last (map (lambda (arg) (evaluate arg env)) expr))]
    ))

(define (new-env) (make-hash))
(define (lookup-env env name) (hash-ref env name))
(define (set-env env name value) (hash-set! env name value))
