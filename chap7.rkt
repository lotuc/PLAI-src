#lang plai-typed

;; 程序语言的设计不应该是特性的简单堆砌，而应消除语言的弱点和缺陷，使得剩下的特性显得必要。
;; 闭包的处理，“记住调用过程中执行的替换操作”

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "name not found")]
   [else (cond
          [(symbol=? for (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup for (rest env))])]))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (num+ [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (+ (numV-n l) (numV-n r)))]
   [else
    (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (* (numV-n l) (numV-n r)))]
   [else
    (error 'num* "one argument was not a number")]))

(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [appC (f a) (let ([fd (interp f env)])
                  (interp (funV-body fd)
                          (extend-env (bind (funV-arg fd)
                                            (interp a env))
                                      mt-env)))]
    [fdC (n a b) (funV n a b)]
    [idC (n) (lookup n env)]))


(test (interp (plusC (numC 10) (numC 5))
              mt-env)
      (numV 15))

;; 闭包处理出错
(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "name not found")
