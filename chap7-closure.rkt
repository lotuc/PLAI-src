#lang plai-typed

;; 程序语言的设计不应该是特性的简单堆砌，而应消除语言的弱点和缺陷，使得剩下的特性显得必要。
;; 闭包的处理，“记住调用过程中执行的替换操作”
;; 闭包值生成它时的环境

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

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
  [lamC (arg : symbol) (body : ExprC)])

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
                  (interp (closV-body fd)
                          (extend-env (bind (closV-arg fd)
                                            (interp a env))
                                      (closV-env fd))))]
    [lamC (a b) (closV a b env)]
    [idC (n) (lookup n env)]))


(test (interp (plusC (numC 10) (numC 5))
              mt-env)
      (numV 15))
(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                   (numC 4)))
                    (numC 3))
              mt-env)
      (numV 7))

;; 环境闭包能正确处理这种情况，不加注意的替换模型可能会导致语义错误，
;; 替换模型实现应该考虑命名问题避免错误捕获
;; (interp
;;  (appC
;;   (appC (lamC 'f (lamC 'x (appC (idC 'f) (numC 10))))
;;         (lamC 'y (plusC (idC 'x) (idC 'y))))
;;   (numC 5))
;; mt-env)

;; let: left-left-lambda sugar
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (s : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [lamS (arg : symbol) (body : ExprS)]
  [letS (n : symbol) (v : ExprS) (body : ExprS)])

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [idS (s) (idC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [lamS (a b) (lamC a (desugar b))]
    [letS (n v b) (appC (lamC n (desugar b)) (desugar v))]))

(define (interp-sugar [e : ExprS] [env : Env]) : Value
  (interp (desugar e) env))

(test (interp-sugar (plusS (numS 10) (numS 5))
                    mt-env)
      (numV 15))

(test (interp-sugar
       (letS 'double (lamS 'x (plusS (idS 'x) (idS 'x)))
             (appS (idS 'double) (numS 2)))
       mt-env)
      (numV 4))

(test (interp-sugar
       (letS 'double (lamS 'x (plusS (idS 'x) (idS 'x)))
             (letS 'quadruple
                   (lamS 'x (appS (idS 'double) (appS (idS 'double) (idS 'x))))
                   (appS (idS 'quadruple) (numS 2))))
       mt-env)
      (numV 8))

;; 死循环
;; (interp
;;  (appC (lamC 'forever
;;              (appC (idC 'forever) (idC 'forever)))
;;        (lamC 'forever
;;              (appC (idC 'forever) (idC 'forever))))
;;  mt-env)

;; let 形式
;; (interp-sugar
;;  (letS 'loop-forever (lamS 'f (appS (idS 'f) (idS 'f)))
;;        (appS (idS 'loop-forever) (idS 'loop-forever)))
;;  mt-env)

;; 下一节考虑递归
