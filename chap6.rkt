#lang plai-typed

;; 替换模型的两个问题：
;; - 多次遍历代码（替换、解释）
;; - problem with substitution, which is that it is defined in terms
;;   of representations of the program source
;; 环境模型
;; - 动态捕获问题
;; 对于标识符，希望知道：
;; - 是否绑定了？
;; - 何处被绑定的？
;; 动态作用域的问题？

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : number
  (cond
   [(empty? env) (error 'lookup "name not found")]
   [else (cond
          [(symbol=? for (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup for (rest env))])]))

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)])

(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
   [(empty? fds) (error 'get-fundef "reference to undefined function")]
   [else (let ([fd (first fds)])
           (if (symbol=? (fdC-name fd) name)
               fd
               (get-fundef name (rest fds))))]))

(define (interp [e : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [appC (f a) (let ([fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds))]
    [idC (n) (lookup n env)]))

(define fds (list
             (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
             (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
             (fdC 'const5 'x (numC 5))))

(test (interp (appC 'double (numC 5)) mt-env fds) 10)
(test (interp (appC 'const5 (numC 0)) mt-env fds) 5)
(test (interp (appC 'double (appC 'const5 (numC 0))) mt-env fds) 10)
(test (interp (appC 'quadruple (appC 'const5 (numC 0))) mt-env fds) 20)

;; 动态作用域实例
;; (interp (appC 'f1 (numC 3))
;;         mt-env
;;         (list (fdC 'f1 'x (appC 'f2 (numC 4)))
;;               (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
