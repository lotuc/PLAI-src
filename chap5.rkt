#lang plai-typed

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

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                          (subst what for r))]
    [multC (l r) (multC (subst what for l)
                          (subst what for r))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [appC (f a) (let ([fd (get-fundef f fds)])
                  (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    [idC (_) (error 'interp "shouldn't get here")]))

(define fds (list
             (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
             (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
             (fdC 'const5 'x (numC 5))))

(test (interp (appC 'double (numC 5)) fds) 10)
(test (interp (appC 'const5 (numC 0)) fds) 5)
(test (interp (appC 'double (appC 'const5 (numC 0))) fds) 10)
(test (interp (appC 'quadruple (appC 'const5 (numC 0))) fds) 20)
