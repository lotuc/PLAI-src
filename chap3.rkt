#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression])
  (cond
   [(s-exp-number? s) (numC (s-exp->number s))]
   [(s-exp-list? s)
    (let ([s1 (s-exp->list s)])
      (case (s-exp->symbol (first s1))
        [(+) (plusC (parse (second s1)) (parse (third s1)))]
        [(*) (multC (parse (second s1)) (parse (third s1)))]))]
   [else (error 'parse "invalid input")]))


(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (parse '(+ (* 2 3) (+ 3 4)))) 13)
