#lang racket
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC)(arg : ExprC)]
  [fdC (name : symbol)(arg : symbol)(body : ExprC)]
  [binaryOpC (op : symbol)(l : ExprC)(r : ExprC)]
  [ifZeroC (pred : ExprC)(trueState : ExprC)(falseState : ExprC)])

;;Environment

(define EnvNameSpace
  (list
   (bind 'x (numV 5))
   (bind 'y (numV 6))
   (bind 'z (numV 7))
   ))

;;Contract- symbol (listof Bindings) -> number
;; Finds given symbol's value

(define (lookup [for : symbol] [env : Environment]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;; Contract - ExprC (listof FunDefC) -> number
;;Evaluates expressions to numbers.

(define (interp [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [appC (f a) (local ([define fd (interp f env)])
                  (interp (funV-body fd)
                          (extend-env (bind (funV-arg fd)
                                            (interp a env))
                                      mt-env)))]
    [binaryOpC (op l r)(BinaryOperationWrapper (get-op op)
                                               (interp l env)
                                               (interp r env))]
    [fdC (n a b) (funV n a b)]
    [ifZeroC (pred t f)
             (if (= 0 (numV-n (interp pred env)))
                 (interp t env)
                 (interp f env))]
    ))