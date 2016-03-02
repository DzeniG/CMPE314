#lang plai-typed
;;Data definition oldl (old language)
;;nwl is a number or it is a 
;;addition of two oldl
;;subtraction of two oldl
;;multiplication of two oldl
;;exponent of two oldl

(define-type oldl
  [oldl-num (n : number)]
  [oldl-identf (sim : symbol)]
  [oldl-applic (funct : symbol)(argument : oldl)]
  [oldl-add (left : oldl) (right : oldl)]
  [oldl-sub (left : oldl) (right : oldl)]
  [oldl-mul (left : oldl) (right : oldl)]
  [oldl-exp (left : oldl) (right : oldl)]
  [if-grater-than-zero (req : oldl)(iftrue : oldl)(iffalse : oldl)]
  )

;;Function definition functionDef
;;It is composed of three parts :
;;name by which function is called (in our case it is symbol)
;;argument (formal parameter) that function is going to take
;;body which is an oldl
;;an important point is that body of a function does not hold variables but identifiers (they do not change)
(define-type functionDef
  [funcd (name : symbol)(argument : symbol)(body : oldl)])

;;exponential function

(define (** p t)
  (cond
    ((= p 1) t)
    (else
    (* t(**(- p 1) t)))))

;;parser s-expression -> oldl
;;parses an s-expression but this time it parses function identifier, function application and if-grater-than-zero
;;examples
;;(parse (number->s-exp 5))(oldl-num 5)
;;(parse (symbol->s-exp 'x))(oldl-identf 'x)
;;(parse '(+ 3 4))(oldl-add (oldl-num 3)(oldl-num 4))
;;(parse '(* 3 4))(oldl-mul (oldl-num 3)(oldl-num 4))
;;(parse '(+ x x))(oldl-add (oldl-identf 'x)(oldl-identf 'x))
;;(parse '(* x x))(oldl-mul (oldl-identf 'x)(oldl-identf 'x))
;;(parse '(f (* x x)))(oldl-applic 'f (oldl-mul (oldl-identf 'x)(oldl-identf 'x)))

(define (parse [s : s-expression]) : oldl
  (cond
    [(s-exp-number? s) (oldl-num(s-exp->number s))]
    [(s-exp-symbol? s) (oldl-identf (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(> (length sl) 2)
          (case (s-exp->symbol (first sl))
            [(+) (oldl-add (parse (second sl)) (parse (third sl)))]
            [(*) (oldl-mul (parse (second sl)) (parse (third sl)))]
            [(-) (oldl-sub (parse (second sl)) (parse(third sl)))]
            [(**) (oldl-exp (parse (second sl)) (parse (third sl)))]
            [(if) (if-grater-than-zero (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
            [else (error 'parse "invalid list input!")]
            )]
         [(= (length sl) 2)
          (oldl-applic (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid number of inputs")]
         ))]
    [else (error 'parse "invalid input!")]))



"Output examples"
(parse (number->s-exp 5))
(parse (symbol->s-exp 'x))
(parse '(+ 3 4))
(parse '(* 3 4))
(parse '(+ x x))
(parse '(* x x))
(parse '(f (* x x)))

"Tests"
(test (parse (number->s-exp 5))(oldl-num 5))
(test (parse (symbol->s-exp 'x))(oldl-identf 'x))
(test (parse '(+ 3 4))(oldl-add (oldl-num 3)(oldl-num 4)))
(test (parse '(* 3 4))(oldl-mul (oldl-num 3)(oldl-num 4)))
(test (parse '(+ x x))(oldl-add (oldl-identf 'x)(oldl-identf 'x)))
(test (parse '(* x x))(oldl-mul (oldl-identf 'x)(oldl-identf 'x)))
(test (parse '(f (* x x)))(oldl-applic 'f (oldl-mul (oldl-identf 'x)(oldl-identf 'x))))


;;interpreter oldl->number
;;interprets an oldl expression and a list of function definitions
;;examples
;;(oldl-num 7) -> 7
;;(oldl-add (oldl-num 7) (oldl-num 3))->10
;;(oldl-add (oldl-add (oldl-num 6) (oldl-num 4)) (oldl-mul (oldl-num 2) (oldl-num 7)))->24
;;(oldl-exp (oldl-num 2) (oldl-num 3))->9
;;(oldl-exp (oldl-add (oldl-num 2) (oldl-num 3)) (oldl-sub (oldl-num 3) (oldl-num 1)))->32
(define (interpreter [g : oldl] [funcdefs : (listof functionDef)]) : number
  (type-case oldl g
    [oldl-num (n) n]
    [oldl-identf (_) (error 'interpreter "is not supposed to get here")]
    [oldl-applic (fun arg) (local ([define fd (get-functdef fun funcdefs)])
                  (interpreter (subst arg 
                           (funcd-argument fd)
                           (funcd-body fd))
                          funcdefs))]
    [oldl-add (lt rt) (+ (interpreter lt funcdefs) (interpreter rt funcdefs))]
    [oldl-sub (lt rt) (- (interpreter lt funcdefs) (interpreter rt funcdefs))]
    [oldl-mul (lt rt) (* (interpreter lt funcdefs) (interpreter rt funcdefs))]
    [oldl-exp (lt rt) (** (interpreter lt funcdefs) (interpreter rt funcdefs))]
    [if-grater-than-zero (req t f)(if (> 0 (interpreter req funcdefs))
                                      (interpreter t funcdefs)
                                      (interpreter f funcdefs))]))

"Example outputs"
(oldl-num 32)
(oldl-num? (oldl-num 42))
(oldl-add? (oldl-add (oldl-num 2) (oldl-num 3)))

(interpreter (oldl-num 7) empty)
(interpreter (oldl-add (oldl-num 7) (oldl-num 3)) empty)
(interpreter (oldl-add (oldl-add (oldl-num 6) (oldl-num 4)) (oldl-mul (oldl-num 2) (oldl-num 7))) empty)
(interpreter (oldl-exp (oldl-num 2) (oldl-num 3)) empty)
(interpreter (oldl-exp (oldl-add (oldl-num 2) (oldl-num 3)) (oldl-sub (oldl-num 3) (oldl-num 1))) empty)
(interpreter (parse '(+ 3 4)) empty)
(interpreter (parse (number->s-exp 3)) empty)

"Tests"
(test(interpreter (oldl-num 7) empty) 7)
(test(interpreter (oldl-add (oldl-num 7) (oldl-num 3)) empty) 10)
(test(interpreter (oldl-add (oldl-add (oldl-num 6) (oldl-num 4)) (oldl-mul (oldl-num 2) (oldl-num 7))) empty) 24)
(test(interpreter (oldl-exp (oldl-num 2) (oldl-num 3)) empty) 9)
(test(interpreter (parse (number->s-exp 3)) empty) 3)
(test(interpreter (parse '(+ 3 4)) empty) 7)
(test(interpreter (oldl-exp (oldl-add (oldl-num 2) (oldl-num 3)) (oldl-sub (oldl-num 3) (oldl-num 1))) empty) 32)



;;get-funcdef symbol, listof function definitios -> function definition

(define(get-functdef [n : symbol] [funcdefs : (listof functionDef)]) : functionDef
  (cond
    [(empty? funcdefs)(error 'get-functdef "references to undefined function")]
    [(cons? funcdefs) (cond
                        [(equal? n (funcd-name (first funcdefs)))(first funcdefs)]
                        [else (get-functdef n (rest funcdefs))])]))



;;subst oldl, symbol, oldl -> oldl
;;subst is in charge of substituting (replacing) the formal parameter by the actual parameter
;;so that interpreter is able to perform (execute / calculate) the operation on value that is passed to the function

(define(subst [what : oldl][for : symbol][in : oldl]) : oldl
  (type-case oldl in
    [oldl-num (n) in]
    [oldl-identf (sim) (cond
                        [(symbol=? sim for) what]
                        [else in])]
    [oldl-applic (funct argument) (oldl-applic funct (subst what for argument))]
    [oldl-add (lt rt) (oldl-add (subst what for lt) (subst what for rt))]
    [oldl-sub (lt rt) (oldl-sub (subst what for lt) (subst what for rt))]
    [oldl-mul (lt rt) (oldl-mul (subst what for lt) (subst what for rt))]
    [oldl-exp (lt rt) (oldl-add (subst what for lt) (subst what for rt))]
    [if-grater-than-zero (req t f)(if-grater-than-zero(subst what for req)(subst what for t)(subst what for f))]))


