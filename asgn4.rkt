#lang typed/racket
(require typed/rackunit)


;;;; ---- NOTES ----

; We fully implemented assignment 4
; Code is organized as follows
; 1) type definitions
; 2) top-interp and interp
; 3) parsing and its helper functions
; 4) interpreting and its helper functions
; 5) testing




;;;; ---- TYPE DEFINITIONS ----


;; ExprC is either:
;; - one of the following arithmatic expressions: plus, subtract, multiply, divide
;;       which are represented by their symbols (+, -, *, /)
;; - an id, represented as a symbol
;; - a function application, represented by an id and an ExprC.
;;       The id corresponds to the name of the function, and the ExprC
;;       corresponds to the expression being passed into the function as the argument

(define-type ExprC (U numC binopC idC FunappC ifleq0?))

(struct numC ([n : Real]) #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct FunappC ([id : idC] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0? ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)


;; FunDefC is a function defintion, represented by:
;;  - an idC corresponding to the name of the function
;;  - an ExprC corresponding to the function body

(struct FundefC ([id : idC] [args : (Listof idC)] [body : ExprC]) #:transparent)


;; binop-s-list is a list of the binop operators
;;   used to check if a symbol is a binop in parse
(define binop-s-list (list '+ '- '* '/))




;;;; ---- TOP-INTERP and INTERP ----


;; top-interp
;; - accepts an s-expression
;; - calls parse-prog with the s-expression
;; - then interprets the results of parse-prog with interp-fns
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;; interp
;; - given an ExprC and a list of FundefC
;; - evaluates the given expression using the function definitions
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(numC n) n]
    [(binopC op l r) (interp-binop op l r funs)]
    [(FunappC name args) (interp-func (find-func name funs) args funs)]
    [(ifleq0? test-expr then-expr else-expr)
     (cond 
         [(<= (interp test-expr funs) 0) (interp then-expr funs)]
         [else (interp else-expr funs)])]
    [(idC s) (error 'interp
                    "OAZO runtime error in interp: invalid expression, tried to evaluate symbol ~e"
                    exp)]))




;;;; ---- PARSING ----


;; parse-prog
;; - is given concrete syntax in the form of an s-expression
;; - then parses it into list of FundefCs.
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
  [(cons f r) (cons (parse-fundef f) (parse-prog r))]
  ['() '()]))


;; parse-fundef
;; - given concrete syntax of a function definition in the form of an s-expression
;; - parses it into a funC
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'func (list name-expr) : body-expr)
     (FundefC (parse-id name-expr) '() (parse body-expr))]
    [(list 'func (cons name-expr rest-exprs) ': body-expr)
     (FundefC (parse-id name-expr) (parse-func-def-exprs rest-exprs '()) (parse body-expr))]
    [other (error 'parse-fundef "OAZO syntax error in parse-fundef: expected valid syntax, got ~e" other)]))
  

;; parse
;; - given concrete syntax in the form of an s-expression
;; - parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? x) (numC x)]
    [(? symbol? symb)
     (cond
       [(reserved-name? symb) (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" symb)]
       [(idC symb)])]
    [(list s l r) #:when
                   (argInList? s binop-s-list)
                   (cond
                     [(or (reserved-name? l) (reserved-name? r))
                      (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" l)]
                     [else (binopC (cast s Symbol) (parse l) (parse r))])]
    [(list 'ifleq0? test-expr then-expr else-expr) (ifleq0? (parse test-expr) (parse then-expr) (parse else-expr))
     (cond
       [(or (reserved-name? test-expr) (reserved-name? then-expr) (reserved-name? else-expr))
        (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e ~e ~e" test-expr then-expr else-expr)]
       [else (ifleq0? (parse test-expr) (parse then-expr) (parse else-expr))])]
    [(cons name rest)
     (cond
       [(reserved-name? name) (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" name)]
       [else (FunappC (parse-id name) (parse-func-app-exprs rest))])]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" other)]))


;; parse-func-def-exprs is given a S-expression
;; and turns it into a list of idCs
(define (parse-func-def-exprs [exprs-list : Sexp] [dup-check-list : (Listof Sexp)]) : (Listof idC)
  (match exprs-list
    ['() '()] 
    [(cons first-arg rest-args)
     (cond
       [(reserved-name? first-arg)
        (error 'parse-func-def-exprs
               "OAZO syntax error in parse-func-def-exprs: invalid function argument name, got ~e" first-arg)]
       [(argInList? first-arg dup-check-list)
        (error 'parse-func-def-exprs
               "OAZO syntax error in parse-func-def-exprs: found duplicate function argument name of ~e" first-arg)]
       [else (cons (parse-id first-arg) (parse-func-def-exprs rest-args (cons first-arg dup-check-list)))])]))


;; argInList? takes any one element, and a list of elements,
;; and returns true if the element is in the list, and false otherwise
(define (argInList? [arg : Any] [search-list : (Listof Any)]) : Boolean
  (match search-list
    ['() #f]
    [(cons f r) (cond
                  [(equal? f arg) #t]
                  [else (argInList? arg r)])]))


;; parse-func-app-exprs is given an S-expression
;; and parses it in into a list of expressions for a funciton applicatioin
(define (parse-func-app-exprs [expr-list : Sexp]) : (Listof ExprC)
  (match expr-list
    ['() '()]
    [(cons f '()) (cons (parse f) '())]
    [(cons f r) (cons (parse f) (parse-func-app-exprs r))]))


;; parse-id
;; - given a Symbol and returns an idC
(define (parse-id [s : Sexp]) : idC
  (match s
    [(? symbol? s)
     (cond
       [(reserved-name? s)
        (error 'parse-id "OAZO syntax error in parse-id: expected valid id, got ~e" s)]
       [else (idC s)])]
    [other (error 'parse-id "OAZO syntax error in parse-id: expected valid id, got ~e" other)]))


;; reserved-name? - Helper function - consumes a Symbol
;; - returns true if Symbol given is built into the language, and thus reserved
(define (reserved-name? name)
  (or (equal? '+ name)
      (equal? '- name)
      (equal? '* name)
      (equal? '/ name)
      (equal? 'func name)
      (equal? 'ifleq0? name)
      (equal? ': name)
      (equal? 'else name)))




;;;; ---- INTERPRETING


;; interp-fns
;; - given a list of FundefCs
;; - iterates through the list until it finds the function name 'main which it then evaluates
;; - If no function named 'main is found, raises an error
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp-fns-iterater funs funs))


;; interp-fns-iterator is a recursive Helper Function for interp-fns which
;; - Consumes two lists of function definitions:
;;     1) searching as the function defs we will search for 'main in
;;     2) all as the record of all function defs in the program
;; - Iterates through searching to find a function 'main, which it then evaluates
;; - If no function named 'main is found, raises an error
(define (interp-fns-iterater [searching : (Listof FundefC)] [all : (Listof FundefC)]) :  Real
  (match searching
    [(cons (FundefC name args body) rest-of-funcs) #:when (equal? name (idC 'main))
                                                   (interp-func (FundefC name args body) '() all)]
    [(cons first-func rest-of-funcs) (interp-fns-iterater rest-of-funcs all)]
    ['() (error 'interp-fns-iterater "OAZO runtime error in interp-fns-iterater:
          no main function defined in ~e" all)]))


;; interp-func is given a FundefC, a list of ExprCs, and a list of functions,
;; then checks if there are the same number of arguments in the FundefC as the list of the ExprCs,
;; and if so evaluates the FundefC
(define (interp-func [fun : FundefC] [app-args : (Listof ExprC)] [funs : (Listof FundefC)]) : Real
  (match fun
    [(FundefC name def-args body)
     (cond
       [(equal? (length-of app-args) (length-of def-args))
        (interp-func-helper fun app-args funs)]
       [else (error 'interp-fns-iterater
                    "OAZO runtime error in interp-func:
                       application argument count differs from defintion argument count.
                       func trying to interp: ~e   app-args: ~e   def-args: ~e"
                    name app-args def-args)])]))


;; interp-func-helper is given
;; a FundefC (fun), a list of ExprCs (app-args), and a list of functions (funs),
;; then evaluates the FundefC by:
;; 1) evaluating the first ExprC of app-args (if there are no ExprCs, go to step 4)
;; 2) Substituting every instance of the first argument of the FundefC with the first Expr of app-args
;; 3) Repeating 1 & 2 for every following ExprC until there are none left
;; 4) Interpret the body of the FundefC
(define (interp-func-helper [fun : FundefC] [app-args : (Listof ExprC)] [funs : (Listof FundefC)]) : Real
  (match fun
    [(FundefC name '() body) (interp body funs)]
    [(FundefC name (cons first-def-arg rest-def-args) body)
     (interp-func-helper
      (FundefC name rest-def-args
               (subst name body (interp (first app-args) funs) first-def-arg))
      (rest app-args) funs)]))
 
 
;; length-of is given a list of anything and returns a number corresponding to the number of elements in the list
(define (length-of [l : (Listof Any)]) : Integer
  (match l
    ['() 0]
    [(cons f r) (+ 1 (length-of r))]))


;; interp-binop interprets a binop
(define (interp-binop [op : Symbol] [l : ExprC] [r : ExprC] [funs : (Listof FundefC)]) : Real
  (match op
       ['+ (+ (interp l funs) (interp r funs))]
       ['* (* (interp l funs) (interp r funs))]
       ['- (- (interp l funs) (interp r funs))]
       ['/
        (define left (interp l funs))
        (define right (interp r funs))
        (cond
          [(equal? right 0)
           (error 'interp-binop "OAZO runtime error in interp-binop: invalid expression, division by zero ~e" exp)]
          [else (/ left right)])]))


;; find-func
;; - given a function name and a list of functions
;; - returns the function within the given list that corresponds to the given name
;; - if no such function exists, raise an error
(define (find-func [name : idC] [funs : (Listof FundefC)]) : FundefC
  (match funs
  [(cons (FundefC n a b) r) #:when (equal? n name) (FundefC n a b)]
  [(cons f r) (find-func name r)]
  ['() (error 'find-func "OAZO runtime error in find-func:
        the function ~e does not exist in the given function list ~e" name funs)]))


;; subst
;; - given a function name (idC), a function body (ExprC), a number to pass into the function (Real),
;;   and the variable name that the number is being passed in to (idC).
;; - substitutes the number wherever te variable appears in the function body
(define (subst [name : idC] [body : ExprC] [numb : Real] [var : idC]) : ExprC
  (match body
    [(idC id) (cond
                [(equal? (idC id) var) (numC numb)]
                [else (idC id)])]
    [(numC num) (numC num)]
    [(binopC '+ left-expr right-expr) (binopC '+ (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '* left-expr right-expr) (binopC '* (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '- left-expr right-expr) (binopC '- (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '/ left-expr right-expr) (binopC '/ (subst name left-expr numb var) (subst name right-expr numb var))]
    [(FunappC app-name app-args) (FunappC app-name (map (lambda ([arg : ExprC]) (subst name arg numb var)) app-args))]
    [(ifleq0? if-expr then-expr else-expr)
     (ifleq0? (subst name if-expr numb var) (subst name then-expr numb var) (subst name else-expr numb var))]))




;;;; ---- TESTS ----
 

;; objects for testing
(define func1 (FundefC (idC 'six) (list (idC 'x)) (binopC '+ (numC 2) (numC 4))))
(define func2 (FundefC (idC 'addfour) (list (idC 'x)) (binopC '+ (idC 'x) (numC 4))))
(define func3 (FundefC (idC 'addfour) (list (idC 'x)) (FunappC (idC 'six) (list (numC 7)))))
(define func4 (FundefC (idC 'addfour) (list (idC 'x)) (FunappC (idC 'six) (list (binopC '* (numC 2) (numC 4))))))
(define func5 (FundefC (idC 'addfour) (list (idC 'x)) (FunappC (idC 'six)
                                                               (list (binopC '* (FunappC (idC 'f3) (list (numC 60)))
                                                                                    (binopC '+ (numC 8) (numC 9)))))))
(define func6 (FundefC (idC 'plus4) (list (idC 'x)) (binopC '+ (idC 'x) (numC 4))))
(define func7 (FundefC (idC 'plus8) (list (idC 'y)) (binopC '+ (idC 'y) (FunappC (idC 'plus4) (list (numC 4))))))
(define func8 (FundefC (idC 'lottamath) (list (idC 'x)) (binopC '* (binopC '* (idC 'x) (binopC '+ (idC 'x) (numC 14)))
                                                            (binopC '+ (idC 'x) (numC 4)))))
(define func9 (FundefC (idC 'returninput) (list (idC 'w)) (idC 'w)))
(define func10 (FundefC (idC 'invalid) (list (idC 'w)) (idC 'six)))
(define func11 (FundefC (idC 'f3) (list (idC 'xxx)) (binopC '* (idC 'xxx) (numC 0.5))))
(define func12 (FundefC (idC 'wackyfunc) (list (idC 'x))
                        (FunappC (idC 'plus4) (list (binopC '* (FunappC (idC 'f3) (list (numC 60)))
                                                                                    (binopC '+ (idC 'x) (numC 9)))))))
(define func13 (FundefC (idC 'main) '() (FunappC (idC 'plus4) (list (numC 7)))))
(define func14 (FundefC (idC 'main) '() (FunappC (idC 'plus4) (list (numC 0)))))
(define func15 (FundefC (idC 'main) '() (FunappC (idC 'wackyfunc) (list (numC 3)))))
(define func16 (FundefC (idC 'main) '() (binopC '+ (numC 15) (numC 4))))
(define func17 (FundefC (idC 'main) '() (numC 0)))
(define func18 (FundefC (idC 'noargs) '() (FunappC (idC 'plus4) (list (numC 7)))))
(define func19 (FundefC (idC 'twoargs) (list (idC 'x) (idC 'y)) (binopC '+ (idC 'x) (idC 'y))))
(define func20 (FundefC (idC 'threeargs) (list (idC 'x) (idC 'y) (idC 'z))
                        (binopC '+ (idC 'x) (binopC '+ (idC 'y) (idC 'z)))))
(define func21 (FundefC (idC 'addtwo) '() (binopC '+ (idC 'x) (numC 2))))

(define prog1 '{{func {six x} : {+ 2 4}}
                {func {addfour x} : {+ x 4}}
                {func {main} : {plus4 0}}
                {func {plus4 x} : {+ x 4}}})
(define prog2 '{{func {p x} : {+ x 6}}
                {func {s x} : {- x 255}}
                {func {m x} : {* x 30}}
                {func {d x} : {/ x 3}}
                {func {main} : {p {s {m {d 27}}}}}})
(define prog3 '{{func {w w} : {* 17 w}}
                {func {main} : {w 5}}})
(define prog4 '{{func {w w} : {* 17 w}}
                {func {main} : {w {+ 2 0}}}})
(define prog5 '{{func {main} : {ifleq0? 5 {+ 10 5} {- 8 3}}}})
(define prog6 '{{func {six x} : {+ 2 4}}
                {func {+ x} : {+ x 4}}
                {func {main} : {plus4 0}}
                {func {plus4 x} : {+ x 4}}})
(define prog7 '{{func {round x} :
                      {ifleq0? {- {reduce x} 0.5} {- x {reduce x}} {+ x {reduce x}}}}
                {func {reduce x} :
                      {ifleq0? {- x 0.5} x {reduce {- x 1}}}}
                {func {main} : {round 12.3339492}}
                })
(define prog8 '{{func {round x} :
                      {ifleq0? {- {reduce x} 0.5} {- x {reduce x}} {+ x {reduce x}}}}
                {func {reduce x} :
                      {ifleq0? {- x 0.5} x {reduce {- x 1}}}}
                {func {main} : {round 2.98347}}
                })
(define prog9 '{{func {main} : {recurs 7}}
                {func {recurs x} : {ifleq0? x 99 {recurs {- x 1}}}}})
(define prog10 '{{func {double-operation x} : {* / 4}}
                 {func {main} : {double-operation 0}}})
(define prog11 '{{func {double-operation x} : {+ / 4}}
                 {func {main} : {double-operation 0}}})
(define prog12 '{{func {double-operation x} : {- / 4}}
                 {func {main} : {double-operation 0}}})
(define prog13 '{{func {double-operation x} : {/ / 4}}
                 {func {main} : {double-operation 0}}})
(define prog14 '{{func {wrong-ifleq-use x} : {ifleq0? / 4 2}}
                 {func {main} : {wrong-ifleq-use 0}}})
(define prog15 '{{func {no-ifleq-args x} : {ifleq0?}}
                 {func {main} : {ifleq0?}}})
(define prog16 '{{func {ignoreit x} : {+ 3 4}}
                 {func {main} : {ignoreit{/ 1 {+ 0 0}}}}})
(define prog17 '{{func {: x} : {+ 3 x}}
                 {func {main} : {: 7}}})
(define prog18 '{{func {f x} : {+ 3 x}}
                 {func {main} : f}})
(define prog19 '{{func {f x} : {+ 3 x}}
                 {func {main} : {f}}})
(define prog20 '{{func {f1 x} : {+ 3 x}}
                 {func {f2 x} : {+ 4 x}}
                 {func {main} : {f1 f2}}})
(define prog21 '{{func {f1 x} : {+ 3 x}}
                 {func {f2 x} : {+ 4 x}} 
                 {func {main} : {f3 5}}})
(define prog22 '{{func {f1 x} : {+ 3 y}}
                 {func {main} : {f1 5}}})
(define prog23 '{{func {f1 x y} : {+ x y}}
                 {func {main} : {f1 5 6}}})
(define prog24 '{{func {f1} : {+ 2 3}}
                 {func {main} : {f1}}})
(define prog25 '{{func {f1 a b c d e f} : {+ a {+ b {+ c {+ d {+ e f}}}}}}   
                 {func {main} : {f1 1 2 3 4 5 6}}})
(define prog26 '{{func {f x} : {g 3 x}}
                 {func {g a b} : {+ a b}}
                 {func {main} : {f 9}}})
(define prog27 '{{func {f} : {* 3 4}}
                 {func {g x} : {+ x {f}}}
                 {func {main} : {g 8}}})
(define prog28 '{{func {f x y} : {* x y}}
                 {func {main} : {f 1}}})
(define prog29 '{{func {f x y} : {* x y}}
                 {func {main} : {f}}})
(define prog30 '{{func {f x y} : {* x y}}
                 {func {main} : {f 1 2 3}}})
(define prog31 '{{func {f} : {* x y}}
                 {func {main} : {f}}})
(define prog32 '{{func {f x} : {* x y}}
                 {func {main} : {f 1}}})
(define prog33 '{{func {f} : {* 2 3}}
                 {func {main} : {+ 1 f}}})
(define prog34 '{{func {f} : {* 2 3}}
                 {func {main} : {+ 1 {f}}}})
(define prog35 '{{func {f x x} : {* x x}}
                 {func {main} : {f 1 2}}})
(define prog36 '{{func {f x y} : {+ x y}}
                 {func {x} : {+ 7 2}}
                 {func {main} : {f 1 2}}})
(define prog37 '{{func {f x} : {* {x} 8}}
                 {func {w} : {+ 7 2}}
                 {func {main} : {f w}}})
(define prog38 '{{func {f a b c d e f g h i j a} : {+ 3 4}}
                 {func {main} : {f 1 2 3 4 5 6 7 8 9 10 11}}})



; top-interp tests
(check-equal? (top-interp prog1) 4)
(check-equal? (top-interp prog2) 21)
(check-equal? (top-interp prog3) 85)
(check-equal? (top-interp prog4) 34)
(check-equal? (top-interp prog5) 5)
(check-equal? (top-interp prog7) 12.0)
(check-equal? (top-interp prog8) 3.0)
(check-equal? (top-interp prog9) 99)
(check-equal? (top-interp prog23) 11)
(check-equal? (top-interp prog24) 5)
(check-equal? (top-interp prog25) 21)
(check-equal? (top-interp prog26) 12)
(check-equal? (top-interp prog27) 20)
(check-equal? (top-interp prog34) 7)
(check-equal? (top-interp prog36) 3)
(check-exn #rx"OAZO syntax error in parse-id: expected valid id" (lambda () (top-interp prog6)))
(check-exn #rx"OAZO runtime error in interp-fns-iterater:" (lambda () (top-interp '{})))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog10)))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog11)))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog12)))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog13)))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog14)))
(check-exn #rx"OAZO syntax error in parse: expected valid syntax" (lambda () (top-interp prog15)))
(check-exn #rx"OAZO runtime error in interp-binop:" (lambda () (top-interp prog16)))
(check-exn #rx"OAZO syntax error in parse-id: expected valid id" (lambda () (top-interp prog17)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog18)))
(check-exn #rx"OAZO runtime error in interp-func:" (lambda () (top-interp prog19)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog20)))
(check-exn #rx"OAZO runtime error in find-func:" (lambda () (top-interp prog21)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog22)))
(check-exn #rx"OAZO runtime error in interp-func:" (lambda () (top-interp prog28)))
(check-exn #rx"OAZO runtime error in interp-func:" (lambda () (top-interp prog29)))
(check-exn #rx"OAZO runtime error in interp-func:" (lambda () (top-interp prog30)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog31)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog32)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog33)))
(check-exn #rx"OAZO syntax error in parse-func-def-exprs:" (lambda () (top-interp prog35)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (top-interp prog37)))
(check-exn #rx"OAZO syntax error in parse-func-def-exprs:" (lambda () (top-interp prog38)))

; interp tests
(check-equal? (interp (ifleq0? (numC 0) (numC 5) (numC 10)) '()) 5)
(check-equal? (interp (ifleq0? (binopC '+ (numC -5) (numC 7)) (numC 12) (numC 13)) '()) 13)
(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (binopC '+ (numC 2) (numC 6)) '()) 8)
(check-equal? (interp (binopC '* (numC 2) (numC 6)) '()) 12)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3))
                             (binopC '* (numC 10) (numC 4))) '())
              200)
(check-equal? (interp (FunappC (idC 'plus4) (list (numC 6))) (cons func6 '())) 10)
(check-equal? (interp (FunappC (idC 'lottamath) (list (numC 3))) (cons func6 (cons func7 (cons func8 '())))) 357)


(define complicated-argument (binopC '+ (binopC '* (FunappC (idC 'lottamath) (list (numC 3)))
                                                   (numC 2))
                                        (FunappC (idC 'plus4) (list (numC -714)))))
(check-equal? (interp (binopC '+ (binopC '* (FunappC (idC 'lottamath) (list (numC 3)))
                                            (numC 2))
                                 (FunappC (idC 'plus4) (list (numC -714))))
                      (cons func6 (cons func7 (cons func8 '())))) 4)


(check-equal? (interp (FunappC (idC 'lottamath) (list complicated-argument))
                      (cons func6 (cons func7 (cons func8 '()))))
              576)
(check-equal? (interp (FunappC (idC 'wackyfunc) (list (numC 3)))
                      (cons func12 (cons func11 (cons func6 '()))))
              364.0)
(check-equal? (interp (FunappC (idC 'wackyfunc) (list complicated-argument))
                      (cons func12 (cons func11 (cons func6 (cons func7 (cons func8 '()))))))
              394.0)
(check-equal? (interp (FunappC (idC 'returninput) (list (numC 6)))
                      (cons func6 (cons func7 (cons func8 (cons func9 '())))))
              6)
(check-equal? (interp (numC 4) '())
              4)
(check-equal? (interp (binopC '+ (numC 2) (numC 6)) '())
              8)
(check-equal? (interp (binopC '* (numC 2) (numC 6)) '())
              12)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3)) (binopC '* (numC 10) (numC 4))) '())
              200)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3)) (binopC '* (numC 10) (numC 4))) '())
              200)
(check-equal? (interp (FunappC (idC 'noargs) '()) (cons func6 (cons func18 '())))
              11)
(check-equal? (interp (FunappC (idC 'twoargs) (list (numC 2) (numC 3))) (cons func19 '()))
              5)
(check-exn #rx"OAZO runtime error in find-func"
           (lambda () (interp (FunappC (idC 'wackyfunc) (list complicated-argument))
                              (cons func12 (cons func11 (cons func6 '()))))))
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp (FunappC (idC 'plus4) (list (idC 'plus4))) (cons func6 '()))))
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp (FunappC (idC 'invalid) (list (numC 6))) (cons func10 (cons func1 '())))))

; parse-prog tests
(check-equal? (parse-prog '{{func {six x} : {+ 2 4}}
                              {func {addfour x} : {+ x 4}}})
              (cons func1 (cons func2 '())))
(check-equal? (parse-prog '{{func {six x} : {+ 2 4}}
                              {func {addfour x} : {+ x 4}}
                              {func {main} : {plus4 0}}
                              {func {plus4 x} : {+ x 4}}})
              (cons func1 (cons func2 (cons func14 (cons func6 '())))))
(check-equal? (parse-prog '{}) '())
(check-equal? (parse-prog '{{func {noargs} : {plus4 7}}
                            {func {twoargs x y} : {+ x y}}
                            {func {threeargs x y z} : {+ x {+ y z}}}
                            {func {six x} : {+ 2 4}}})
              (cons func18 (cons func19 (cons func20 (cons func1 '())))))

; parse-fundef tests
(check-equal? (parse-fundef '{func {six x} : {+ 2 4}}) func1)
(check-equal? (parse-fundef '{func {addfour x} : {+ x 4}}) func2)
(check-equal? (parse-fundef '{func {addfour x} : {six 7}}) func3)
(check-equal? (parse-fundef '{func {addfour x} : {six {* 2 4}}}) func4)
(check-equal? (parse-fundef '{func {addfour x} : {six {* {f3 60} (+ 8 9)}}}) func5)
(check-equal? (parse-fundef '{func {noargs} : {plus4 7}}) func18)
(check-equal? (parse-fundef '{func {twoargs x y} : {+ x y}}) func19)
(check-equal? (parse-fundef '{func {threeargs x y z} : {+ x {+ y z}}}) func20)
(check-equal? (parse-fundef '{func {addtwo} : {+ x 2}}) func21)
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {addtwo x} : : {+ x 2}})))
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{fun {addtwo x} : {+ x 2}})))
(check-exn #rx"OAZO syntax error in parse-id: expected valid id, got" (lambda () (parse-fundef '{func {+} : {+ x 2}})))

; parse tests
(check-equal? (parse '4) (numC 4))
(check-equal? (parse '{+ 1 2}) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (binopC '* (numC 1) (numC 2)))
(check-equal? (parse '{* {+ 2 3} {* 10 4}})
              (binopC '* (binopC '+ (numC 2) (numC 3))
                     (binopC '* (numC 10) (numC 4))))
(check-equal? (parse '{f 2}) (FunappC (idC 'f) (list (numC 2))))
(check-equal? (parse '{f {* 2 4}}) (FunappC (idC 'f) (list (binopC '* (numC 2) (numC 4)))))
(check-equal? (parse '{six {* {f3 60} {+ 8 9}}})
            (FunappC (idC 'six) (list (binopC '* (FunappC (idC 'f3) (list (numC 60))) (binopC '+ (numC 8) (numC 9))))))
(check-equal? (parse '{* {f3} {+ 8 9}})
              (binopC '* (FunappC (idC 'f3) '()) (binopC '+ (numC 8) (numC 9))))
(check-equal? (parse '{six {* {f3} {+ 8 9}}})
              (FunappC (idC 'six) (list (binopC '* (FunappC (idC 'f3) '()) (binopC '+ (numC 8) (numC 9))))))
(check-equal? (parse '{six {* {f3 60 59} (+ 8 9)}})
              (FunappC (idC 'six) (list (binopC '* (FunappC (idC 'f3)
                                                          (list (numC 60) (numC 59))) (binopC '+ (numC 8) (numC 9))))))
(check-equal? (parse 'bruh) (idC 'bruh))
(check-equal? (parse 'f) (idC 'f))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse '4)
              (numC 4))
(check-equal? (parse '{+ 1 2})
              (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2})
              (binopC '* (numC 1) (numC 2)))
(check-equal? (parse '{* {+ 2 3} {* 10 4}})
              (binopC '* (binopC '+ (numC 2) (numC 3)) (binopC '* (numC 10) (numC 4))))
(check-equal? (parse '{- 10 {* 2 3}})
              (binopC '- (numC 10) (binopC '* (numC 2) (numC 3))))
(check-equal? (parse '{- 5 10})
              (binopC '- (numC 5) (numC 10)))
(check-equal? (parse '{/ 12 4})
              (binopC '/ (numC 12) (numC 4)))
(check-exn #rx"OAZO syntax error in parse-id:" (lambda () (parse '{1 2 3})))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse 'ifleq0?)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{+ func a})))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{+ b})))
(check-exn #rx"OAZO syntax error in parse-id:" (lambda () (parse '{1 2 3})))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{six {* {f3 60 +} (+ 8 9)}})))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{parse '{+ 1}})))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '())))

; parse-func-def-exprs tests
(check-equal? (parse-func-def-exprs '{x y z} '()) (list (idC 'x) (idC 'y) (idC 'z)))
(check-equal? (parse-func-def-exprs '{x y} '()) (list (idC 'x) (idC 'y)))
(check-equal? (parse-func-def-exprs '{x} '()) (list (idC 'x)))
(check-exn #rx"OAZO syntax error" (lambda () (parse-func-def-exprs '{+ - /} '())))
(check-exn #rx"OAZO syntax error" (lambda () (parse-func-def-exprs '{+} '())))

; argInList? tests
(check-equal? (argInList? (idC 'x) (list (idC 'w) (idC 'y) (idC 'z) (idC 'x))) #t)
(check-equal? (argInList? (idC 'x) (list (idC 'x) (idC 'y) (idC 'z))) #t)
(check-equal? (argInList? (idC 'x) (list (idC 'y) (idC 'z))) #f)
(check-equal? (argInList? (idC 'x) '()) #f)

; parse-func-app-exprs tests
(check-equal? (parse-func-app-exprs '{4 7 8}) (list (numC 4) (numC 7) (numC 8)))
(check-equal? (parse-func-app-exprs '{4 {+ 2 x}}) (list (numC 4) (binopC '+ (numC 2) (idC 'x))))
(check-equal? (parse-func-app-exprs '{4}) (list (numC 4)))
(check-exn #rx"OAZO syntax error" (lambda () (parse-func-app-exprs '{+ - /})))
(check-exn #rx"OAZO syntax error" (lambda () (parse-func-app-exprs '{+})))

; parse-id tests
(check-equal? (parse-id 'bruh) (idC 'bruh))
(check-equal? (parse-id 'f) (idC 'f))
(check-equal? (parse-id 'x) (idC 'x))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{7})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{"doug"})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{+ 2 4})))

; interp-fns test
(check-equal? (interp-fns (cons func6 (cons func13 '()))) 11)
(check-equal? (interp-fns (cons func6 (cons func14 '()))) 4)
(check-equal? (interp-fns (cons func14 (cons func6 '()))) 4)
(check-equal? (interp-fns (cons func7 (cons func6 (cons func8 (cons func9 (cons func14 (cons func1 '()))))))) 4)
(check-equal? (interp-fns (cons func12 (cons func15 (cons func11 (cons func6 '()))))) 364.0)
(check-equal? (interp-fns (cons func2 (cons func16 '()))) 19)
(check-equal? (interp-fns (cons func2 (cons func17 '()))) 0)
(check-exn #rx"OAZO runtime error in interp-fns-iterater"
           (lambda () (interp-fns (cons func7 (cons func6 (cons func8 (cons func9 (cons func2 (cons func1 '())))))))))

; interp-func tests
(check-equal? (interp-func func6 (list (numC 6)) (cons func6 '())) 10)
(check-equal? (interp-func func8 (list (numC 3)) (cons func6 (cons func7 (cons func8 '())))) 357)
(check-equal? (interp-func func7 (list (numC 3)) (cons func6 (cons func7 (cons func8 '())))) 11)
(check-equal? (interp-func func9 (list (numC 204)) (cons func6 (cons func7 (cons func8 (cons func9 '()))))) 204)
(check-equal? (interp-func func18 '() (cons func18 (cons func6 '()))) 11)
(check-equal? (interp-func func19 (list (numC 204) (numC 80)) (cons func19 '())) 284)
(check-exn #rx"OAZO runtime error in find-func"
           (lambda () (interp-func func7 (list (numC 3)) (cons func7 (cons func8 '())))))
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp-func func10 (list (numC 204)) (cons func10 (cons func7 (cons func8 (cons func1 '())))))))

; length-of tests
(check-equal? (length-of '()) 0)
(check-equal? (length-of '(1)) 1)
(check-equal? (length-of '(1 2)) 2)

; find-func tests
(check-equal? (find-func (idC 'plus4) (cons func6 '())) func6)
(check-equal? (find-func (idC 'plus4) (cons func6 (cons func7 '()))) func6)
(check-equal? (find-func (idC 'plus8) (cons func6 (cons func7 '()))) func7)
(check-exn #rx"OAZO runtime error in find-func" (lambda () (find-func (idC 'f) (cons func6 (cons func7 '())))))

; subst tests
(define subst-expr1 (binopC '+ (idC 'x) (numC 4)))
(define subst-expr2 (binopC '+ (numC 8) (numC 4)))
(define subst-expr3 (FunappC (idC 'six) (list (binopC '* (numC 4) (idC 'wango)))))
(define subst-expr4 (FunappC (idC 'six) (list (binopC '* (numC 4) (numC 15)))))
(define subst-expr5
  (FunappC (idC 'six)
           (list (binopC '* (FunappC (idC 'f3)
                               (list (binopC '+ (numC 17) (idC 'x))))
                         (binopC '+ (idC 'x) (binopC '* (numC 2) (idC 'x)))))))
(define subst-expr6
  (FunappC (idC 'six)
           (list (binopC '* (FunappC (idC 'f3)
                               (list (binopC '+ (numC 17) (numC -6))))
                         (binopC '+ (numC -6) (binopC '* (numC 2) (numC -6)))))))

(check-equal? (subst (idC 'test1) subst-expr1 8 (idC 'x)) subst-expr2)
(check-equal? (subst (idC 'test2) subst-expr3 15 (idC 'wango)) subst-expr4)
(check-equal? (subst (idC 'test3) subst-expr5 -6 (idC 'x)) subst-expr6)
(check-equal? (subst (idC 'test4) subst-expr3 15 (idC 'x)) subst-expr3)
