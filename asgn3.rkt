#lang typed/racket
(require typed/rackunit)


;; Assignment 3
;; Full project implemented
  


;;;; ---- TYPE DEFINITIONS ----


;;; ExprC is either:
;;; - one of the following arithmatic expressions: plus, subtract, multiply, divide
;;;       which are represented by their symbols (+, -, *, /)
;;; - an id, represented as a symbol
;;; - a function application, represented by an id and an ExprC.
;;;       The id corresponds to the name of the function, and
;;;       the ExprC corresponds to the expression being 
;;;       passed into the function as the argument
(define-type ExprC (U numC binopC idC FunappC ifleq0?))
(struct numC ([n : Real]) #:transparent)
(struct binopC
  ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct FunappC ([id : idC] [arg1 : ExprC]) #:transparent)
(struct ifleq0? ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)


;;; FunDefC is a function defintion, represented by:
;;; an idC, corresponding to the name of the function
;;; an idC, corresponding to the argument of the function
;;; and an ExprC, corresponding to the function body
(struct FundefC ([name : idC] [arg1 : idC] [body : ExprC]) #:transparent)




;;;; ---- TOP-INTERP ----


;; top-interp accepts an s-expression, calls parse-prog with the s-expression,
;; then interprets the results of parse-prog with interp-fns
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))




;;;; ---- PARSING ----


;; parse-prog is given concrete syntax in the form of an s-expression and parses it into list of FundefCs.
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
  [(cons f r) (cons (parse-fundef f) (parse-prog r))]
  ['() '()]))


;; EXPR types cannot be used as function names
(define (reserved-name? name)
  (or (equal? '+ name)
      (equal? '- name)
      (equal? '* name)
      (equal? '/ name)
      (equal? 'fun name)
      (equal? 'ifleq0? name)
      (equal? ': name)))


;; parse-fundef is given contrece syntax of a function definition in the form of an s-expression
;; and parses it into a funC
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'func (list name-expr arg1-expr) ': body-expr)
     (cond
       [(reserved-name? name-expr)
        (error 'parse-fundef "OAZO syntax error in parse-fundef: invalid function name, got ~e" name-expr)]
       [else (FundefC (parse-id name-expr) (parse-id arg1-expr) (parse body-expr))])]
    [other (error 'parse-fundef "OAZO syntax error in parse-fundef: expected valid syntax, got ~e" other)]))
 

;; parse is given concrete syntax in the form of an s-expression and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? x) (numC x)]
    [(? symbol? symb) (idC symb)]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [(list name expr) (FunappC (parse-id name) (parse expr))]
    [(list 'ifleq0? test-expr then-expr else-expr)
     (ifleq0? (parse test-expr) (parse then-expr) (parse else-expr))]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" other)]))

 
;; parse-id is given a symbol and returns an idC
(define (parse-id [s : Sexp]) : idC
  (match s
    [(? symbol? s) (idC s)]
    [other (error 'parse-id "OAZO syntax error in parse-id: expected valid id, got ~e" other)]))




;;;; ---- INTERPRETING


;; interp-fns is given a list of FundefCs, and iterates through the list until it finds the function name 'main 
;; which it then evaluates. If no function named 'main is found, raises an error
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp-fns-iterater funs funs))
  
  
;; interp-fns-iterater is a recursive helper function to interp-fns. it takes in two lists of function definitions:
;; searching as the function defs left to search for 'main within, and all as the record of all function defs in the program.
;; Iterates through searching to find a function 'main, which it then evaluates. If no function named 'main is found, raises an error
(define (interp-fns-iterater [searching : (Listof FundefC)] [all : (Listof FundefC)]) :  Real
  (match searching
    [(cons (FundefC n a b) r) #:when (equal? n (idC 'main)) (interp-func (FundefC n a b) (numC 0) all)]
    [(cons f r) (interp-fns-iterater r all)]
    ['() (error 'interp-fns-iterater "OAZO runtime error in interp-fns-iterater: no main function defined in ~e" all)]))


;; interp-func is given a FundefC and an ExprC, as well as a list of functions, and evaluates the FundefC by:
;; 1) evaluating the ExprC
;; 2) FundefC has an single parameter, arg1. Wherever arg1 appears in the body of FundefC,
;;          substitutes in the evaluated ExprC.
;; 3) Evaluate the body of FundefC
(define (interp-func [fun : FundefC] [arg1 : ExprC] [funs : (Listof FundefC)]) : Real
  (match fun
    [(FundefC n a b) (interp (subst n b (interp arg1 funs) a) funs)]))

 
;; interp is given an ExprC and a list of FundefCs,
;; and evaluates the given expression using the function definitions
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(numC n) n]
    [ (binopC op l r)
      (match op
       ['+ (+ (interp l funs) (interp r funs))]
       ['* (* (interp l funs) (interp r funs))]
       ['- (- (interp l funs) (interp r funs))]
       ['/ (/ (interp l funs) (interp r funs))])]
    [ (FunappC name arg1) (interp-func (find-func name funs) arg1 funs)]
    [(ifleq0? test-expr then-expr else-expr)
     (cond 
         [(<= (interp test-expr funs) 0) (interp then-expr funs)]
         [else (interp else-expr funs)])]
    [(idC s) (error 'interp "OAZO runtime error in interp: invalid expression, tried to evaluate sumbol ~e" exp)]))

 
;; find-func is given a function name and a list of functions,
;; and returns the function within the given list that corresponds to the given name.
;; if no such function exists, raise an error
(define (find-func [name : idC] [funs : (Listof FundefC)]) : FundefC
  (match funs
  [(cons (FundefC n a b) r) #:when (equal? n name) (FundefC n a b)]
  [(cons f r) (find-func name r)]
  ['() (error 'find-func "OAZO runtime error in find-func: the function ~e does not exist in the given function list ~e" name funs)]))


;; subst is given a function name (idC), function body (ExprC), a number to pass into the function (Real),
;; and the variable name that the number is being passed in to (idC).
;; Wherever the variable name appears in the function body, subst substitues number in.
(define (subst [name : idC] [body : ExprC] [numb : Real] [var : idC]) : ExprC
  (match body
    [(idC id) (cond
                [(equal? (idC id) var) (numC numb)]
                [else (error 'subst "OAZO runtime error in subst: the variable name ~e does not exist in function ~e" id name)])]
    [(numC num) (numC num)]
    [(binopC '+ left-expr right-expr) (binopC '+ (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '* left-expr right-expr) (binopC '* (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '- left-expr right-expr) (binopC '- (subst name left-expr numb var) (subst name right-expr numb var))]
    [(binopC '/ left-expr right-expr) (binopC '/ (subst name left-expr numb var) (subst name right-expr numb var))]
    [(FunappC app-name app-arg) (FunappC app-name (subst name app-arg numb var))]
    [(ifleq0? if-expr then-expr else-expr)
     (ifleq0? (subst name if-expr numb var) (subst name then-expr numb var) (subst name else-expr numb var))]))





;;;; ---- TESTS ----


;; objects for testing
(define func1 (FundefC (idC 'six) (idC 'x) (binopC '+ (numC 2) (numC 4))))
(define func2 (FundefC (idC 'addfour) (idC 'x) (binopC '+ (idC 'x) (numC 4))))
(define func3 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (numC 7))))
(define func4 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (binopC '* (numC 2) (numC 4)))))
(define func5 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (binopC '* (FunappC (idC 'f3) (numC 60)) (binopC '+ (numC 8) (numC 9))))))
(define func6 (FundefC (idC 'plus4) (idC 'x) (binopC '+ (idC 'x) (numC 4))))
(define func7 (FundefC (idC 'plus8) (idC 'y) (binopC '+ (idC 'y) (FunappC (idC 'plus4) (numC 4)))))
(define func8 (FundefC (idC 'lottamath) (idC 'x) (binopC '* (binopC '* (idC 'x) (binopC '+ (idC 'x) (numC 14))) (binopC '+ (idC 'x) (numC 4)))))
(define func9 (FundefC (idC 'returninput) (idC 'w) (idC 'w)))
(define func10 (FundefC (idC 'invalid) (idC 'w) (idC 'six)))
(define func11 (FundefC (idC 'f3) (idC 'xxx) (binopC '* (idC 'xxx) (numC 0.5))))
(define func12 (FundefC (idC 'wackyfunc) (idC 'x) (FunappC (idC 'plus4) (binopC '* (FunappC (idC 'f3) (numC 60)) (binopC '+ (idC 'x) (numC 9))))))
(define func13 (FundefC (idC 'main) (idC 'init) (FunappC (idC 'plus4) (numC 7))))
(define func14 (FundefC (idC 'main) (idC 'init) (FunappC (idC 'plus4) (idC 'init))))
(define func15 (FundefC (idC 'main) (idC 'init) (FunappC (idC 'wackyfunc) (numC 3))))
(define func16 (FundefC (idC 'main) (idC 'init) (binopC '+ (numC 15) (numC 4))))
(define func17 (FundefC (idC 'main) (idC 'init) (idC 'init)))
(define prog1 '{{func {six x} : {+ 2 4}}
                {func {addfour x} : {+ x 4}}
                {func {main init} : {plus4 init}}
                {func {plus4 x} : {+ x 4}}})
(define prog2 '{{func {p x} : {+ x 6}}
                {func {s x} : {- x 255}}
                {func {m x} : {* x 30}}
                {func {d x} : {/ x 3}}
                {func {main init} : {p {s {m {d 27}}}}}})
(define prog3 '{{func {w w} : {* 17 w}}
                {func {main init} : {w 5}}})
(define prog4 '{{func {w w} : {* 17 w}}
                {func {main joe} : {w {+ 2 joe}}}})
(define prog5 '{{func {main init} : {ifleq0? 5 {+ 10 5} {- 8 3}}}})
(define prog6 '{{func {six x} : {+ 2 4}}
                {func {+ x} : {+ x 4}}
                {func {main init} : {plus4 init}}
                {func {plus4 x} : {+ x 4}}})
(define prog7 '{{func {round x} :
                      {ifleq0? {- {reduce x} 0.5} {- x {reduce x}} {+ x {reduce x}}}}
                {func {reduce x} :
                      {ifleq0? {- x 0.5} x {reduce {- x 1}}}}
                {func {main init} : {round 12.2}}
                })
(define prog8 '{{func {round x} :
                      {ifleq0? {- {reduce x} 0.5} {- x {reduce x}} {+ x {reduce x}}}}
                {func {reduce x} :
                      {ifleq0? {- x 0.5} x {reduce {- x 1}}}}
                {func {main init} : {round 2.7}}
                })
(define prog9 '{{func {main init} : {recurs 7}}
                {func {recurs x} : {ifleq0? x 99 {recurs {- x 1}}}}})
(define prog10 '{{func {: x} : {+ 3 x}}
                 {func {main init} : {: 7}}})
(define prog11 '{{func {f x} : {+ 3 x}}
                 {func {main init} : f}})
(define prog12 '{{func {f x} : {+ 3 x}}
                 {func {main init} : {f}}})
(define prog13 '{{func {f1 x} : {+ 3 x}}
                 {func {f2 x} : {+ 4 x}}
                 {func {main init} : {f1 f2}}})
(define prog14 '{{func {f1 x} : {+ 3 x}}
                 {func {f2 x} : {+ 4 x}}
                 {func {main init} : {f3 5}}})
(define prog15 '{{func {f1 x} : {+ 3 y}}
                 {func {main init} : {f1 5}}})

  
; top-interp tests
(check-equal? (top-interp prog1) 4)
(check-equal? (top-interp prog2) 21)
(check-equal? (top-interp prog3) 85)
(check-equal? (top-interp prog4) 34)
(check-equal? (top-interp prog5) 5)
(check-equal? (top-interp prog7) 12.0)
(check-equal? (top-interp prog8) 3.0)
(check-equal? (top-interp prog9) 99)
(check-exn #rx"OAZO syntax error in parse-fundef: invalid function name" (lambda () (top-interp prog6)))
(check-exn #rx"OAZO runtime error in interp-fns-iterater" (lambda () (top-interp '{})))
(check-exn #rx"OAZO syntax error in parse-fundef: invalid function name" (lambda () (top-interp prog10)))
(check-exn #rx"OAZO runtime error in subst" (lambda () (top-interp prog11)))
(check-exn #rx"OAZO syntax error in parse" (lambda () (top-interp prog12)))
(check-exn #rx"OAZO runtime error in subst" (lambda () (top-interp prog13)))
(check-exn #rx"OAZO runtime error in find-func" (lambda () (top-interp prog14)))
(check-exn #rx"OAZO runtime error in subst" (lambda () (top-interp prog15)))


; parse-prog tests
(check-equal? (parse-prog '{{func {six x} : {+ 2 4}}
                              {func {addfour x} : {+ x 4}}})
              (cons func1 (cons func2 '())))
(check-equal? (parse-prog '{{func {six x} : {+ 2 4}}
                              {func {addfour x} : {+ x 4}}
                              {func {main init} : {plus4 init}}
                              {func {plus4 x} : {+ x 4}}})
              (cons func1 (cons func2 (cons func14 (cons func6 '())))))
(check-equal? (parse-prog '{}) '())


; parse-fundef tests
(check-equal? (parse-fundef '{func {six x} : {+ 2 4}}) func1)
(check-equal? (parse-fundef '{func {addfour x} : {+ x 4}}) func2)
(check-equal? (parse-fundef '{func {addfour x} : {six 7}}) func3)
(check-equal? (parse-fundef '{func {addfour x} : {six {* 2 4}}}) func4)
(check-equal? (parse-fundef '{func {addfour x} : {six {* {f3 60} (+ 8 9)}}}) func5)
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {addtwo x} : : {+ x 2}})))
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{fun {addtwo x} : {+ x 2}})))
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {addtwo} : {+ x 2}})))
(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {addtwo x y} : {+ x 2}})))


; parse tests
(check-equal? (parse '4) (numC 4))
(check-equal? (parse '{+ 1 2}) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (binopC '* (numC 1) (numC 2)))
(check-equal? (parse '{* {+ 2 3} {* 10 4}})
              (binopC '* (binopC '+ (numC 2) (numC 3))
                     (binopC '* (numC 10) (numC 4))))
(check-equal? (parse '{f 2}) (FunappC (idC 'f) (numC 2)))
(check-equal? (parse '{f {* 2 4}}) (FunappC (idC 'f) (binopC '* (numC 2) (numC 4))))
(check-equal? (parse '{six {* {f3 60} (+ 8 9)}}) (FunappC (idC 'six) (binopC '* (FunappC (idC 'f3) (numC 60)) (binopC '+ (numC 8) (numC 9)))))
(check-equal? (parse 'bruh) (idC 'bruh))
(check-equal? (parse 'f) (idC 'f))
(check-equal? (parse 'x) (idC 'x))
(check-exn #rx"OAZO syntax error in parse" (lambda () (parse '{1 2 3})))
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
(check-exn #rx"syntax" (lambda () (parse '{1 2 3})))


; parse-id tests
(check-equal? (parse-id 'bruh) (idC 'bruh))
(check-equal? (parse-id 'f) (idC 'f))
(check-equal? (parse-id 'x) (idC 'x))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{7})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{"doug"})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{+ 2 4})))


; interp-func tests
(check-equal? (interp-func func6 (numC 6) (cons func6 '())) 10)
(check-equal? (interp-func func8 (numC 3) (cons func6 (cons func7 (cons func8 '())))) 357)
(check-equal? (interp-func func7 (numC 3) (cons func6 (cons func7 (cons func8 '())))) 11)
(check-equal? (interp-func func9 (numC 204) (cons func6 (cons func7 (cons func8 (cons func9 '()))))) 204)
(check-exn #rx"OAZO runtime error in find-func"
           (lambda () (interp-func func7 (numC 3) (cons func7 (cons func8 '())))))
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp-func func7 (idC 'x) (cons func6 (cons func7 (cons func8 '()))))))
(check-exn #rx"OAZO runtime error in subst"
           (lambda () (interp-func func10 (numC 204) (cons func10 (cons func7 (cons func8 (cons func1 '())))))))
 

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
 

; interp tests
(check-equal? (interp (ifleq0? (numC 0) (numC 5) (numC 10)) '()) 5)
(check-equal? (interp (ifleq0? (binopC '+ (numC -5) (numC 7)) (numC 12) (numC 13)) '()) 13)
(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (binopC '+ (numC 2) (numC 6)) '()) 8)
(check-equal? (interp (binopC '* (numC 2) (numC 6)) '()) 12)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3))
                             (binopC '* (numC 10) (numC 4))) '())
              200)
(check-equal? (interp (FunappC (idC 'plus4) (numC 6)) (cons func6 '())) 10)
(check-equal? (interp (FunappC (idC 'lottamath) (numC 3)) (cons func6 (cons func7 (cons func8 '())))) 357)
(define complicated-argument (binopC '+ (binopC '* (FunappC (idC 'lottamath) (numC 3)) (numC 2)) (FunappC (idC 'plus4) (numC -714))))
(check-equal? (interp (binopC '+ (binopC '* (FunappC (idC 'lottamath) (numC 3)) (numC 2)) (FunappC (idC 'plus4) (numC -714)))
                      (cons func6 (cons func7 (cons func8 '())))) 4)
(check-equal? (interp (FunappC (idC 'lottamath) complicated-argument) (cons func6 (cons func7 (cons func8 '())))) 576)
(check-equal? (interp (FunappC (idC 'wackyfunc) (numC 3)) (cons func12 (cons func11 (cons func6 '())))) 364.0)
(check-equal? (interp (FunappC (idC 'wackyfunc) complicated-argument) (cons func12 (cons func11 (cons func6 (cons func7 (cons func8 '())))))) 394.0)
(check-equal? (interp (FunappC (idC 'returninput) (numC 6)) (cons func6 (cons func7 (cons func8 (cons func9 '()))))) 6)
(check-exn #rx"OAZO runtime error in find-func"
           (lambda () (interp (FunappC (idC 'wackyfunc) complicated-argument)
                              (cons func12 (cons func11 (cons func6 '()))))))
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp (FunappC (idC 'plus4) (idC 'plus4)) (cons func6 '()))))
(check-exn #rx"OAZO runtime error in subst"
           (lambda () (interp (FunappC (idC 'invalid) (numC 6)) (cons func10 (cons func1 '())))))
(check-equal? (interp (numC 4) '())
              4)
(check-equal? (interp (binopC '+ (numC 2) (numC 6)) '())
              8)
(check-equal? (interp (binopC '* (numC 2) (numC 6)) '())
              12)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3)) (binopC '* (numC 10) (numC 4))) '())
              200)


; find-func tests
(check-equal? (find-func (idC 'plus4) (cons func6 '())) func6)
(check-equal? (find-func (idC 'plus4) (cons func6 (cons func7 '()))) func6)
(check-equal? (find-func (idC 'plus8) (cons func6 (cons func7 '()))) func7)
(check-exn #rx"OAZO runtime error in find-func" (lambda () (find-func (idC 'f) (cons func6 (cons func7 '())))))


; subst tests
(define subst-expr1 (binopC '+ (idC 'x) (numC 4)))
(define subst-expr2 (binopC '+ (numC 8) (numC 4)))
(define subst-expr3 (FunappC (idC 'six) (binopC '* (numC 4) (idC 'wango))))
(define subst-expr4 (FunappC (idC 'six) (binopC '* (numC 4) (numC 15))))
(define subst-expr5 (FunappC (idC 'six) (binopC '* (FunappC (idC 'f3) (binopC '+ (numC 17) (idC 'x))) (binopC '+ (idC 'x) (binopC '* (numC 2) (idC 'x))))))
(define subst-expr6 (FunappC (idC 'six) (binopC '* (FunappC (idC 'f3) (binopC '+ (numC 17) (numC -6))) (binopC '+ (numC -6) (binopC '* (numC 2) (numC -6))))))

(check-equal? (subst (idC 'test1) subst-expr1 8 (idC 'x)) subst-expr2)
(check-equal? (subst (idC 'test2) subst-expr3 15 (idC 'wango)) subst-expr4)
(check-equal? (subst (idC 'test3) subst-expr5 -6 (idC 'x)) subst-expr6)
(check-exn #rx"OAZO runtime error in subst" (lambda () (subst (idC 'test4) subst-expr3 15 (idC 'x))))
