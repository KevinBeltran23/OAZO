#lang typed/racket
(require typed/rackunit)

;; question for lab:
; --> is it better style to have all the test cases together at the bottom,
;     or after each function they are testing?
;
; --> what should happend when the EXPN in the function defintion is an id?
;     '{func {addtwo x} : f}
;     (just an function name, not a function call)
;     syntax (parser) or runtime (interp)? i think runtime
;     currently have it as a runtime error in subst because it's not a valid argument
;
; --> what about having the input of a function application be a symbol?
;     '{f1 f2}
;     syntax (parser) or runtime (interp)?
;     currently have it as a runtime error in interp (tried to evaluate symbol)
;    
; --> what about a function call on a function that doesn't exist?
;     syntax (parser) or runtime (interp)? i think runtime
;     currently have it as a runtime error in find-func
;
; --> function with argument symbol different from symbol used in body? ex. '{func {addtwo y} : {+ x 2}}
;     syntax (parser) or runtime (interp)? i think runtime
;     currently have it as a runtime error in subst
;
; --> function with the same symbol for the name and the argument? ex. '{func {x x} : {+ x 2}}
;     syntax (parser) or runtime (interp)? No Error!
;     no test case yet
;     namespace
;     Actually will be fine. Write a test that works that has this sort of function.
;
; --> user writes a recursive function? function that calls itself will run forever. Syntax or runtime error?
;     this can work because of the ifleq0 conditional.
;     write test case with recursion using ifleq0
;     but if user defines infinite recursive function, we won't be able to catch it?
;     write a test case for this
;
; --> two FundefC's with the same id?
;     syntax (parser) or runtime (interp)?
;     no test case yet
;
; --> no main function given in program
;     syntax (parser) or runtime (interp)?
;     no test case yet
;
; --> main function argument is not called 'init
;     syntax (parser) or runtime (interp)?
;     no test case yet
;
;
; not a question just to do:
; write interp-fns test cases (don't need to many, can test top-interp hard)
; implement interp-fns
; define parse-prog
; write parse-prog test cases (don't need to many, can test top-interp hard)
; implement parse-prog
; define top-interp
; test the shit out of top-interp
; write out top-interp
;
; change name of id in FundefC structure to ... name.
; list of arguments instead of 1 argument?
; put all test cases at bottom


;;; ExprC is either:
;;; - one of the following arithmatic expressions: plus, subtract, multiply, divide
;;;       which are represented by their symbols (+, -, *, /)
;;; - an id, represented as a symbol
;;; - a function application, represented by an id and an ExprC.
;;;       The id corresponds to the name of the function, and
;;;       the ExprC corresponds to the expression being 
;;;       passed into the function as the argument
(define-type ExprC (U numC plusC multC idC FunappC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct multC ([l : ExprC] [r : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct FunappC ([id : idC] [arg1 : ExprC]) #:transparent)


;;; FunDefC is a function defintion, represented by:
;;; an idC, corresponding to the name of the function
;;; an idC, corresponding to the argument of the function
;;; and an ExprC, corresponding to the function body
(struct FundefC ([id : idC] [arg1 : idC] [body : ExprC]) #:transparent)


;;;; ---- objects for testing ----
(define func1 (FundefC (idC 'six) (idC 'x) (plusC (numC 2) (numC 4))))
(define func2 (FundefC (idC 'addfour) (idC 'x) (plusC (idC 'x) (numC 4))))
(define func3 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (numC 7))))
(define func4 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (multC (numC 2) (numC 4)))))
(define func5 (FundefC (idC 'addfour) (idC 'x) (FunappC (idC 'six) (multC (FunappC (idC 'f3) (numC 60)) (plusC (numC 8) (numC 9))))))
(define func6 (FundefC (idC 'plus4) (idC 'x) (plusC (idC 'x) (numC 4))))
(define func7 (FundefC (idC 'plus8) (idC 'y) (plusC (idC 'y) (FunappC (idC 'plus4) (numC 4)))))
(define func8 (FundefC (idC 'lottamath) (idC 'x) (multC (multC (idC 'x) (plusC (idC 'x) (numC 14))) (plusC (idC 'x) (numC 4)))))
(define func9 (FundefC (idC 'returninput) (idC 'w) (idC 'w)))
(define func10 (FundefC (idC 'invalid) (idC 'w) (idC 'six)))
(define func11 (FundefC (idC 'f3) (idC 'xxx) (multC (idC 'xxx) (numC 0.5))))
(define func12 (FundefC (idC 'wackyfunc) (idC 'x) (FunappC (idC 'plus4) (multC (FunappC (idC 'f3) (numC 60)) (plusC (idC 'x) (numC 9))))))
;(define fu)




;;;; ---- PARSING ----

;; parse is given concrete syntax in the form of an s-expression and parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? x) (numC x)]
    [(? symbol? symb) (idC symb)]
    [(list '* left-exp right-exp) (multC (parse left-exp) (parse right-exp))]
    [(list '+ left-exp right-exp) (plusC (parse left-exp) (parse right-exp))]
    [(list name expr) (FunappC (parse-id name) (parse expr))]
    ; what if try to parse an idC by itself?
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" other)]))


;; parse-id is given a symbol and returns an idC
(define (parse-id [s : Sexp]) : idC
  (match s
    [(? symbol? s) (idC s)]
    [other (error 'parse-id "OAZO syntax error in parse-id: expected valid id, got ~e" other)]))


;; parse-fundef is given contrece syntax of a function definition in the form of an s-expression
;; and parses it into a funC
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'func (list name-expr arg1-expr) ': body-expr) (FundefC (parse-id name-expr) (parse-id arg1-expr) (parse body-expr))]
    [other (error 'parse-fundef "OAZO syntax error in parse-fundef: expected valid syntax, got ~e" other)]))


; parse tests
(check-equal? (parse '4) (numC 4))
(check-equal? (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (multC (numC 1) (numC 2)))
(check-equal? (parse '{* {+ 2 3} {* 10 4}})
              (multC (plusC (numC 2) (numC 3))
                     (multC (numC 10) (numC 4))))
(check-equal? (parse '{f 2}) (FunappC (idC 'f) (numC 2)))
(check-equal? (parse '{f {* 2 4}}) (FunappC (idC 'f) (multC (numC 2) (numC 4))))
(check-equal? (parse '{six {* {f3 60} (+ 8 9)}}) (FunappC (idC 'six) (multC (FunappC (idC 'f3) (numC 60)) (plusC (numC 8) (numC 9)))))
(check-equal? (parse 'bruh) (idC 'bruh))
(check-equal? (parse 'f) (idC 'f))
(check-equal? (parse 'x) (idC 'x))
(check-exn #rx"OAZO syntax error in parse" (lambda () (parse '{1 2 3})))


; parse-id tests
(check-equal? (parse-id 'bruh) (idC 'bruh))
(check-equal? (parse-id 'f) (idC 'f))
(check-equal? (parse-id 'x) (idC 'x))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{7})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{"doug"})))
(check-exn #rx"OAZO syntax error in parse-id" (lambda () (parse-id '{+ 2 4})))


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
#;(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {addtwo y} : {+ x 2}})))
#;(check-exn #rx"OAZO syntax error in parse-fundef" (lambda () (parse-fundef '{func {x x} : {+ x 2}})))






;;;; ---- INTERPRETING

;; interp-fns is given a list of FundefCs, and iterates through the list until
;; it finds the function name 'main , which it then evaluates. If no function
;; named 'main is found, raises an error
#;(define (interp-fns [funs : (Listof FundefC)]) : Real
  0)



;; interp is given an ExprC and a list of FundefCs,
;; and evaluates the given expression using the function definitions
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(numC n) n]
    [ (plusC left-exp right-exp) ( + (interp left-exp funs) (interp right-exp funs) ) ] 
    [ (multC left-exp right-exp) ( * (interp left-exp funs) (interp right-exp funs) ) ]
    [ (FunappC name arg1) (interp-func (find-func name funs) arg1 funs)] 
    [(idC s) (error 'interp "OAZO runtime error in interp: invalid expression, tried to evaluate sumbol ~e" exp)]))


;; interp-func is given a FundefC and an ExprC, as well as a list of functions, and evaluates the FundefC by:
;; 1) evaluating the ExprC
;; 2) FundefC has an single parameter, arg1. Wherever arg1 appears in the body of FundefC,
;;          substitutes in the evaluated ExprC.
;; 3) Evaluate the body of FundefC
(define (interp-func [fun : FundefC] [arg1 : ExprC] [funs : (Listof FundefC)]) : Real
  (match fun
    [(FundefC n a b) (interp (subst n b (interp arg1 funs) a) funs)]))


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
    [(plusC left-expr right-expr) (plusC (subst name left-expr numb var) (subst name right-expr numb var))]
    [(multC left-expr right-expr) (multC (subst name left-expr numb var) (subst name right-expr numb var)) ]
    [(FunappC app-name app-arg) (FunappC app-name (subst name app-arg numb var))]))



; interp tests
(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (plusC (numC 2) (numC 6)) '()) 8)
(check-equal? (interp (multC (numC 2) (numC 6)) '()) 12)
(check-equal? (interp (multC (plusC (numC 2) (numC 3))
                             (multC (numC 10) (numC 4))) '())
              200)
(check-equal? (interp (FunappC (idC 'plus4) (numC 6)) (cons func6 '())) 10)
(check-equal? (interp (FunappC (idC 'lottamath) (numC 3)) (cons func6 (cons func7 (cons func8 '())))) 357)
(define complicated-argument (plusC (multC (FunappC (idC 'lottamath) (numC 3)) (numC 2)) (FunappC (idC 'plus4) (numC -714))))
(check-equal? (interp (plusC (multC (FunappC (idC 'lottamath) (numC 3)) (numC 2)) (FunappC (idC 'plus4) (numC -714)))
                      (cons func6 (cons func7 (cons func8 '())))) 4)
(check-equal? (interp (FunappC (idC 'lottamath) complicated-argument) (cons func6 (cons func7 (cons func8 '())))) 576)
(check-equal? (interp (FunappC (idC 'wackyfunc) (numC 3)) (cons func12 (cons func11 (cons func6 '())))) 364.0)
(check-equal? (interp (FunappC (idC 'wackyfunc) complicated-argument) (cons func12 (cons func11 (cons func6 (cons func7 (cons func8 '())))))) 394.0)
(check-equal? (interp (FunappC (idC 'returninput) (numC 6)) (cons func6 (cons func7 (cons func8 (cons func9 '()))))) 6)
(check-exn #rx"OAZO runtime error in find-func"
           (lambda () (interp (FunappC (idC 'wackyfunc) complicated-argument)
                              (cons func12 (cons func11 (cons func6 '()))))))
; checks for passing an idC in for a function input
(check-exn #rx"OAZO runtime error in interp"
           (lambda () (interp (FunappC (idC 'plus4) (idC 'plus4)) (cons func6 '()))))
(check-exn #rx"OAZO runtime error in subst"
           (lambda () (interp (FunappC (idC 'invalid) (numC 6)) (cons func10 (cons func1 '())))))


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


; find-func tests
(check-equal? (find-func (idC 'plus4) (cons func6 '())) func6)
(check-equal? (find-func (idC 'plus4) (cons func6 (cons func7 '()))) func6)
(check-equal? (find-func (idC 'plus8) (cons func6 (cons func7 '()))) func7)
(check-exn #rx"OAZO runtime error in find-func" (lambda () (find-func (idC 'f) (cons func6 (cons func7 '())))))


; subst tests
(define subst-expr1 (plusC (idC 'x) (numC 4)))
(define subst-expr2 (plusC (numC 8) (numC 4)))
(define subst-expr3 (FunappC (idC 'six) (multC (numC 4) (idC 'wango))))
(define subst-expr4 (FunappC (idC 'six) (multC (numC 4) (numC 15))))
(define subst-expr5 (FunappC (idC 'six) (multC (FunappC (idC 'f3) (plusC (numC 17) (idC 'x))) (plusC (idC 'x) (multC (numC 2) (idC 'x))))))
(define subst-expr6 (FunappC (idC 'six) (multC (FunappC (idC 'f3) (plusC (numC 17) (numC -6))) (plusC (numC -6) (multC (numC 2) (numC -6))))))

(check-equal? (subst (idC 'test1) subst-expr1 8 (idC 'x)) subst-expr2)
(check-equal? (subst (idC 'test2) subst-expr3 15 (idC 'wango)) subst-expr4)
(check-equal? (subst (idC 'test3) subst-expr5 -6 (idC 'x)) subst-expr6)
(check-exn #rx"OAZO runtime error in subst" (lambda () (subst (idC 'test4) subst-expr3 15 (idC 'x))))





              






















;; top-interp accepts an s-expression, calls parse with the s-expression,
;; then interprets the results of the parser with interp
(define (top-interp [code : Sexp]) : Real
  (interp (parse code) '()))

; top-interp tests
(check-equal? (top-interp '{* {+ 2 3} {* 5 4}}) 100)
(check-equal? (top-interp '{* {+ 2 4} {+ 10 5}}) 90)
(check-equal? (top-interp '{* {+ 3 4} {* {* 2 4} 7}}) 392)
#;(check-exn #rx"syntax" (lambda () (top-interp '{* 7})))  ;; should have a different error due to -> "where an id is not +, - , *, /, fun, ifleq0?, :, or else:"







