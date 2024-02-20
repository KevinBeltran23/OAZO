#lang typed/racket
(require typed/rackunit)


;;;; ---- NOTES ----

; We fully implemented assignment 5
; Code is organized as follows
; 1) type definitions
; 2) top-interp and interp
; 3) parsing and its helper functions
; 4) interp's helper functions
; 5) testing



;;;; ---- TYPE DEFINITIONS ----


;; ExprC is either a number (NumC), an id (IdC), a string (StrC),
;; an if ____ else ____ then ____ expression (IfC),
;; an anonymous function definition (LamC),
;; or a function application (AppC)
(define-type ExprC (U NumC IdC StrC IfC LamC AppC))

(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC  ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)
(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)


;; Value is either a number, a boolean, a string, a closure, or a primitive operator
(define-type Value (U NumV BoolV StrV CloV PrimV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Environment]) #:transparent)
(struct PrimV ([s : Symbol]) #:transparent)


;; An Environment is a list of Bindings
(define-type Environment(Listof Binding))


;; A Binding consists of a Symbol and a Value
(struct Binding([id : Symbol] [val : Value]) #:transparent)


;; top-env binds primitive Values to their corresponding symbols
(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))
        (Binding 'error (PrimV 'error))))





;;;; ---- TOP-INTERP and INTERP ----


;; top-interp is given a program in the form of an s-expression and:
;; - parses the s-expression into an ExprC representing the AST
;; - interprets the AST into a Value representing the result of the program
;; - serializes the Value by printing it as a string
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


;; interp
;; - given an ExprC and an environment
;; - Interprets the expression with the given environment
;; - p sure it should return some kind of Value type 
(define (interp [exp : ExprC] [env : Environment]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(IdC s) (lookup s env)]
    [(StrC s) (StrV s)]
    [(IfC test-expr then-expr else-expr)
     (match (interp test-expr env)
       [(BoolV #t) (interp then-expr env)]
       [(BoolV #f) (interp else-expr env)]
       [other (error 'interp "OAZO runtime error in interp:
                              comparison ~e returned non-boolean value of ~e"
                     test-expr other)])]
    [(LamC params body)
     (CloV params
           body
           env)]
    [(AppC func args)
     (match (interp func env)
       [(PrimV s) (interp-primv s (map (λ ([arg : ExprC]) (interp arg env)) args))]
       [(CloV params body clov-env)
        (interp body (extend-env clov-env
                                 (create-appc-bindings params
                                                       (map (λ ([arg : ExprC]) (interp arg env)) args)
                                                       '())))]
       [other (error 'interp "OAZO runtime error in interp:
                              tried to apply a non function value ~e
                              which was interpreted from expr ~e"
                     other func)])]))


 
 
;;;; ---- PARSING ----


;; parse
;; - given concrete syntax in the form of an s-expression
;; - parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? x) (NumC x)]
    [(? symbol? symb) #:when
                      (not-reserved? symb)
                      (IdC symb)]
    [(? string? str) (StrC str)]
    [(list 'if test-expr 'then then-expr 'else else-expr)
     (IfC (parse test-expr) (parse then-expr) (parse else-expr))]
    ; need to also catch multiple bindings with the same name
    [(list 'let bindings ... body) (parse-let (cast bindings (Listof Sexp)) body)]
    [(list 'anon (list params ...) ': body)
     (LamC (parse-params params '()) (parse body))]
    [(list 'quote args ...) (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]
    [(list func args ...)
     (AppC (parse func) (map (λ ([arg : Sexp]) (parse arg)) args))]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]))
  

;; parse-let takes a list of Sexps and a body and turns it into an ExprC
(define (parse-let [bindings : (Listof Sexp)] [body : Sexp]) : ExprC
  (AppC (LamC (find-names bindings '()) (parse body))
        (find-exprs bindings)))


;; find-names is given a list of Sexps, bindings, and returns a list of IdCs
;; corresponding to the names in the bindings
(define (find-names [bindings : (Listof Sexp)] [seen : (Listof Sexp)]) : (Listof IdC)
  (match bindings
    ['() '()]
    [(cons (list (? symbol? name) '<- expr) r-bindings)
     (cond
       [(argInList? name seen) (error 'find-names "OAZO syntax error in find-names: ~e defined multiple times" name)]
       [else (cons (IdC name) (find-names r-bindings (cons name seen)))])]
    [other (error 'find-names "OAZO syntax error in find-names: expected valid let syntax, got ~e" other)]))


;; find-exprs is given a list of Sexps, bindings, and returns a list of ExprCs
;; corresponding to the exprs in the bindings
(define (find-exprs [bindings : (Listof Sexp)]) : (Listof ExprC)
  (match bindings
    ['() '()]
    [(cons (list name '<- expr) r-bindings) (cons (parse expr) (find-exprs r-bindings))]))


;; parse-params is given a list of any and turns in into a list of IdCs.
;; if one of the elements of the given list is not a symbol or a reserved name, then it raises an error.
(define (parse-params [params : (Listof Any)] [seen : (Listof Any)]) : (Listof IdC)
  (match params
    ['() '()]
    [(cons (? symbol? f) r) #:when
                            (and (not (argInList? f seen)) (not-reserved? f))
                            (cons (IdC f) (parse-params r (cons f seen)))]
    [other (error 'parse-params "OAZO syntax error in parse-params: expected valid parameter syntax, got ~e" other)]))


;; not-reserved? - Helper function - consumes a Symbol
;; - throws error if name given is built into the language, and thus reserved
;; - otherwise returns true
(define (not-reserved? [name : Symbol]) : Boolean
  (cond
    [(or (equal? 'if name)
         (equal? 'let name)
         (equal? 'anon name)
         (equal? ': name)
         (equal? '<- name)
         (equal? 'then name)
         (equal? 'else name))
     (error 'not-reserved? "OAZO syntax error in not-reserved?: expected valid name, got ~e" name)]
    [else #t]))




;;;; ---- INTERPRETING


;; serialize
;; - should accept any OAZO5 value, and return a string
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (format "~v" n)]
    [(BoolV b) (cond [b "true"]
                     [else "false"])]
    [(StrV s) s]
    [(CloV params body env) "#<procedure>"]
    [(PrimV symb) "#<primop>"]))


;; Lookup function to retrieve value from environment
(define (lookup [s : Symbol] [env : Environment]) : Value
  (cond
    [(null? env) (error 'lookup-binding "OAZO runtime error in lookup: Symbol ~e not found in environment" s)]
    [else (match (first env)
            [(Binding symb val) #:when (equal? symb s) val]
            [other (lookup s (rest env))])]))


;; interp-primv is given a symbol corresponding to a primitive function and
;; a list of values corresponding to the arguments said function is being called with.
;; it then evaluates the primitive operator with the given arguments
(define (interp-primv [op : Symbol] [args : (Listof Value)]): Value
  (match args
    [(list arg1) (match op
                   ['error (error 'user-error "OAZO user error: ~e" (serialize arg1))]
                   [other (call-interp-primv-error1 op)])]
    [(list (NumV n1) (NumV n2)) (match op
                                  ['+ (NumV (+ n1 n2))]
                                  ['- (NumV (- n1 n2))]
                                  ['* (NumV (* n1 n2))]
                                  ['/ (cond
                                        [(equal? n2 0) (error 'interp-primv "OAZO runtime error in interp-primv:
                                                                              tried to divide by 0")]
                                        [else (NumV (/ n1 n2))])]
                                  ['<= (BoolV (<= n1 n2))]
                                  ['equal? (BoolV (equal? n1 n2))]
                                  [other (call-interp-primv-error1 op)])]
    [(list arg1 arg2) (match op
                        ['equal? (cond
                                   [(and (StrV? arg1) (StrV? arg2)) (BoolV (equal? arg1 arg2))]
                                   [(and (BoolV? arg1) (BoolV? arg2)) (BoolV (equal? arg1 arg2))]
                                   [else
                                    (error 'interp-primv "OAZO runtime error in interp-primv:
                                                          tried to call equal? on arguments ~e and ~e" arg1 arg2)])]
                        ['error (call-interp-primv-error1 op)]
                        [other (call-interp-primv-error2 op)])]
    [other (call-interp-primv-error1 op)]))


;; call-interp-primv-error1 is a helper function which calls an error
;; for interp-primv when there are the wrong number of arguments
(define (call-interp-primv-error1 [op : Symbol]) : Value
  (error 'interp-primv "OAZO runtime error in interp-primv: wrong number of arguments given for operator ~e" op))


;; call-interp-primv-error2 is a helper function which calls an error
;; for interp-primv when there are the wrong type of arguments
(define (call-interp-primv-error2 [op : Symbol]) : Value
  (error 'interp-primv "OAZO runtime error in interp-primv: wrong type of arguments given for operator ~e" op))


;; create-appc-bindings is given a list of IdC's, params, a list of Value's, args, and an empty list, seen
;; and creates a list of bindings, each binding corresponding to a name in params and a value in args.
(define (create-appc-bindings [params : (Listof IdC)] [args : (Listof Value)] [seen : (Listof Symbol)]) : Environment
  (match params
    ['() (match args
           ['() '()]
           [other (error 'create-appc-bindings "OAZO runtime error in create-appc-bindings: too many args")])]
    [(cons (IdC s) r-params)
     (cond
       [(argInList? s seen)
        (error 'create-appc-bindings "OAZO runtime error in create-appc-bindings:
                                      multiple params of name ~e" s)]
       [else 20 is(match args
               [(cons f-arg r-args)
                (cons (Binding s f-arg)
                      (create-appc-bindings r-params
                                            r-args
                                            (cons s seen)))]
               [other (error 'create-appc-bindings "OAZO runtime error in create-appc-bindings:
                                                    too many params")])])]))


;; argInList? takes any one element, and a list of elements,
;; and returns true if the element is in the list, and false otherwise
(define (argInList? [arg : Any] [search-list : (Listof Any)]) : Boolean
  (match search-list
    ['() #f]
    [(cons f r) (cond
                  [(equal? f arg) #t]
                  [else (argInList? arg r)])]))


;; extend-env takes a starting environment, start and a list of bindings, new
;; and adds all of the bindings of new to the start environment
(define (extend-env [start : Environment] [new : Environment]) : Environment
  (match new
    ['() start]
    [(cons f-binding r-bindings) (cons f-binding (extend-env start r-bindings))]))


 

;;;; ---- TESTS ----

;; definitions for testing
(define prog1 '{{anon {compose add1} :
                      {{anon {add2} :
                             {add2 99}}   
                       {compose add1 add1}}}
                {anon {f g} :
                      {anon {x} :
                            {f {g x}}}}
                {anon {x} :
                      {+ x 1}}})
(define prog2 '{anon {x} : {* 3 x}})
(define prog3 '{{anon {x} : +} 7})
(define prog4 '{{anon {x} : {+}} 7})
(define prog5 '{{anon {x} : {}} 7})
(define prog6 '{{anon {x y} : {if x then {if y then 7 else {* 8 2}} else {{anon {} : {/ 18 3}}}}} true false})
(define prog7 '{{anon {x y} : {if x then {if y then 7 else {* 8 2}} else {{anon {} : {/ 18 3}}}}} false false})
(define prog8 '{let {+ 3 4}})
(define prog9 '{let [f <- {anon {x} : {* 2 x}}] {f 4}})
(define prog10 '{let [fact <- {anon {self n} :
                                    {if {<= n 0}
                                        then 1
                                        else {* n {self self {- n 1}}}}}]
                  {fact fact 4}})
(define prog11 '{let [7 <- {anon {x} : {* 2 x}}] {f 4}})
(define prog12 '{if {<= 7 8} then {error "error test!"} else "oops"})
(define prog13 '(parse '(+ then 4)))
(define prog14 (quote ((anon (empty) : ((anon (cons) : ((anon (empty?) : ((anon (first) : ((anon (rest) : ((anon (Y) : ((anon (length) : ((anon (addup) : (addup (cons 3 (cons 17 empty...)))))))))))))))))))))

 
;; top-interp tests
(check-equal? (top-interp '10) "10")
(check-equal? (top-interp "Hello") "Hello")
(check-equal? (top-interp '{+ 3 4}) "7")
(check-equal? (top-interp '{{anon {x} : {+ x 1}} 17}) "18")
(check-equal? (top-interp '{equal? 3 {+ 1 2}}) "true")
(check-equal? (top-interp '{equal? 3 {+ 3 2}}) "false")
(check-equal? (top-interp prog1) "101")
(check-equal? (top-interp prog2) "#<procedure>")
(check-equal? (top-interp prog3) "#<primop>")
(check-equal? (top-interp prog6) "16")
(check-equal? (top-interp prog7) "6")
(check-equal? (top-interp prog8) "7")
(check-equal? (top-interp prog9) "8")
(check-equal? (top-interp prog10) "24")
; not sure what this should return tbh 
(check-equal? (top-interp prog14) "20")

(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (top-interp prog4)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (top-interp prog5)))
(check-exn #rx"OAZO syntax error in find-names:" (lambda () (top-interp prog11)))
(check-exn #rx"OAZO user error:" (lambda () (top-interp prog12)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (top-interp prog13)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (top-interp '(parse '(+ then 4)))))



;; interp tests
(check-equal? (interp (NumC 5) top-env) (NumV 5))
(check-equal? (interp (LamC '() (NumC 4)) top-env) (CloV '() (NumC 4) top-env))
(check-equal? (interp (LamC (list (IdC 'x)) (IdC 'x)) top-env) (CloV (list (IdC 'x)) (IdC 'x) top-env))
(check-equal? (interp (LamC (list (IdC 'y))
                            (AppC (LamC (list (IdC 'x)) (IdC 'x))
                                  (list (IdC 'y))))
                      top-env)
              (CloV (list (IdC 'y))
                    (AppC (LamC (list (IdC 'x)) (IdC 'x))
                          (list (IdC 'y)))
                    top-env))
(check-equal? (interp (IdC '*) top-env) (PrimV '*))
(check-equal? (interp (IdC 'b) (list (Binding 'b (StrV "test")))) (StrV "test"))
(check-equal? (interp (IdC 'c) (list (Binding 'c (NumV 5)))) (NumV 5))
(check-equal? (interp (IdC 'true) top-env) (BoolV #t))
(check-equal? (interp (IfC (IdC 'true)
                           (NumC 2)
                           (NumC 1))
                      top-env)
              (NumV 2))
(check-equal? (interp (IfC (IdC 'false)
                           (NumC 2)
                           (NumC 1))
                      top-env)
              (NumV 1))
(check-equal? (interp (AppC (IdC '+) (list (NumC 2) (NumC 10)))
                      top-env)
              (NumV 12))
(check-equal? (interp (AppC (IdC '*)
                            (list (AppC (IdC '+) (list (NumC 2) (NumC 10)))
                                  (NumC 5)))
                      top-env)
              (NumV 60))
(check-equal? (interp (AppC (LamC '() (NumC 4)) '())
                      top-env)
              (NumV 4))
(check-equal? (interp (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 6)))
                      top-env)
              (NumV 6))
(check-equal? (interp (AppC (LamC (list (IdC 'y))
                                  (AppC (LamC (list (IdC 'x)) (IdC 'x))
                                        (list (IdC 'y))))
                            (list (NumC 6)))
                      top-env)
              (NumV 6))
(check-equal? (interp (AppC (LamC (list (IdC 'x) (IdC 'y))
                                  (AppC (IdC '+) (list (IdC 'x)  (IdC 'y))))
                            (list (NumC 6) (NumC 10)))
                      top-env)
              (NumV 16))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp (AppC (IdC '/) (list (NumC 2) (NumC 0)))
                                                                       top-env)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (interp (AppC (NumC 2) (list (NumC 2) (NumC 0)))
                                                                 top-env)))
(check-exn #rx"OAZO runtime error in interp:" (lambda () (interp (IfC (IdC '+)
                                                                      (NumC 2)
                                                                      (NumC 1))
                                                                 top-env)))


;; serialize tests
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "hello")


;; lookup tests
(check-equal? (lookup '+ top-env) (PrimV '+))
(check-equal? (lookup '- top-env) (PrimV '-))
(check-exn #rx"OAZO runtime error in lookup:" (lambda () (lookup 'nope top-env)))


;; interp-primv tests
(check-equal? (interp-primv '+ (list (NumV 7) (NumV 8))) (NumV 15))
(check-equal? (interp-primv '- (list (NumV 7) (NumV 3))) (NumV 4))
(check-equal? (interp-primv '* (list (NumV 7) (NumV 3))) (NumV 21))
(check-equal? (interp-primv '/ (list (NumV 6) (NumV 3))) (NumV 2))
(check-equal? (interp-primv '<= (list (NumV 6) (NumV 3))) (BoolV #f))
(check-equal? (interp-primv '<= (list (NumV 3) (NumV 3))) (BoolV #t))
(check-equal? (interp-primv '<= (list (NumV 2) (NumV 3))) (BoolV #t))
(check-equal? (interp-primv 'equal? (list (NumV 2) (NumV 2))) (BoolV #t))
(check-equal? (interp-primv 'equal? (list (StrV "hey") (StrV "hey"))) (BoolV #t))
(check-equal? (interp-primv 'equal? (list (NumV 3) (NumV 2))) (BoolV #f))
(check-equal? (interp-primv 'equal? (list (StrV "heyy") (StrV "hey"))) (BoolV #f))
(check-equal? (interp-primv 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))
(check-exn #rx"OAZO user error:" (lambda () (interp-primv 'error (list (StrV "test")))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv '+ (list (NumV 7) (NumV 7) (NumV 7)))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv '+ (list (NumV 7)))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv '+ '())))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv '+ (list (StrV "hey") (NumV 7)))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv 'error
                                                                             (list (StrV "test") (StrV "test")))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv '/ (list (NumV 7) (NumV 0)))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv 'error (list (NumV 7) (NumV 9)))))
(check-exn #rx"OAZO runtime error in interp-primv:" (lambda () (interp-primv 'equal? (list (PrimV '+) (BoolV #f)))))


; create-appc-bindings tests
(check-equal? (create-appc-bindings '() '() '()) '())
(check-equal? (create-appc-bindings (list (IdC 'x)) (list (StrV "Hello")) '()) (list (Binding 'x (StrV "Hello"))))
(check-equal? (create-appc-bindings (list (IdC 'x) (IdC 'y) (IdC 'z)) (list (StrV "Hello") (NumV 8) (BoolV #t)) '())
              (list (Binding 'x (StrV "Hello"))
                    (Binding 'y (NumV 8))
                    (Binding 'z (BoolV #t))))
(check-exn #rx"OAZO runtime error in create-appc-bindings:"
           (lambda () (create-appc-bindings (list (IdC 'x) (IdC 'y)) (list (StrV "Hello")) '())))
(check-exn #rx"OAZO runtime error in create-appc-bindings:"
           (lambda () (create-appc-bindings (list (IdC 'x)) (list (StrV "Hello") (NumV 8)) '())))
(check-exn #rx"OAZO runtime error in create-appc-bindings:"
           (lambda () (create-appc-bindings (list (IdC 'x) (IdC 'y) (IdC 'x))
                                            (list (StrV "Hello") (NumV 8) (BoolV #t))
                                            '())))


; extend-env tests
(check-equal? (extend-env top-env '()) top-env)
(check-equal? (extend-env top-env (list (Binding 'x (NumV 7)))) (cons (Binding 'x (NumV 7)) top-env))
(check-equal? (extend-env top-env (list (Binding 'x (NumV 7)) (Binding 'y (NumV 8))))
              (cons (Binding 'x (NumV 7))
                    (cons (Binding 'y (NumV 8)) top-env)))
(check-equal? (extend-env '() (list (Binding 'x (NumV 7)) (Binding 'y (NumV 8))))
              (list (Binding 'x (NumV 7)) (Binding 'y (NumV 8))))


;; parse tests

(check-equal? (parse '10) (NumC 10))
(check-equal? (parse 'doug) (IdC 'doug))
(check-equal? (parse '"This is a string") (StrC "This is a string"))
(check-equal? (parse '{+}) (AppC (IdC '+) '()))
(check-equal? (parse '{+ 1}) (AppC (IdC '+) (list (NumC 1))))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{+ 1 2 "teddy"}) (AppC (IdC '+) (list (NumC 1) (NumC 2) (StrC "teddy"))))
(check-equal? (parse '{if 3 then 2 else 1}) (IfC (NumC 3) (NumC 2) (NumC 1)))
(check-equal? (parse '+) (IdC '+))
(check-equal? (parse '{anon {} : 4})
              (LamC '() (NumC 4)))
(check-equal? (parse '{anon {x} : 4})
              (LamC (list (IdC 'x)) (NumC 4)))
(check-equal? (parse '{anon {x y} : 4})
              (LamC (list (IdC 'x) (IdC 'y)) (NumC 4)))
(check-equal? (parse '{anon {x y z} : 4})
              (LamC (list (IdC 'x) (IdC 'y) (IdC 'z)) (NumC 4)))
(check-equal? (parse '{anon {+ y z} : 4})
              (LamC (list (IdC '+) (IdC 'y) (IdC 'z)) (NumC 4)))
(check-equal? (parse '{name})
              (AppC (IdC 'name) '()))
(check-equal? (parse '{if {<= 5 10} then {+ 2 5} else {+ 2 10}})
              (IfC (AppC (IdC '<=) (list (NumC 5) (NumC 10)))
                   (AppC (IdC '+) (list (NumC 2) (NumC 5)))
                   (AppC (IdC '+) (list (NumC 2) (NumC 10)))))
(check-equal? (parse '{if {<= 5 10} then {+ 2 5} else {+ 2 10}})
              (IfC (AppC (IdC '<=) (list (NumC 5) (NumC 10)))
                   (AppC (IdC '+) (list (NumC 2) (NumC 5)))
                   (AppC (IdC '+) (list (NumC 2) (NumC 10)))))
(check-equal? (parse '{if {<= 2 1} then 2 else 1})
              (IfC (AppC (IdC '<=) (list (NumC 2) (NumC 1)))
                   (NumC 2)
                   (NumC 1)))
(check-equal? (parse '{{anon {add} :
                             {* 2 {add}}}
                       {anon {} : {+ 3 7}}})
              (AppC (LamC (list (IdC 'add))
                          (AppC (IdC '*)
                                (list (NumC 2) (AppC (IdC 'add) '()))))
                    (list (LamC '()
                                (AppC (IdC '+)
                                      (list (NumC '3) (NumC '7)))))))
(check-equal? (parse '{{anon {add} :
                             {add 7 8}}
                       {anon {w z} : {+ w z}}})
              (AppC (LamC (list (IdC 'add))
                          (AppC (IdC 'add)
                                (list (NumC 7) (NumC 8))))
                    (list (LamC (list (IdC 'w) (IdC 'z))
                                (AppC (IdC '+)
                                      (list (IdC 'w) (IdC 'z)))))))
(check-equal? (parse '{{anon {add} :
                             {add 7 8 9 10}}
                       {anon {w x y z} : {+ {+ w x} {+ y z}}}})
              (AppC (LamC (list (IdC 'add))
                          (AppC (IdC 'add)
                                (list (NumC 7) (NumC 8) (NumC 9) (NumC 10))))
                    (list (LamC (list (IdC 'w) (IdC 'x) (IdC 'y) (IdC 'z))
                                (AppC (IdC '+)
                                      (list (AppC (IdC '+) (list (IdC 'w) (IdC 'x)))
                                            (AppC (IdC '+) (list (IdC 'y) (IdC 'z)))))))))
(check-equal? (parse '{let
                        {z <- {+ 9 14}}
                        {y <- 98}
                        {+ z y}})
              (AppC (LamC (list (IdC 'z) (IdC 'y))
                          (AppC (IdC '+)
                                (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+)
                                (list (NumC 9) (NumC 14)))
                          (NumC 98))))


(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{})))
(check-exn #rx"OAZO syntax error in parse-params:" (lambda () (parse '{anon {7 y z} : 4})))
(check-exn #rx"OAZO syntax error in parse-params:" (lambda () (parse '{anon {{f 5} y z} : 4})))
(check-exn #rx"OAZO syntax error in parse-params" (lambda () (parse '(anon (x x) : 3))))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if 7 8})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if {<= 5 10} {+ 2 5} else {+ 2 10}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if {<= 5 10} then {+ 2 5} {+ 2 10}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if {<= 5 10} {+ 2 5} {+ 2 10}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{let})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse 'anon)))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{anon {:} : {+ 3 4}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if : then {+ 3 4} else {+ 5 6}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{anon {<-} : {+ 3 4}})))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '{if <- then {+ 3 4} else {+ 5 6}})))
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (parse '(+ then 4))))
(check-exn #rx"OAZO syntax error in find-names" (lambda () (parse '(let (z <- (anon () : 3)) (z <- 9) (z)))))
