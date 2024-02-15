#lang typed/racket
(require typed/rackunit)


;;;; ---- NOTES ----



; We __ implemented assignment 5
; Code is organized as follows
; 1) type definitions
; 2) top-interp and interp
; 3) parsing and its helper functions
; 4) interp's helper functions
; 5) testing



;;;; ---- TYPE DEFINITIONS ----


;; ExprC is ...
(define-type ExprC (U NumC IdC StrC IfC LamC AppC))

(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC  ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)
#;(Struct LetC : we are going to parse it directly into a AppC)
(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)


;; Value is ...
(define-type Value (U NumV BoolV StrV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
#;(struct CloV ([]) #:transparent)
#;(struct PrimV ([]) #:transparent)


;; these are the primitive operators. when seen individually should parse into IdC
; +
; - 
; *
; /
; <=
; equal?
; true
; false
; error


;; we need to define the environment type(s)




;;;; ---- TOP-INTERP and INTERP ----

;; top-interp is given a program in the form of an s-expression and:
;; - parses the s-expression into an ExprC representing the AST
;; - interprets the AST into a Value representing the result of the program
;; - serializes the Value by printing it as a string
#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; interp
;; - given an ExprC and an environment
;; - Interprets the expression with the given environment
#;(define (interp [exp : ExprC] [env : Env]) : Real
  0)

;; serialize
;; - should accept any OAZO5 value, and return a string
;; - For numbers, use ~v directly, so that (for instance) the serialization of
;;   your representation of 34 would produce the string "34"
;; - The serialization of strings should include wrapping double-quotes. You can use the racket function ~v for this as well.
;; - all closures should be serialized as the string "#<procedure>", and primitive operators should be serialized as the string "#<primop>".
#;(define (serialize [val : Value]) : String
  "place holder")

 


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
    #;[(let ...) ...]
    [(list 'anon (list params ...) ': body)
     (LamC (parse-params params) (parse body))]
    [(list func args ...)
     (AppC (parse func) (map (λ ([arg : Sexp]) (parse arg)) args))]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]))


;; parse-params is given a list of any and turns in into a list of IdCs.
;; if one of the elements of the given list is not a symbol or a reserved name, then it raises an error.
(define (parse-params [params : (Listof Any)]) : (Listof IdC)
  (match params
    ['() '()]
    [(cons (? symbol? f) r) #:when
                            (not-reserved? f)
                            (cons (IdC f) (parse-params r))]
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
         (equal? '<- name))
     (error 'not-reserved? "OAZO syntax error in not-reserved?: expected valid name, got ~e" name)]
    [else #t]))




;;;; ---- INTERPRETING



;;;; ---- TESTS ----

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
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '{})))
(check-exn #rx"OAZO syntax error in parse-params:" (lambda () (parse '{anon {7 y z} : 4})))
(check-exn #rx"OAZO syntax error in parse-params:" (lambda () (parse '{anon {{f 5} y z} : 4})))
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
