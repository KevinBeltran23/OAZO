#lang typed/racket
(require typed/rackunit)


;;;; ---- NOTES ----



; We __ implemented assignment 5
; Code is organized as follows
; 1) type definitions
; 2) top-interp and interp
; 3) parsing and its helper functions
; 4) interpreting and its helper functions
; 5) testing



;;;; ---- TYPE DEFINITIONS ----



;Expr	 	=	 	Num
; 	 	|	 	id
; 	 	|	 	String
; 	 	|	 	{if Expr then Expr else Expr}
; 	 	|	 	{let [id <- Expr] ... Expr}
; 	 	|	 	{anon {id ...} Expr}
; 	 	|	 	{Expr Expr ...}
; where an id is not ?, else:, with, as, or blam.

;; ExprC is ...
(define-type ExprC (U NumC IdC StrC IfC LamC AppC))

(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC  ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)
#;(Struct LetC : we are going to parse it directly into a AppC)
(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([func : (U IdC LamC)] [args : (Listof ExprC)]) #:transparent)


;; The book has a union type Value - also strongly hinted at in 5.2
;; - Next, add closures to the set of values. This is pretty much straight out of the book.
;; - Define a representation for primitive operators, and add it to your set of Values

;  The OAZO5 language has a variety of different kinds of values:
;  -  Real numbers
;  -  Booleans
;  -  Strings
;  -  Closures
;  -  Primitive Operators

; Note that in this language, all primitive operators are values, and can be passed and applied like user-defined functions.

;; Value is ...
(define-type Value (U NumV BoolV StrV))

(struct NumV ([n : Real]) #:transparent)
#;(struct CloV ([]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
#;(struct PrimV ([]) #:transparent)


;; these are the primitive operators 
; + - should be AppC
; -
; *
; /
; <= - ..
; equal? - should be AppC
; true - should be IdC
; false - should be IdC
; error - should be AppC


;; we need to define the environment type(s)




;;;; ---- TOP-INTERP and INTERP ----

;; top-interp
;; - I havent written anything here I just copied the instructions
;; - accepts an s-expression
;; - calls serialize with the s-expression
;; - Combines parsing and evaluation
;; - Your body can be different if you really want, but the types must match these.
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; interp
;; - given an ExprC and an environment
;; - Interprets an expression, with a given environment
(define (interp [exp : ExprC] [env : Env]) : Real
  0)

;; serialize
;; - should accept any OAZO5 value, and return a string
;; - For numbers, use ~v directly, so that (for instance) the serialization of
;;   your representation of 34 would produce the string "34"
;; - The serialization of strings should include wrapping double-quotes. You can use the racket function ~v for this as well.
;; - all closures should be serialized as the string "#<procedure>", and primitive operators should be serialized as the string "#<primop>".
(define (serialize [anything : ExprC]) : String
  "place holder")

 


;;;; ---- PARSING ----


;; parse
;; - given concrete syntax in the form of an s-expression
;; - parses it into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? x) (NumC x)]
    [(? symbol? symb) ...]
    [(? string? str) (StrC str)]
    [(list 'if text-expr 'then then-expr 'else else-expr) ...]
    [(let ...) ...]
    [(list 'anon '() ': body) ...]
    [(list 'anon (cons f-param r-param) ': body) ...]
    [(cons f-arg r-arg) ...]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]))

;; reserved-name? - Helper function - consumes a Symbol
;; - returns true if Symbol given is built into the language, and thus reserved
;; - need to think of a better name for this helper function
;; - could possibly put the errors inside of here? 
(define (reserved-name? name)
  (or (equal? '? name)
      (equal? 'else name)
      (equal? ': name)
      (equal? 'with name)
      (equal? 'as name)
      (equal? 'blam name)))



;;;; ---- INTERPRETING



;;;; ---- TESTS ----

;; parse tests
(check-equal? (parse '{10})
              (NumC 10))
(check-equal? (parse '{name})
              (AppC (IdC 'name)))
(check-equal? (parse '{"This is a string"})
              (StrC "This is a string"))
(check-equal? (parse '{if {>= 5 10} {+ 2 5} {+ 2 10}})
              (IfC (AppC (IdC '>=) (list (NumC 5) (NumC 10))) (AppC (IdC '+) (list (NumC 2) (NumC 5))) (AppC (IdC '+) (list (NumC 2) (NumC 10)))))
(check-equal? (parse '{if {> 2 1} then {2} else {1}})
              (IfC (AppC (IdC '>) (list (NumC 2) (NumC 1))) (NumC 2) (NumC 1)))

(check-equal? (parse '{{anon {add} :
                             {* 2 {add}}}
                       {anon {} : {+ 3 7}}})
              (AppC (LamC (list (IdC 'add))
                          (AppC (IdC '*) (list (NumC 2) (AppC (IdC 'add) '()))))
                    (LamC '()
                          (AppC (IdC '+) (list (NumC '3) (NumC '7))))))
(check-equal? (parse '{{anon {add} :
                             {add 7 8}}
                       {anon {w z} : {+ w z}}})
              (AppC (LamC (list (IdC 'add))
                          (AppC (IdC 'add) (list (NumC 7) (NumC 8))))
                    (LamC (list (IdC 'w) (IdC 'z))
                          (AppC (IdC '+) (list (IdC 'w) (IdC 'z))))))

(check-equal? (parse '{+}) (AppC (IdC '+)))
(check-equal? (parse '{+ 1}) (AppC (IdC '+) (list (NumC 1))))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '+)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '-)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '/)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse '<=)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse 'equal?)))
(check-exn #rx"OAZO syntax error in parse:" (lambda () (parse 'error)))


