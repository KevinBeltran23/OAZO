#lang typed/racket
(require typed/rackunit)

;;;; ---- NOTES ----

; currently stripped down language to work only with binary primitive operators
; we need to implement the store passing structure for this functionality first
; then we can build upon that once store passing works for +, -, *, etc...

; we will need to modify lookup to work with stores
; Check to make sure that each store is used exactly once with the exception of the mutation operations added later


; We ____ implemented assignment 7
; Code is organized as follows
; 1) type definitions
; 2) top-interp and interp
; 3) parsing and its helper functions
; 4) interp's helper functions
; 5) testing
 


;;;; ---- TYPE DEFINITIONS ----

#;(  new oazo syntax
Expr	 	=	 	Num
 	 	|	 	id
 	 	|	 	String
 	 	|	 	{id := Expr}
 	 	|	 	{if Expr then Expr else Expr}
 	 	|	 	{let [[id : ty] <- Expr] ... Expr}
 	 	|	 	{anon {[ty id] ...} : Expr}
 	 	|	 	{seq Expr ...}
 	 	|	 	{Expr Expr ...}

  ty	 	=	 	num
 	 	|	 	bool
 	 	|	 	str
 	 	|	 	void
 	 	|	 	{ty ... -> ty}
 	 	|	 	numarray

  operator	 	=	 	+
 	 	|	 	-
 	 	|	 	*
 	 	|	 	/
 	 	|	 	num-eq?
 	 	|	 	str-eq?
 	 	|	 	<=
 	 	|	 	substring
 	 	|	 	arr
 	 	|	 	aref
 	 	|	 	aset
 	 	|	 	alen

... where an id is not let, :=, if, then, else, :, <-, seq.
)

;; an Expr is ...


;; a ty is ...


;; an operator is ...



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
;; - Change the Env type to map names to locations
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


;; A Store is a list of Cells
(define-type Store (Listof Cell))


;; A Cell consists of a location and a value
(define-type-alias Location Real)
(struct Cell([location : Location] [val : Value]) #:transparent)

 


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

;  Choose a representation for a Value-combined-with-store,
;  and change the type of the interp function so that it accepts a store and returns a Value-combined-with-store
; Rewrite the interp rules for numbers and primitive applications so
; that they thread the store through the computation as they should.

(define (interp [exp : ExprC] [env : Environment]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(IdC s) (lookup s env)]
    #;[(StrC s) (StrV s)]
    #;[(IfC test-expr then-expr else-expr)
     (match (interp test-expr env)
       [(BoolV #t) (interp then-expr env)]
       [(BoolV #f) (interp else-expr env)]
       [other (error 'interp "OAZO runtime error in interp:
                              comparison ~e returned non-boolean value of ~e"
                     test-expr other)])]
    #;[(LamC params body)
     (CloV params
           body 
           env)]
    [(AppC func args)
     (match (interp func env)
       [(PrimV s) (interp-primv s (map (λ ([arg : ExprC]) (interp arg env)) args))]
       #;[(CloV params body clov-env)
        (interp body (extend-env clov-env
                                 (create-appc-bindings params
                                                       (map (λ ([arg : ExprC])
                                                              (interp arg env))
                                                            args)
                                                       '())))]
       [other (error 'interp "OAZO unimplemented feature in ~e" other)])]
    [other (error 'interp "OAZO unimplemented feature in ~e" other)]))
 
 
  
 
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
       [(and (not-reserved? name) (argInList? name seen))
        (error 'find-names "OAZO syntax error in find-names: ~e defined multiple times" name)]
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
    [(StrV s) (format "~v" s)]
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
                                   [else (BoolV #f)])]
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
       [else (match args
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

;;;; ---- Testing ----
 
(check-equal? (top-interp '{+ 3 4}) "7")
(check-exn #rx"OAZO unimplemented feature" (lambda () (top-interp '{{anon {x} : {+ 3 x}} 7})))








