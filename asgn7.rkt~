#lang typed/racket
(require typed/rackunit)

;;;; ---- NOTES ----

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
;; A Binding consists of a Symbol and a Location
(struct Binding([id : Symbol] [location : Location]) #:transparent)

;; A Store is a list of Cells
(define-type Store (Listof Cell))

;; A Cell consists of a location and a value
(define-type-alias Location Real)
(struct Cell([location : Location] [val : Value]) #:transparent)

;; A VStore is a value combined with a store
(struct VStore([val : Value] [sto : Store]) #:transparent)


;; top-env binds primitive Values to their corresponding symbols
;; need to redefine top-env to work with stores
#;(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))
        (Binding 'error (PrimV 'error))))

(define top-env
  (list (Binding '+ 0)
        (Binding '- 1)
        (Binding '* 2)
        (Binding '/ 3)
        (Binding 'num-eq? 4)
        (Binding 'str-eq? 5)
        (Binding '<= 6)
        (Binding 'substring 7)
        (Binding 'arr 8)
        (Binding 'aref 9)
        (Binding 'aset 10)
        (Binding 'alen 11)))
 
;; Im assuming we need a top store???
;; and then somehow we "add" more values to our store
;; not sure tbh this is just here temporarily
(define top-store
  (list (Cell 0 (PrimV '+))
        (Cell 1 (PrimV '-))
        (Cell 2 (PrimV '*))
        (Cell 3 (PrimV '/))
        (Cell 4 (PrimV 'num-eq?))
        (Cell 5 (PrimV 'str-eq?))
        (Cell 6 (PrimV '<=))
        (Cell 7 (PrimV 'substring))
        (Cell 8 (PrimV 'arr))
        (Cell 9 (PrimV 'aref))
        (Cell 10 (PrimV 'aset))
        (Cell 11 (PrimV 'alen))))
 


;;;; ---- TOP-INTERP and INTERP ----


;; top-interp is given a program in the form of an s-expression and:
;; - parses the s-expression into an ExprC representing the AST
;; - interprets the AST into a Value representing the result of the program
;; - serializes the Value by printing it as a string
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env top-store)))
  
 
;; interp
;; - given an ExprC and an environment
;; - Interprets the expression with the given environment
(define (interp [exp : ExprC] [env : Environment] [store : Store]) : VStore
  (match exp 
    [(NumC n) (VStore (NumV n) store)]
    [(IdC s) (VStore (fetch (lookup s env) store) store)]
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
     (match (VStore-val (interp func env store)) 
       [(PrimV s) (interp-primv s (map (λ ([arg : ExprC]) (VStore-val (interp arg env store))) args) store)]
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
         (equal? ':= name)
         (equal? '<- name)
         (equal? 'seq name)
         (equal? 'then name)
         (equal? 'else name))
     (error 'not-reserved? "OAZO syntax error in not-reserved?: expected valid name, got ~e" name)]
    [else #t]))

 

;;;; ---- INTERPRETING


;; serialize
;; - should accept any OAZO7 value, and return a string
(define (serialize [vs : VStore]) : String
  (match (VStore-val vs)
    [(NumV n) (format "~v" n)]
    [(BoolV b) (cond [b "true"]
                     [else "false"])]
    [(StrV s) (format "~v" s)]
    [(CloV params body env) "#<procedure>"]
    [(PrimV symb) "#<primop>"]))


;; Lookup function to retrieve a Location from environment
(define (lookup [s : Symbol] [env : Environment]) : Location
  (cond 
    [(null? env) (error 'lookup-binding "OAZO runtime error in lookup: Symbol ~e not found in environment" s)]
    [else (match (first env)
            [(Binding symb location) #:when (equal? symb s) location]
            [other (lookup s (rest env))])]))


;; Fetch function to retreive a value from store
(define (fetch [l : Location] [store : Store]) : Value
  (cond
    [(null? store) (error 'lookup-binding "OAZO runtime error in lookup: Location ~e not found in store" l)]
    [else (match (first store)
            [(Cell location val) #:when (equal? location l) val]
            [other (fetch l (rest store))])]))

 

;; interp-primv is given a symbol corresponding to a primitive function and
;; a list of values corresponding to the arguments said function is being called with.
;; it then evaluates the primitive operator with the given arguments
(define (interp-primv [op : Symbol] [args : (Listof Value)] [store : Store]): VStore
  (match args
    [(list arg1) (match op
                   ['error (VStore (error 'user-error "OAZO user error: ~e" (serialize (VStore arg1 store))) store)]
                   [other (call-interp-primv-error1 op store)])]
    [(list (NumV n1) (NumV n2)) (match op
                                  ['+ (VStore (NumV (+ n1 n2)) store)]
                                  ['- (VStore (NumV (- n1 n2)) store)]
                                  ['* (VStore (NumV (* n1 n2)) store)]
                                  ['/ (cond
                                        [(equal? n2 0) (VStore (error 'interp-primv "OAZO runtime error in interp-primv:
                                                                              tried to divide by 0") store)]
                                        [else (VStore (NumV (/ n1 n2)) store)])]
                                  ['<= (VStore (BoolV (<= n1 n2)) store)]
                                  ['equal? (VStore (BoolV (equal? n1 n2)) store)]
                                  [other (call-interp-primv-error1 op store)])]
    [(list arg1 arg2) (match op
                        ['equal? (cond
                                   [(and (StrV? arg1) (StrV? arg2)) (VStore (BoolV (equal? arg1 arg2)) store)]
                                   [(and (BoolV? arg1) (BoolV? arg2)) (VStore (BoolV (equal? arg1 arg2)) store)]
                                   [else (VStore (BoolV #f) store)])]
                        ['error (call-interp-primv-error1 op store)]
                        [other (call-interp-primv-error2 op store)])]
    [other (call-interp-primv-error1 op store)]))


;; call-interp-primv-error1 is a helper function which calls an error
;; for interp-primv when there are the wrong number of arguments
(define (call-interp-primv-error1 [op : Symbol] [store : Store]) : VStore
  (error 'interp-primv "OAZO runtime error in interp-primv: wrong number of arguments given for operator ~e" op))


;; call-interp-primv-error2 is a helper function which calls an error
;; for interp-primv when there are the wrong type of arguments
(define (call-interp-primv-error2 [op : Symbol] [store : Store]) : VStore
  (VStore (error 'interp-primv "OAZO runtime error in interp-primv: wrong type of arguments given for operator ~e" op) store))


;; create-appc-bindings is given a list of IdC's, params, a list of Value's, args, and an empty list, seen
;; and creates a list of bindings, each binding corresponding to a name in params and a value in args.
#;(define (create-appc-bindings [params : (Listof IdC)] [args : (Listof Value)] [seen : (Listof Symbol)]) : Environment
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


;; top-interp tests
(check-equal? (top-interp '{+ 3 4}) "7")
(check-exn #rx"OAZO unimplemented feature" (lambda () (top-interp '{{anon {x} : {+ 3 x}} 7})))
 
;; interp tests
(check-equal? (interp (NumC 5) top-env top-store) (VStore (NumV 5) top-store))
(check-equal? (interp (AppC (IdC '+) (list (NumC 2) (NumC 10))) top-env top-store)
              (VStore (NumV 12) top-store))

;; interp-primv tests
 
(check-equal? (interp-primv '+ (list (NumV 7) (NumV 8)) top-store) (VStore (NumV 15) top-store))


;; lookup and fetch tests


;; serialize tests


;; extend-env tests


;; create-appc-bindings tests


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
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '(let (: <- "") "World"))))










