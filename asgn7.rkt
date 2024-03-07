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
(define-type Value (U NumV BoolV StrV CloV PrimV ArrayV NullV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent) 
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Environment]) #:transparent)
(struct PrimV ([s : Symbol]) #:transparent)
(struct ArrayV ([location : Integer] [length : Integer]) #:transparent)
(struct NullV () #:transparent)


;; An Environment is a list of Bindings
;; - Change the Env type to map names to locations
(define-type Environment(Listof Binding))

 
;; A Binding consists of a Symbol and a Location
(struct Binding([id : Symbol] [location : Integer]) #:transparent)

;; A Store is a list of Cells
(struct Store ([cells : (Listof Cell)] [length : Integer]) #:transparent)

;; A Cell consists of a location (Integer) and a val (Value)
(struct Cell([location : Integer] [val : Value]) #:transparent)

;; A VStore is a value combined with a store
(struct VStore([val : Value] [sto : Store]) #:transparent)

; A VListStore is a list of values combined with a store
(struct VListStore([args : (Listof Value)] [sto : Store]) #:transparent)

; An IStore is a base lcoation combined with a store
(struct IStore ([store : Store] [base : Integer] ) #:transparent)


;; top-env binds primitive Values to their corresponding symbols
;; need to redefine top-env to work with stores
#;(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'num-eq? (PrimV 'num-eq?))
        (Binding 'str-eq? (PrimV 'str-eq?))
        (Binding 'arr-eq? (PrimV 'str-eq?))
        (Binding 'substring 7)
        (Binding 'arr (PrimV 'arr))
        (Binding 'aref (PrimV 'aref))
        (Binding 'aset (PrimV 'aset))
        (Binding 'alen (PrimV 'alen))))

(define top-env
  (list (Binding '+ 0)
        (Binding '- 1)
        (Binding '* 2)
        (Binding '/ 3)
        (Binding '<= 4)
        (Binding 'num-eq? 5)
        (Binding 'str-eq? 6)
        (Binding 'arr-eq? 7)
        (Binding 'substring 8)
        (Binding 'arr 9)
        (Binding 'aref 10)
        (Binding 'aset 11)
        (Binding 'alen 12)))
  
;; Im assuming we need a top store???
;; and then somehow we "add" more values to our store
;; not sure tbh this is just here temporarily
(define top-store
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (PrimV '*))
               (Cell 3 (PrimV '/))
               (Cell 4 (PrimV '<=))
               (Cell 5 (PrimV 'num-eq?))
               (Cell 6 (PrimV 'str-eq?))
               (Cell 7 (PrimV 'arr-eq?))
               (Cell 8 (PrimV 'substring))
               (Cell 9 (PrimV 'arr))
               (Cell 10 (PrimV 'aref))
               (Cell 11 (PrimV 'aset))
               (Cell 12 (PrimV 'alen)))
         13))




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
    [(StrC s) (VStore (StrV s) store)]
    #;[(IfC test-expr then-expr else-expr)
     (match (interp test-expr env) 
       [(BoolV #t) (interp then-expr env)]
       [(BoolV #f) (interp else-expr env)]
       [other (error 'interp "OAZO runtime error in interp:
                              comparison ~e returned non-boolean value of ~e"
                     test-expr other)])] 
    [(LamC params body)
     (VStore (CloV params body env) store)]  
    [(AppC func args) 
     (match (interp func env store)
       [(VStore (PrimV s) new-store) (interp-primv s (interp-args args env new-store '()))]
       #;[(VStore (CloV params body clov-env) new-store)
        (match (interp-args args env new-store '())
                  [(VListStore vls-list vls-store)
                   (interp body
                           (extend-env clov-env
                                       (create-appc-bindings params
                                                             vls-list
                                                             '()))
                           vls-store)])]
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
    [(PrimV symb) "#<primop>"]
    [(ArrayV loc len) "#<array>"]
    [(NullV) "null"]))


;; Lookup function to retrieve a Location from environment
(define (lookup [s : Symbol] [env : Environment]) : Integer
  (cond 
    [(null? env) (error 'lookup-binding "OAZO runtime error in lookup: Symbol ~e not found in environment" s)]
    [else (match (first env)
            [(Binding symb location) #:when (equal? symb s) location]
            [other (lookup s (rest env))])]))
 

;; Fetch function to retreive a value from store
(define (fetch [l : Integer] [store : Store]) : Value
  (match store
    [(Store '() n) (error 'lookup-binding "OAZO runtime error in lookup: Location ~e not found in store" l)]
    [(Store (cons (Cell location val) r) n) #:when (equal? location l) val]
    [(Store (cons f r) n) (fetch l (Store r n))])
  
  #;(cond
    [(null? store) (error 'lookup-binding "OAZO runtime error in lookup: Location ~e not found in store" l)]
    [else (match (first (Store-cells store))
            [(Cell location val) #:when (equal? location l) val]
            [other (fetch l (rest store))])]))


;; Interprets a list of ExprC's and returns a list of Values combined with a new Store
(define (interp-args [args : (Listof ExprC)] [env : Environment] [store : Store] [vals : (Listof Value)]) : VListStore
  (match args
    ['() (VListStore (reverse-list vals '()) store)]
    [(cons f r) (match (interp f env store)
                  [(VStore v s) (interp-args r env s (cons v vals))])]))
 

;; Reverses a list
(define (reverse-list [input : (Listof Value)] [output : (Listof Value)]) : (Listof Value)
  (match input
    ['() output]
    [(cons f r) (reverse-list r (cons f output))]))


 
;; interp-primv is given a symbol corresponding to a primitive function and
;; a list of values corresponding to the arguments said function is being called with.
;; it then evaluates the primitive operator with the given arguments
(define (interp-primv [op : Symbol] [vliststore : VListStore]): VStore
  (define store (VListStore-sto vliststore))
  (match (VListStore-args vliststore)
    [(list arg1) (match op 
                   ['error (VStore (error 'user-error "OAZO user error: ~e" (serialize (VStore arg1 store))) store)]
                   ['alen (cond
                            [(ArrayV? arg1) (VStore (NumV (ArrayV-length arg1)) store)]
                            [else (call-interp-primv-error2 op)])]
                   [other (call-interp-primv-error1 op)])]
    [(list (NumV n1) (NumV n2)) (match op
                                  ['+ (VStore (NumV (+ n1 n2)) store)] 
                                  ['- (VStore (NumV (- n1 n2)) store)]
                                  ['* (VStore (NumV (* n1 n2)) store)]
                                  ['/ (cond
                                        [(equal? n2 0) (VStore (error 'interp-primv "OAZO runtime error in interp-primv:
                                                                              tried to divide by 0") store)]
                                        [else (VStore (NumV (/ n1 n2)) store)])]
                                  ['<= (VStore (BoolV (<= n1 n2)) store)]
                                  ['arr (cond
                                          [(integer? n1)
                                           (match (allocate store n1 (NumV n2))
                                             [(IStore new-store base) (VStore (ArrayV base (cast n1 Integer)) new-store)])]
                                          [else (call-interp-primv-error2 op)])]
                                  ['num-eq? (VStore (BoolV (equal? n1 n2)) store)]
                                  [other (call-interp-primv-error1 op)])]
    [(list arg1 arg2) (match op
                        ['str-eq? (cond
                                    [(and (StrV? arg1) (StrV? arg2))
                                     (VStore (BoolV (equal? arg1 arg2)) store)]
                                    [else (call-interp-primv-error2 op)])]
                        ['arr-eq? (cond
                                   [(and (ArrayV? arg1) (ArrayV? arg2))
                                    (VStore (BoolV (and (equal? (ArrayV-location arg1) (ArrayV-location arg2))
                                                        (equal? (ArrayV-length arg1) (ArrayV-length arg2))))
                                            store)]
                                   [else (call-interp-primv-error2 op)])]
                        ['aref (match (list arg1 arg2)
                                 [(list (ArrayV loc len) (NumV n))
                                  (cond
                                    [(and (integer? n) (> n -1) (< n len))
                                     (VStore (fetch (cast (+ loc n) Integer) store) store)]
                                    [else (call-interp-primv-error2 op)])]
                                 [other (call-interp-primv-error2 op)])]
                        ['error (call-interp-primv-error1 op)]
                        [other (call-interp-primv-error2 op)])]
    [(list (ArrayV loc len) (NumV idx) (NumV newval))
     (match op
       ['aset (cond
                [(and (integer? idx) (> idx -1) (< idx len))
                 (VStore (NullV)
                         (Store (mutate-array store (+ loc (cast idx Integer)) newval)
                                (Store-length store)))]
                [else (call-interp-primv-error2 op)])]
       [other (call-interp-primv-error1 op)])]
    [other (call-interp-primv-error1 op)]))


;;
(define (mutate-array [inputStore : Store] [loc : Integer] [newval : Real]) : (Listof Cell)
  (match inputStore
    [(Store '() store-len) '()]
    [(Store (cons (Cell n v) r) store-len)
     (cond
       [(equal? n loc)
        (cons (Cell n (NumV newval)) (mutate-array (Store r store-len) loc newval))]
       [else
        (cons (Cell n v) (mutate-array (Store r store-len) loc newval))])]))


;; call-interp-primv-error1 is a helper function which calls an error
;; for interp-primv when there are the wrong number of arguments
(define (call-interp-primv-error1 [op : Symbol]) : VStore
  (error 'interp-primv "OAZO runtime error in interp-primv: wrong number of arguments given for operator ~e" op))


;; call-interp-primv-error2 is a helper function which calls an error
;; for interp-primv when there are the wrong type of arguments
(define (call-interp-primv-error2 [op : Symbol]) : VStore
  (error 'interp-primv "OAZO runtime error in interp-primv: wrong type of arguments given for operator ~e" op))


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
; change to take in a store as well.
; Then instead of creating binding between s & f-arg,
; gotta create a binding between s & a num, then a cell from that num to f-arg
; This num is found by allocating 1 memory space of f-arg. That covers the cell and will give the num to create binding too.
; keep passing new store along while building up environment, return a E store.


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




;; Allocate accepts a store, a number of locations to be allocated,
;; and a value to place in all of them, and returns two things; the base location, and an extended store
;; Design your store so that the "next allocated" location is derived directly from the store.
;; It could be a separate counter that’s part of a define-type, or it could be a function that just
;; scans the store to find a new address. Don’t use the new-loc defined by the book; it makes testing quite painful.
(define (allocate [store : Store] [num : Real] [val : Value]) : IStore
  (cond
    [(and (integer? num) (> num 0))
     (match store
       [(Store cells length)
        (IStore (Store (allocate-helper cells (cast num Integer) val length) (cast (+ length num) Integer)) length)])]
    [else (error 'allocate "OAZO runtime error in allocate: tried to allocate fewer than 1 cell")]))


;; Allocate Helper recursively creates new cells in the store
(define (allocate-helper [start-cells : (Listof Cell)] [num : Integer] [val : Value] [next-loc : Integer]) : (Listof Cell)
  (match start-cells
    ['() (cond
           [(equal? num 0) '()]
           [else (cons (Cell next-loc val) (allocate-helper '() (- num 1) val (+ next-loc 1)))])]
    [(cons f r) (cons f (allocate-helper r num val next-loc))]))






;;;; ---- Testing ----

;; definitions for tests
(define test-store1
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-)))
         2))
(define test-store2
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (NumV 3))
               (Cell 3 (NumV 3))
               (Cell 4 (NumV 3))
               (Cell 5 (NumV 3)))
         6))
(define test-store3
  (Store (list (Cell 0 (StrV "test"))
               (Cell 1 (StrV "test"))
               (Cell 2 (StrV "test")))
         3))
(define test-store4
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (PrimV '*))
               (Cell 3 (PrimV '/))
               (Cell 4 (PrimV '<=))
               (Cell 5 (PrimV 'num-eq?))
               (Cell 6 (PrimV 'str-eq?))
               (Cell 7 (PrimV 'arr-eq?))
               (Cell 8 (PrimV 'substring))
               (Cell 9 (PrimV 'arr))
               (Cell 10 (PrimV 'aref))
               (Cell 11 (PrimV 'aset))
               (Cell 12 (PrimV 'alen))
               (Cell 13 (NumV 3)))
         14))
(define test-store5
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (PrimV '*))
               (Cell 3 (PrimV '/))
               (Cell 4 (PrimV '<=))
               (Cell 5 (PrimV 'num-eq?))
               (Cell 6 (PrimV 'str-eq?))
               (Cell 7 (PrimV 'arr-eq?))
               (Cell 8 (PrimV 'substring))
               (Cell 9 (PrimV 'arr))
               (Cell 10 (PrimV 'aref))
               (Cell 11 (PrimV 'aset))
               (Cell 12 (PrimV 'alen))
               (Cell 13 (NumV 3))
               (Cell 14 (NumV 3))
               (Cell 15 (NumV 3))
               (Cell 16 (NumV 3)))
         17))
(define test-store6
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (PrimV '*))
               (Cell 3 (PrimV '/))
               (Cell 4 (PrimV '<=))
               (Cell 5 (PrimV 'num-eq?))
               (Cell 6 (PrimV 'str-eq?))
               (Cell 7 (PrimV 'arr-eq?))
               (Cell 8 (PrimV 'substring))
               (Cell 9 (PrimV 'arr))
               (Cell 10 (PrimV 'aref))
               (Cell 11 (PrimV 'aset))
               (Cell 12 (PrimV 'alen))
               (Cell 13 (NumV 9))
               (Cell 14 (NumV 9))
               (Cell 15 (NumV 12))
               (Cell 16 (NumV 12))
               (Cell 17 (NumV 12)))
         18))
(define test-store7
  (Store (list (Cell 0 (PrimV '+))
               (Cell 1 (PrimV '-))
               (Cell 2 (PrimV '*))
               (Cell 3 (PrimV '/))
               (Cell 4 (PrimV '<=))
               (Cell 5 (PrimV 'num-eq?))
               (Cell 6 (PrimV 'str-eq?))
               (Cell 7 (PrimV 'arr-eq?))
               (Cell 8 (PrimV 'substring))
               (Cell 9 (PrimV 'arr))
               (Cell 10 (PrimV 'aref))
               (Cell 11 (PrimV 'aset))
               (Cell 12 (PrimV 'alen))
               (Cell 13 (NumV 9))
               (Cell 14 (NumV 12))
               (Cell 15 (NumV 9)))
         16))


(define prog1 '{alen {arr 3 7}})
(define prog2 '{aref {arr 3 7} 0})
(define prog3 '{aref {arr 3 7} 1})
(define prog4 '{aref {arr 3 7} 2})
(define prog5 '{aref {arr 3 7} -1})
(define prog6 '{aref {arr 3 7} 3})
(define prog7 '{aref {arr 3 7} 1.6})
(define prog8 '{arr 1.1 5})
(define prog9 '{str-eq? "bazinga" "bazinga"})
(define prog10 '{str-eq? "bazinga" "bazoinga"})
(define prog11 '{num-eq? 6 {+ 4 2}})
(define prog12 '{num-eq? 6 {+ 4 4}})

  
;; top-interp tests
(check-equal? (top-interp '{+ 3 4}) "7")
(check-equal? (top-interp '{* 3 4}) "12")
(check-equal? (top-interp '{- {/ {* {+ 4 5} {+ 12 18}} 6} 5}) "40")
(check-equal? (top-interp '{anon {x} : {* 3 x}}) "#<procedure>")
(check-equal? (top-interp '{anon {x y} : {+ y x}}) "#<procedure>")
(check-equal? (top-interp '+) "#<primop>")
(check-equal? (top-interp '{anon {} : {+ 2 3}}) "#<procedure>")
(check-equal? (top-interp '{arr 5 79}) "#<array>")
(check-equal? (top-interp '{arr-eq? {arr 2 9} {arr 3 12}}) "false")
(check-equal? (top-interp prog1) "3")
(check-equal? (top-interp prog2) "7")
(check-equal? (top-interp prog3) "7")
(check-equal? (top-interp prog4) "7")
(check-equal? (top-interp prog9) "true")
(check-equal? (top-interp prog10) "false")
(check-equal? (top-interp prog11) "true")
(check-equal? (top-interp prog12) "false")
(check-exn #rx"OAZO runtime error in allocate" (lambda () (top-interp '{arr 0 79})))
(check-exn #rx"OAZO runtime error in allocate" (lambda () (top-interp '{arr -1 79})))
(check-exn #rx"OAZO unimplemented feature" (lambda () (top-interp '{{anon {x} : {+ 3 x}} 7})))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog5)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog6)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog7)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog8)))


;; interp tests
(check-equal? (interp (NumC 5) top-env top-store) (VStore (NumV 5) top-store))
(check-equal? (interp (AppC (IdC '+) (list (NumC 2) (NumC 10))) top-env top-store)
              (VStore (NumV 12) top-store))
(check-equal? (interp (AppC (IdC 'arr) (list (NumC 1) (NumC 3))) top-env top-store) (VStore (ArrayV 13 1) test-store4))
(check-equal? (interp (AppC (IdC 'arr) (list (NumC 4) (NumC 3))) top-env top-store) (VStore (ArrayV 13 4) test-store5))
(check-equal? (interp (AppC (IdC 'arr-eq?)
                            (list (AppC (IdC 'arr)
                                        (list (NumC 2) (NumC 9)))
                                  (AppC (IdC 'arr)
                                        (list (NumC 3) (NumC 12))))) top-env top-store) (VStore (BoolV #f) test-store6))
(check-equal? (interp (AppC (IdC 'aset)
                            (list (AppC (IdC 'arr)
                                        (list (NumC 3) (NumC 9)))
                                  (NumC 1)
                                  (NumC 12)))
                      top-env
                      top-store)
              (VStore (NullV) test-store7))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp (AppC (IdC 'aset)
                                                                            (list (AppC (IdC 'arr)
                                                                                        (list (NumC 3) (NumC 9)))
                                                                                  (NumC -1)
                                                                                  (NumC 12)))
                                                                      top-env
                                                                      test-store7)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp (AppC (IdC 'aset)
                                                                            (list (AppC (IdC 'arr)
                                                                                        (list (NumC 3) (NumC 9)))
                                                                                  (NumC 0.5)
                                                                                  (NumC 12)))
                                                                      top-env
                                                                      test-store7)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp (AppC (IdC 'aset)
                                                                            (list (AppC (IdC 'arr)
                                                                                        (list (NumC 3) (NumC 9)))
                                                                                  (NumC 3)
                                                                                  (NumC 12)))
                                                                      top-env
                                                                      test-store7)))


;; interp-args tests
(check-equal? (interp-args (list (NumC 3) (AppC (IdC '+) (list (NumC 4) (NumC 5))))
                           top-env top-store
                           '())
              (VListStore (list (NumV 3) (NumV 9)) top-store))
(check-equal? (interp-args (list (NumC 3) (NumC 5) (AppC (IdC '+) (list (NumC 4) (NumC 5))))
                           top-env top-store
                           '())
              (VListStore (list (NumV 3) (NumV 5) (NumV 9)) top-store))
(check-equal? (interp-args '() top-env top-store '()) (VListStore '() top-store))


;; reverse-list tests
(check-equal? (reverse-list (list (NumV 1) (NumV 2) (NumV 3)) '()) (list (NumV 3) (NumV 2) (NumV 1)))


;; interp-primv tests
(check-equal? (interp-primv '+ (VListStore (list (NumV 7) (NumV 8)) top-store)) (VStore (NumV 15) top-store))
(check-equal? (interp-primv 'arr (VListStore (list (NumV 1) (NumV 3)) top-store)) (VStore (ArrayV 13 1) test-store4))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore (list (NumV 0) (NumV 1) (NumV 3)) top-store))))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore (list (NumV 3)) top-store))))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore '() top-store))))


 

;; lookup and fetch tests


;; serialize tests


;; extend-env tests


;; allocate tests

(check-equal? (allocate test-store1 4 (NumV 3)) (IStore test-store2 2))
(check-equal? (allocate (Store '() 0) 3 (StrV "test")) (IStore test-store3 0))
(check-exn #rx"OAZO runtime error in allocate" (lambda () (allocate test-store1 0 (NumV 3))))


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










