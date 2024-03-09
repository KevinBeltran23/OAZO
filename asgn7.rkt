#lang typed/racket
(require typed/rackunit)

;;;; ---- NOTES ----




; We did not fully implement assignment 7

; We implemented mutation with full test coverage
; then almost finished the type-checker
; Incomplete:
;    types in type-env for 'seq', 'error', and ':='
;    some tests are still causing errors. The type-checker seems to be incorrect in at least 1 spot.
;        (we ran out of time & were unable to test it much or fix errors)
;    we do not have full test coverage
 



;; prog -> |Parse| -> AST -> |TC| -> AST -> |interp| -> value

;;;; ---- TYPE DEFINITIONS ----


;; ExprC is either a number (NumC), an id (IdC), a string (StrC),
;; an if ____ else ____ then ____ expression (IfC),
;; an anonymous function definition (LamC),
;; or a function application (AppC)
(define-type ExprC (U NumC IdC StrC IfC LamC LamCT AppC ParamC))

(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC  ([test-expr : ExprC] [then-expr : ExprC] [else-expr : ExprC]) #:transparent)
(struct LamCT ([params : (Listof ParamC)] [body : ExprC]) #:transparent)
(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct ParamC ([id : IdC] [type : Type]) #:transparent)


;; Value is either a number, a boolean, a string, a closure, or a primitive operator
(define-type Value (U NumV BoolV StrV CloV PrimV ArrayV NullV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent) 
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof IdC)] [body : ExprC] [env : Environment]) #:transparent)
(struct PrimV ([s : Symbol]) #:transparent)
(struct ArrayV ([location : Integer] [length : Integer]) #:transparent)
(struct NullV () #:transparent)


;; A Type is either a built in BaseT or a user defined {ty ... -> ty}
(define-type Type(U NumT BoolT StrT VoidT ArrayT UserT))

(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct VoidT () #:transparent)
(struct ArrayT () #:transparent)
(struct UserT ([inputs : (Listof Type)] [output : Type]) #:transparent)

 
;; An Environment is a list of Bindings
(define-type Environment(Listof Binding))

 
;; A Binding consists of a Symbol (id) and an Integer (location)
(struct Binding([id : Symbol] [location : Integer]) #:transparent)


;; A Tenv is a list of TBindings
(define-type TEnvironment(Listof TBinding))


;; A TBinding consists of a Symbol and an associated Type
(struct TBinding ([id : Symbol] [ty : Type]) #:transparent)


;; A Store is a list of Cells and an integer representing the length of the store
(struct Store ([cells : (Listof Cell)] [length : Integer]) #:transparent)


;; A Cell consists of a location (Integer) and a val (Value)
(struct Cell([location : Integer] [val : Value]) #:transparent)


;; A VStore is a value combined with a Store
(struct VStore([val : Value] [sto : Store]) #:transparent)


; A VListStore is a list of values (args) combined with a Store
(struct VListStore([args : (Listof Value)] [sto : Store]) #:transparent)


; An IStore is  a base lcoation (Integer) combined with a Store 
(struct IStore([base : Integer] [store : Store]) #:transparent)


; An EStore is an Environment combined with a Store
(struct EStore([env : Environment] [store : Store]) #:transparent)


;; top-tenv binds a symbol to it's type
(define top-tenv 
  (list (TBinding '+ (UserT (list (NumT) (NumT)) (NumT)))
        (TBinding '- (UserT (list (NumT) (NumT)) (NumT)))
        (TBinding '* (UserT (list (NumT) (NumT)) (NumT)))
        (TBinding '/ (UserT (list (NumT) (NumT)) (NumT)))
        (TBinding '<= (UserT (list (NumT) (NumT)) (BoolT)))
        (TBinding 'num-eq? (UserT (list (NumT) (NumT)) (BoolT)))
        (TBinding 'str-eq? (UserT (list (StrT) (StrT)) (BoolT)))
        (TBinding 'arr-eq? (UserT (list (ArrayT) (ArrayT)) (BoolT)))
        (TBinding 'substring (UserT (list (StrT) (NumT) (NumT)) (StrT)))
        (TBinding 'arr (UserT (list (NumT) (NumT)) (ArrayT)))
        (TBinding 'aref (UserT (list (ArrayT) (NumT)) (NumT)))
        (TBinding 'aset (UserT (list (ArrayT) (NumT) (NumT)) (VoidT)))
        (TBinding 'alen (UserT (list (ArrayT)) (NumT)))
        (TBinding 'true (BoolT))
        (TBinding 'false (BoolT))
        #;(TBinding 'seq (UserT '() (NumT)))
        #;(TBinding 'error (UserT (StrT (NumT)) (VoidT)))
        #;(TBinding ':= (UserT (list (StrT) (NumT)) (VoidT)))))


;; top-env binds primitive Values to their corresponding ...  NEEDS DESCRIPTION ----------------------------
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
        (Binding 'alen 12)
        (Binding 'true 13)
        (Binding 'false 14)
        (Binding 'seq 15)
        (Binding 'error 16)
        (Binding ':= 17)))
    
;; top-store  NEEDS DECSRIPTION ----------------------------
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
               (Cell 12 (PrimV 'alen))
               (Cell 13 (BoolV #t))
               (Cell 14 (BoolV #f))
               (Cell 15 (PrimV 'seq))
               (Cell 16 (PrimV 'error))
               (Cell 17 (PrimV ':=)))
         18))




;;;; ---- TOP-INTERP and INTERP ----



   
;; top-interp is given a program in the form of an s-expression and:
;; - parses the s-expression into an ExprC representing the AST
;; - interprets the AST into a Value representing the result of the program
;; - serializes the Value by printing it as a string
(define (top-interp [s : Sexp]) : String
  (define AST (parse s))
  (type-check AST top-tenv)
  (serialize (interp (strip-types AST) top-env top-store)))
  
 
;; interp
;; - given an ExprC and an environment
;; - Interprets the expression with the given environment
(define (interp [exp : ExprC] [env : Environment] [store : Store]) : VStore
  (match exp 
    [(NumC n) (VStore (NumV n) store)]
    [(IdC s) (VStore (fetch (lookup s env) store) store)]
    [(StrC s) (VStore (StrV s) store)]
    [(IfC test-expr then-expr else-expr)
     (match (interp test-expr env store) 
       [(VStore b s) (match b 
                       [(BoolV #t) (interp then-expr env s)]
                       [(BoolV #f) (interp else-expr env s)]
                       [other (error 'interp "OAZO runtime error in interp:
                              comparison ~e returned non-boolean value of ~e"
                                     test-expr other)])])] 
    [(LamC params body)
     (VStore (CloV params body env) store)]   
    [(AppC func args) 
     (match (interp func env store)
       [(VStore (PrimV ':=) new-store)
        (match args
          [(list (IdC s) e) (interp-mutate s env (interp e env new-store))]
          [other (call-interp-primv-error2 ':=)])]
       [(VStore (PrimV s) new-store) (interp-primv s (interp-args args env new-store '()))]
       [(VStore (CloV params body clov-env) new-store)
        (match (interp-args args env new-store '())
          [(VListStore vls-list vls-store)
           (match (create-appc-bindings params vls-list '() vls-store '())
             [(EStore estore-env estore-store)
              (interp body
                      (extend-env clov-env
                                  estore-env)
                      estore-store)])])])]))

   
  
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
    [(list old ':= new) (AppC (IdC ':=) (list (parse old) (parse new)))]
    [(list 'let bindings ... body) (parse-let (cast bindings (Listof Sexp)) body)]
    [(list 'anon (list params ...) ': body)
     (LamCT (parse-params params '()) (parse body))]
    [(list 'quote args ...) (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]
    [(list 'seq args ...)
     (AppC (IdC 'seq) (map (λ ([arg : Sexp]) (parse arg)) args))]
    [(list func args ...)
     (AppC (parse func) (map (λ ([arg : Sexp]) (parse arg)) args))]
    [other (error 'parse "OAZO syntax error in parse: expected valid syntax, got ~e" s)]))


;; parse-let takes a list of Sexps and a body and turns it into an ExprC
(define (parse-let [bindings : (Listof Sexp)] [body : Sexp]) : ExprC
  (AppC (LamCT (find-names bindings '()) (parse body))
        (find-exprs bindings)))


;; find-names is given a list of Sexps, bindings, and returns a list of IdCs
;; corresponding to the names in the bindings
(define (find-names [bindings : (Listof Sexp)] [seen : (Listof Sexp)]) : (Listof ParamC)
  (match bindings
    ['() '()]
    [(cons (list (list (? symbol? name) ': type) '<- expr) r-bindings)
     (cond
       [(and (not-reserved? name) (argInList? name seen))
        (error 'find-names "OAZO syntax error in find-names: ~e defined multiple times" name)]
       [else (cons (ParamC (IdC name) (parse-type type)) (find-names r-bindings (cons name seen)))])]
    [other (error 'find-names "OAZO syntax error in find-names: expected valid let syntax, got ~e" other)]))


;; find-exprs is given a list of Sexps, bindings, and returns a list of ExprCs
;; corresponding to the exprs in the bindings
(define (find-exprs [bindings : (Listof Sexp)]) : (Listof ExprC)
  (match bindings
    ['() '()]
    [(cons (list name '<- expr) r-bindings) (cons (parse expr) (find-exprs r-bindings))]))


;; parse-params is given a list of any and turns in into a list of IdCs.
;; if one of the elements of the given list is not a symbol or a reserved name, then it raises an error.
(define (parse-params [params : (Listof Sexp)] [seen : (Listof Any)]) : (Listof ParamC)
  (match params
    ['() '()]
    [(cons (list type (? symbol? s)) r) #:when
                            (not-reserved? s)#;(and (not (argInList? f seen)) (not-reserved? f))
                            (cons (ParamC (IdC s) (parse-type type)) (parse-params r (cons s seen)))]
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




;;;; ---- TYPE CHECKING


;; type-check
;; - returns the type of an ExprC
(define (type-check [exp : ExprC] [tenv : TEnvironment]) : Type
  (match exp
    [(NumC n) (NumT)]
    [(StrC str) (StrT)]
    [(IdC s) (lookup-type s tenv)]
    [(IfC test-expr then-expr else-expr)
     (define test-type (type-check test-expr tenv))
     (define then-type (type-check then-expr tenv))
     (define else-type (type-check else-expr tenv))
     (cond
       [(and (equal? then-type else-type)
             (equal? test-type (BoolT)))
        then-type]
       [else (error 'type-check "OAZO type error in type-check: non-matching types given to if: ~e" exp)])]
    [(AppC func args)
     (define func-type (type-check func tenv))
     (cond
       [(UserT? func-type) (cond
                             [(equal? (UserT-inputs func-type) (map (λ ([arg : ExprC])
                                                                      (type-check arg tenv))
                                                                    args))
                              (UserT-output func-type)]
                             [else (error 'type-check "OAZO type error in type-check:
                                                       tried to call a function with the wrong arguments: ~e" exp)])]
       [else (error 'type-check "OAZO type error in type-check: tried to call a non function expr: ~e" exp)])]
    [(LamCT params body)
     (UserT (map (λ ([param : ParamC])
                   (ParamC-type param))
                 params)
            (type-check body (extend-type-env tenv  (map (λ ([param : ParamC])
                                                           (TBinding (IdC-s (ParamC-id param)) (ParamC-type param)))
                                                         params))))]))


;; strip-types takes an ExprC and strips it of it's types
(define (strip-types [exp : ExprC]) : ExprC
  (match exp 
    [(NumC n) exp]
    [(IdC s) exp]
    [(StrC s) exp]
    [(IfC test-expr then-expr else-expr) (IfC (strip-types test-expr) (strip-types then-expr) (strip-types else-expr))]
    [(LamCT params body) (LamC (map (λ ([param : ParamC]) (ParamC-id param)) params) (strip-types body))]   
    [(AppC func args) (AppC (strip-types func) (map (λ ([arg : ExprC]) (strip-types arg)) args))]))
 

;; parse-type
;; - determines the type associated with the given symbol
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StrT)]
    ['void (VoidT)]
    ['numarray (ArrayT)]
    [(list inTypes ... '-> outType) (UserT (map (λ ([inType : Sexp])
                                                  (parse-type inType))
                                                (cast inTypes (Listof Sexp)))
                                           (parse-type outType))]))


;; extend-type-env takes a starting type environment and extends it with a new type environemnt
(define (extend-type-env [start : TEnvironment] [new : TEnvironment]) : TEnvironment
  (match new
    ['() start]
    [(cons f-binding r-bindings) (cons f-binding (extend-type-env start r-bindings))]))

 
;; lookup-type is given a type and an type environment, and looks for that type in the type environment
(define (lookup-type [s : Symbol] [tenv : TEnvironment]) : Type
  (cond 
    [(null? tenv) (error 'lookup-type "OAZO runtime error in lookup-type: Type ~e not found in TEnvironment" s)]
    [else (match (first tenv)
            [(TBinding symb ty) #:when (equal? symb s) ty]
            [other (lookup-type s (rest tenv))])]))

  
 
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


;; lookup is given a symbol and an environment, and looks for that symbol in the environment.
;; If Lookup function to retrieve a Location from environment
(define (lookup [s : Symbol] [env : Environment]) : Integer
  (cond 
    [(null? env) (error 'lookup-binding "OAZO runtime error in lookup: Symbol ~e not found in environment" s)]
    [else (match (first env)
            [(Binding symb location) #:when (equal? symb s) location]
            [other (lookup s (rest env))])]))
 
  
;; fetch is given an Integer (l) and a store, and retrieves the value at location l from the store
(define (fetch [l : Integer] [store : Store]) : Value
  (match store
    [(Store '() n) (error 'lookup-binding "OAZO runtime error in fetch: Location ~e not found in store" l)]
    [(Store (cons (Cell location val) r) n) #:when (equal? location l) val]
    [(Store (cons f r) n) (fetch l (Store r n))]))
 

;; interp-args is given a list of ExprC's (args), an Environment (env),
;; a Store (store), and an initially empty list (vals) and
;; returns a list of Values, corresponding to the interping of each ExprC, combined with a new store
(define (interp-args [args : (Listof ExprC)]
                     [env : Environment]
                     [store : Store]
                     [vals : (Listof Value)]) : VListStore
  (match args
    ['() (VListStore (cast (reverse-list vals '()) (Listof Value)) store)]
    [(cons f r) (match (interp f env store)
                  [(VStore v s) (interp-args r env s (cons v vals))])]))
 

;; reverse-list is given an input list and an initially empty output list, and reverses the input list
(define (reverse-list [input : (Listof Any)] [output : (Listof Any)]) : (Listof Any)
  (match input
    ['() output]
    [(cons f r) (reverse-list r (cons f output))]))

 
;; interp-primv is given a symbol corresponding to a primitive function and
;; a list of values corresponding to the arguments said function is being called with.
;; it then evaluates the primitive operator with the given arguments
(define (interp-primv [op : Symbol] [vliststore : VListStore]): VStore
  (define store (VListStore-sto vliststore))
  (define args (VListStore-args vliststore))
  (match args
    [(list arg1) (match op 
                   ['error (error 'user-error "OAZO user error: ~e" (serialize (VStore arg1 store)))]
                   ['alen (cond
                            [(ArrayV? arg1) (VStore (NumV (ArrayV-length arg1)) store)]
                            [else (call-interp-primv-error2 op)])]
                   ['seq (VStore arg1 store)]
                   [other (call-interp-primv-error1 op)])]
    [(list (NumV n1) (NumV n2)) (match op
                                  ['+ (VStore (NumV (+ n1 n2)) store)] 
                                  ['- (VStore (NumV (- n1 n2)) store)]
                                  ['* (VStore (NumV (* n1 n2)) store)]
                                  ['/ (cond
                                        [(equal? n2 0) (error 'interp-primv "OAZO runtime error in interp-primv:
                                                                              tried to divide by 0")]
                                        [else (VStore (NumV (/ n1 n2)) store)])]
                                  ['<= (VStore (BoolV (<= n1 n2)) store)]
                                  ['arr (cond
                                          [(integer? n1)
                                           (match (allocate store n1 (NumV n2))
                                             [(IStore base new-store) (VStore (ArrayV base (cast n1 Integer))
                                                                              new-store)])]
                                          [else (call-interp-primv-error2 op)])]
                                  ['num-eq? (VStore (BoolV (equal? n1 n2)) store)]
                                  ['seq (VStore (NumV n2) store)]
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
                        ['seq (VStore arg2 store)]
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
    [(list argList ...)
     (match op
       ['seq (match args
               [(cons f r) (interp-primv op (VListStore r store))])]
       [other (call-interp-primv-error1 op)])]))


;; mutate-array is given a store (inputStore), a location in that store (loc), and a real number (newval)
;; and mutates the value at location loc in store to be newval
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
(define (create-appc-bindings [params : (Listof IdC)] [args : (Listof Value)] [seen : (Listof Symbol)]
                              [input-store : Store] [build-env : Environment]) : EStore
  (match params
    ['() (match args
           ['() (EStore (cast (reverse-list build-env '()) Environment) input-store)]
           [other (error 'create-appc-bindings "OAZO runtime error in create-appc-bindings: too many args")])]
    [(cons (IdC s) r-params)
     (cond 
       [(argInList? s seen)
        (error 'create-appc-bindings "OAZO runtime error in create-appc-bindings:
                                      multiple params of name ~e" s)]
       [else (match args
               [(cons f-arg r-args)
                (match (allocate input-store 1 f-arg)
                  [(IStore base new-store)
                   (create-appc-bindings r-params r-args (cons s seen) new-store (cons (Binding s base) build-env))])]
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


;; allocate accepts a store, a number of locations to be allocated, and a value to place in all of them
;; - returns two things; the base location, and an extended store
(define (allocate [store : Store] [num : Real] [val : Value]) : IStore
  (cond
    [(and (integer? num) (> num 0))
     (match store
       [(Store cells length)
        (IStore length (Store (allocate-helper cells (cast num Integer) val length) (cast (+ length num) Integer)))])]
    [else (error 'allocate "OAZO runtime error in allocate: tried to allocate fewer than 1 cell")]))


;; allocate-helper recursively creates new cells in the store
(define (allocate-helper [start-cells : (Listof Cell)] [num : Integer] [val : Value] [next-loc : Integer]):(Listof Cell)
  (match start-cells
    ['() (cond
           [(equal? num 0) '()]
           [else (cons (Cell next-loc val) (allocate-helper '() (- num 1) val (+ next-loc 1)))])]
    [(cons f r) (cons f (allocate-helper r num val next-loc))]))

;; interp-mutate mutates a store
(define (interp-mutate [s : Symbol] [env : Environment] [vstore : VStore]) : VStore
  (match vstore
    [(VStore val store)
     (VStore (NullV)
             (Store (update-store (Store-cells store) (find-loc s env) val)
                    (Store-length store)))]))

;; update-store returns a new store with additional cells
(define (update-store [cells : (Listof Cell)] [loc : Integer] [val : Value]) : (Listof Cell)
  (match cells
    ['() '()]
    [(cons (Cell cloc cval) r)
     (cond
       [(equal? cloc loc) (cons (Cell loc val) (update-store r loc val))]
       [else (cons (Cell cloc cval) (update-store r loc val))])]))

;; find-loc returns the location of a given symbol in an environment
(define (find-loc [s : Symbol] [env : Environment]) : Integer
  (match env
    ['() (error 'interp "OAZO runtime error in interp-mutate:
                         tried to mutate binding ~e, but binding ~e doesn't exist" s s)]
    [(cons (Binding bs bi) r) (cond
                                [(equal? bs s) bi]
                                [else (find-loc s r)])]))


 

;;;; ---- Testing ----



#;(
   
;; store definitions for tests
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
               (Cell 13 (BoolV #t))
               (Cell 14 (BoolV #f))
               (Cell 15 (PrimV 'seq))
               (Cell 16 (PrimV 'error))
               (Cell 17 (PrimV ':=))
               (Cell 18 (NumV 3)))
         19))
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
               (Cell 13 (BoolV #t))
               (Cell 14 (BoolV #f))
               (Cell 15 (PrimV 'seq))
               (Cell 16 (PrimV 'error))
               (Cell 17 (PrimV ':=))
               (Cell 18 (NumV 3))
               (Cell 19 (NumV 3))
               (Cell 20 (NumV 3))
               (Cell 21 (NumV 3)))
         22))
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
               (Cell 13 (BoolV #t))
               (Cell 14 (BoolV #f))
               (Cell 15 (PrimV 'seq))
               (Cell 16 (PrimV 'error))
               (Cell 17 (PrimV ':=))
               (Cell 18 (NumV 9))
               (Cell 19 (NumV 9))
               (Cell 20 (NumV 12))
               (Cell 21 (NumV 12))
               (Cell 22 (NumV 12)))
         23))
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
               (Cell 13 (BoolV #t))
               (Cell 14 (BoolV #f))
               (Cell 15 (PrimV 'seq))
               (Cell 16 (PrimV 'error))
               (Cell 17 (PrimV ':=))
               (Cell 18 (NumV 9))
               (Cell 19 (NumV 12))
               (Cell 20 (NumV 9)))
         21))
 

;; program definitions for tests
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
(define prog13 '{{anon {[num x]} : {+ 3 x}} 7})
(define prog14 '{{anon {[{{num -> num} {num -> num} -> {num -> num}} compose] [{num -> num} add1]} :
                       {{anon {[{num -> num} add2]} :
                              {add2 99}}   
                        {compose add1 add1}}}
                 {anon {[{num -> num} f] [{num -> num} g]} :
                       {anon {[num x]} :
                             {f {g x}}}}
                 {anon {[num x]} :
                       {+ x 1}}})
(define prog14.5 '{{anon {[{{num -> num} {num -> str} -> {num -> num}} compose] [{num -> num} add1]} :
                       {{anon {[{num -> num} add2]} :
                              {add2 99}}   
                        {compose add1 add1}}}
                 {anon {[{num -> num} f] [{num -> num} g]} :
                       {anon {[num x]} :
                             {f {g x}}}}
                 {anon {[num x]} :
                       {+ x 1}}})
(define prog16 '{if {<= 1 1} then "one" else "two"})
(define prog17 '{seq {+ 2 3}})
(define prog18 '{seq {+ 2 3} {+ 4 5}})
(define prog19 '{{anon {[numarray a]} : {seq {aset a 1 20}
                                             {aset a 2 300}
                                             {+ {aref a 0} {+ {aref a 1} {aref a 2}}}}}
                 {arr 3 1}})
(define prog20 '{aset {arr 10 5} 9 100})
(define prog21 '{if {<= 3 1} then "one" else "two"})
(define prog22 '{error "wawaweewa"})
(define prog23 '{/ 3 0})
(define prog24 '{f a})
(define prog25 '{{anon {[num x] [num y] [num z]} : 7} 1 2 3 4})
(define prog26 '{{anon {[num x] [num y] [num z]} : 7} 1 2})
(define prog27 '{{anon {[num x] [num y] [num x]} : 7} 1 2 3})
(define prog28 '{if 1 then 2 else 3})
(define prog29 '{quote bro ski})
(define prog30 '{aref {arr 5 1} 10})
(define prog31 '{alen 7})
(define prog32 '{str-eq? 7 8})
(define prog33 '{{anon {[numarray a]} : {arr-eq? a a}}
                 {arr 3 1}})
(define prog34 '{str-eq? 7 "yuh"})
(define prog35 '{arr-eq? 7 "yuh"})
(define prog36 '{aref "yuh" "yuh"})
(define prog37 '{+ "yuh" "yuh"})
(define prog38 '{error "yuh" "yuh"})
(define prog39 '{+ 1 2 3 4 5})
(define prog40 '{+ {arr 2 3} 8 9})
(define prog41 '{{anon {[num x]} : {seq {x := 3} {+ x 1}}} 7})
(define prog42 '{{anon {[num x]} : {seq {x := 3 4} {+ x 1}}} 7})
(define prog43 '{{anon {[num x]} : {seq {x :=} {+ x 1}}} 7})
(define prog44 '{{anon {[num x]} : {seq {:= x 3} {+ x 1}}} 7})
(define prog45 '{{anon {[num x]} : {seq {x 3 :=} {+ x 1}}} 7})
(define prog46 '{{anon {[num x] [num y] [num z]} : {seq {z := 3} {+ z 1}}} 7 8 9})
(define prog47 '{{anon {[num x]} : {seq {3 := 3} {+ z 1}}} 7})
(define prog48 '{{anon {[num x]} : {seq {y := 3} {+ z 1}}} 7})


 

;; top-interp tests
(check-equal? (top-interp '{+ 3 4}) "7")
(check-equal? (top-interp '{* 3 4}) "12")
(check-equal? (top-interp '{- {/ {* {+ 4 5} {+ 12 18}} 6} 5}) "40")
(check-equal? (top-interp '{anon {[num x]} : {* 3 x}}) "#<procedure>")
(check-equal? (top-interp '{anon {[num x] [num y]} : {+ y x}}) "#<procedure>")
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
(check-equal? (top-interp prog13) "10")
(check-equal? (top-interp prog14) "101")
(check-equal? (top-interp prog16) "\"one\"")
;(check-equal? (top-interp prog17) "5")
;(check-equal? (top-interp prog18) "9")
;(check-equal? (top-interp prog19) "321")
(check-equal? (top-interp prog20) "null")
(check-equal? (top-interp prog21) "\"two\"")
(check-equal? (top-interp prog33) "true")
;(check-equal? (top-interp prog41) "4")
;(check-equal? (top-interp prog46) "4")
(check-equal? (top-interp prog14.5) "THIS SHOULD NOT WORK")
(check-exn #rx"OAZO runtime error in allocate" (lambda () (top-interp '{arr 0 79})))
(check-exn #rx"OAZO runtime error in allocate" (lambda () (top-interp '{arr -1 79})))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog5)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog6)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog7)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog8)))
(check-exn #rx"OAZO user error" (lambda () (top-interp prog22)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog23)))
(check-exn #rx"OAZO runtime error in lookup" (lambda () (top-interp prog24)))
(check-exn #rx"OAZO runtime error in create-appc-bindings" (lambda () (top-interp prog25)))
(check-exn #rx"OAZO runtime error in create-appc-bindings" (lambda () (top-interp prog26)))
(check-exn #rx"OAZO runtime error in create-appc-bindings" (lambda () (top-interp prog27)))
(check-exn #rx"OAZO runtime error in interp" (lambda () (top-interp prog28)))
(check-exn #rx"OAZO syntax error in parse" (lambda () (top-interp prog29)))
(check-exn #rx"OAZO runtime error in interp" (lambda () (top-interp prog30)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog31)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog32)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog34)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog35)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog36)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog37)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog38)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog39)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog40)))
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (top-interp prog42)))
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (top-interp prog43)))
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (top-interp prog44)))
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (top-interp prog45)))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (top-interp prog47)))
(check-exn #rx"OAZO runtime error in interp-mutate" (lambda () (top-interp prog48)))
(check-exn #rx"OAZO dafuq goin on" (lambda () (top-interp prog14.5)))


;; interp tests
(check-equal? (interp (NumC 5) top-env top-store) (VStore (NumV 5) top-store))
(check-equal? (interp (AppC (IdC '+) (list (NumC 2) (NumC 10))) top-env top-store)
              (VStore (NumV 12) top-store))
(check-equal? (interp (AppC (IdC 'arr) (list (NumC 1) (NumC 3))) top-env top-store) (VStore (ArrayV 18 1) test-store4))
(check-equal? (interp (AppC (IdC 'arr) (list (NumC 4) (NumC 3))) top-env top-store) (VStore (ArrayV 18 4) test-store5))
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
(check-equal? (interp-primv 'arr (VListStore (list (NumV 1) (NumV 3)) top-store)) (VStore (ArrayV 18 1) test-store4))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore (list (NumV 0) (NumV 1)
                                                                                                 (NumV 3)) top-store))))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore (list (NumV 3)) top-store))))
(check-exn #rx"OAZO runtime error in interp-primv" (lambda () (interp-primv '+ (VListStore '() top-store))))


;; lookup and fetch tests
(check-exn #rx"OAZO runtime error in fetch" (lambda () (fetch 100 top-store)))


;; serialize tests


;; extend-env tests


;; allocate tests
(check-equal? (allocate test-store1 4 (NumV 3)) (IStore 2 test-store2))
(check-equal? (allocate (Store '() 0) 3 (StrV "test")) (IStore 0 test-store3))
(check-exn #rx"OAZO runtime error in allocate" (lambda () (allocate test-store1 0 (NumV 3))))


;; create-appc-bindings tests

 

;; misc test for implementing type checking
;(check-equal? (interp (type-checker (NumC 12) top-tenv) top-env top-store) (VStore (NumV 12) top-store))
;(check-equal? (serialize (interp (type-checker (NumC 12) top-tenv) top-env top-store)) "12")
;(check-equal? (interp (type-checker (IfC (IdC 'true) (NumC 1) (NumC 2)) top-tenv) top-env top-store)
;              (VStore (NumV 1) top-store))
;(check-equal? (serialize (interp (type-checker (IfC (IdC 'true) (NumC 1) (NumC 2)) top-tenv) top-env top-store)) "1")
(check-equal? (top-interp '{if true then 1 else 2}) "1")
(check-equal? (top-interp '{if {<= 2 2} then 1 else 2}) "1")
(check-equal? (top-interp '{+ 2 1}) "3")

(check-exn #rx"OAZO type error in type-check" (lambda () (top-interp '{if true then "Bob" else 2}))) 
 

;; type-checker tests
;(check-equal? (type-checker (NumC 12) top-tenv) (NumC 12))


;; type-check tests
(check-equal? (type-check (StrC "hello") top-tenv) (StrT))


;; lookup-type tests
(check-equal? (lookup-type 'num top-tenv) (NumT))
(check-equal? (lookup-type 'bool top-tenv) (BoolT))


;; parse-type tests
(check-equal? (parse-type '{num bool numarray -> void}) (UserT (list (NumT) (BoolT) (ArrayT)) (VoidT)))
(check-equal? (parse-type '{{num num -> num} num -> num})
              (UserT (list (UserT (list (NumT) (NumT)) (NumT)) (NumT)) (NumT)))
(check-equal? (parse-type '{-> num}) (UserT '() (NumT)))




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
              (LamCT '() (NumC 4)))
(check-equal? (parse '{anon {[num x]} : 4})
              (LamCT (list (ParamC (IdC 'x) (NumT))) (NumC 4)))
(check-equal? (parse '{anon {[num x] [num y]} : 4})
              (LamCT (list (ParamC (IdC 'x) (NumT)) (ParamC (IdC 'y) (NumT))) (NumC 4)))
(check-equal? (parse '{anon {[num x] [num y] [num z]} : 4})
              (LamCT (list (ParamC (IdC 'x) (NumT)) (ParamC (IdC 'y) (NumT)) (ParamC (IdC 'z) (NumT))) (NumC 4)))
(check-equal? (parse '{anon {[num +] [num y] [num z]} : 4})
              (LamCT (list (ParamC (IdC '+) (NumT)) (ParamC (IdC 'y) (NumT)) (ParamC (IdC 'z) (NumT))) (NumC 4)))
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
(check-equal? (parse '{{anon {[{-> num} add]} :
                             {* 2 {add}}}
                       {anon {} : {+ 3 7}}})
              (AppC (LamCT (list (ParamC (IdC 'add) (UserT '() (NumT))))
                          (AppC (IdC '*)
                                (list (NumC 2) (AppC (IdC 'add) '()))))
                    (list (LamCT '()
                                (AppC (IdC '+)
                                      (list (NumC '3) (NumC '7)))))))
(check-equal? (parse '{let
                        {[z : num] <- {+ 9 14}}
                        {[y : num] <- 98}
                        {+ z y}}) 
              (AppC (LamCT (list (ParamC (IdC 'z) (NumT)) (ParamC (IdC 'y) (NumT)))
                          (AppC (IdC '+)
                                (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+)
                                (list (NumC 9) (NumC 14)))
                          (NumC 98))))
#;(
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
(check-exn #rx"OAZO syntax error in not-reserved" (lambda () (parse '(+ then 4))))
(check-exn #rx"OAZO syntax error in find-names" (lambda () (parse '(let (z <- (anon () : 3)) (z <- 9) (z)))))
(check-exn #rx"OAZO syntax error in not-reserved?" (lambda () (parse '(let (: <- "") "World"))))
)

;; find-names tests
(check-exn #rx"OAZO syntax error in find-names" (lambda () (find-names (cons 7 '()) '())))

)


