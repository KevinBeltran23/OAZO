#lang typed/racket
(require typed/rackunit)


;;;; Define the Arith language described in the textbook in chapter 3. Please feel free to copy code from the texbook.
(define-type ArithC (U numC plusC multC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)

;; interpret is given an AST in the form of an ArithC and evaluates it to a Real number
(define (interp [AST : ArithC]) : Real
  (match AST
    [(numC n) n]
    [ (plusC left-exp right-exp) ( + (interp left-exp) (interp right-exp) ) ] 
    [ (multC left-exp right-exp) ( * (interp left-exp) (interp right-exp) )  ]))

; interpret tests
(check-equal? (interp (numC 4)) 4)
(check-equal? (interp (plusC (numC 2) (numC 6))) 8)
(check-equal? (interp (multC (numC 2) (numC 6))) 12)
(check-equal? (interp (multC (plusC (numC 2) (numC 3))
                             (multC (numC 10) (numC 4))))
              200)



;; parse is given concrete syntax in the form of an s-expression and parses it into an ArithC
(define (parse [code : Sexp]) : ArithC
  (match code
    [(? real? x) (numC x)]
    [(list '* left-exp right-exp) (multC (parse left-exp) (parse right-exp))]
    [(list '+ left-exp right-exp) (plusC (parse left-exp) (parse right-exp))]
    [other (error 'parse "expected valid syntax, got ~e" other)]))


; parse tests
(check-equal? (parse '4) (numC 4))
(check-equal? (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
(check-equal? (parse '{* 1 2}) (multC (numC 1) (numC 2)))
(check-equal? (parse '{* {+ 2 3} {* 10 4}})
              (multC (plusC (numC 2) (numC 3))
                     (multC (numC 10) (numC 4))))
(check-exn #rx"syntax" (lambda () (parse '{1 2 3})))



;; printer prints a give  AST
(define (printer [AST : ArithC]) : String
  (match AST
    [(numC n) (format "~v" n)]
    [(plusC l r) (string-append "PlusC: (" (printer l) "), (" (printer r) ")")]
    [(multC l r) (string-append "MultC: (" (printer l) "), (" (printer r) ")")]))

; printer tests
(check-equal? (printer (numC 4)) "4")
(check-equal? (printer (multC (plusC (numC 2) (numC 3))
                              (multC (numC 10) (numC 4))))
              "MultC: (PlusC: (2), (3)), (MultC: (10), (4))")



;; top-interp accepts an s-expression, calls parse with the s-expression,
;; then interprets the results of the parser with interp
(define (top-interp [code : Sexp]) : Real
  (interp (parse code)))

; top-interp tests
(check-equal? (top-interp '{* {+ 2 3} {* 5 4}}) 100)
(check-equal? (top-interp '{* {+ 2 4} {+ 10 5}}) 90)
(check-equal? (top-interp '{* {+ 3 4} {* {* 2 4} 7}}) 392)
(check-exn #rx"syntax" (lambda () (top-interp '{* 7})))

