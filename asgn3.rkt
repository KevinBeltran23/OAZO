#lang typed/racket
(require typed/rackunit)

;; Define the Arith language described in the textbook in chapter 3. Please feel free to copy code from the texbook.
(define-type ArithC (U numC binopC))
(struct numC
  ([n : Real]) #:transparent)
(struct binopC
  ([op : Symbol] [l : ArithC] [r : ArithC]) #:transparent)

 
;; interpret is given an AST in the form of an ArithC and evaluates it to a Real number
(define (interp [AST : ArithC]) : Real
  (match AST
    [(numC n) n]
    [ (binopC op l r)
      (match op
       ['+ (+ (interp l) (interp r))]
       ['* (* (interp l) (interp r))]
       ['- (- (interp l) (interp r))]
       ['/ (/ (interp l) (interp r))])]))
    
; interpret tests
(check-equal? (interp (numC 4))
              4)
(check-equal? (interp (binopC '+ (numC 2) (numC 6)))
              8)
(check-equal? (interp (binopC '* (numC 2) (numC 6)))
              12)
(check-equal? (interp (binopC '* (binopC '+ (numC 2) (numC 3)) (binopC '* (numC 10) (numC 4))))
              200)
  

;; parse is given concrete syntax in the form of an s-expression and parses it into an ArithC
(define (parse [code : Sexp]) : ArithC
  (match code
    [(? real? x) (numC x)]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [other (error 'parse "OAZO: expected valid syntax, got ~e" other)]))


; parse tests
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
 

;; printer prints a given AST
(define (printer [AST : ArithC]) : String
  (match AST
    [(numC n) (format "~v" n)]
    [(binopC '+ l r) (string-append "PlusC: (" (printer l) "), (" (printer r) ")")]
    [(binopC '* l r) (string-append "MultC: (" (printer l) "), (" (printer r) ")")]))

; printer tests
(check-equal? (printer (numC 4)) "4")
(check-equal? (printer (binopC '* (binopC '+ (numC 2) (numC 3))
                              (binopC '* (numC 10) (numC 4))))
              "MultC: (PlusC: (2), (3)), (MultC: (10), (4))")


;; top-interp accepts an s-expression, calls parse with the s-expression,
;; then interprets the results of the parser with interp
(define (top-interp [code : Sexp]) : Real
  (interp (parse code)))

; top-interp tests
(check-equal? (top-interp '{* {+ 2 3} {* 5 4}})
              100)
(check-equal? (top-interp '{* {+ 2 4} {+ 10 5}})
              90)
(check-equal? (top-interp '{* {+ 3 4} {* {* 2 4} 7}})
              392)
(check-equal? (top-interp '{- 5 12})
              -7)
(check-equal? (top-interp '{/ 20 {* 2 5}})
              2)
(check-exn #rx"OAZO: expected valid syntax" (lambda () (top-interp '{* 7})))
(check-exn #rx"OAZO: expected valid syntax" (lambda () (top-interp '{^ 2 4})))


