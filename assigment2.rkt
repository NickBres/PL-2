#lang pl
#|
Question 1
__________
Function takes list of numbers and returns sum of their squares.
Using foldl and lambda functions.
|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares numbers)
  (foldl (lambda ([x : Number] [acc : Number]) : Number (+ acc (* x x))) 0 numbers))

;; Question 1 tests
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(4)) => 16)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(1 -2 3 -4)) => 30)

#|
Question 2.a
__________
Funtion create polynomial, that takes k numbers and returns polynom function, that takes x
and calculates polynom with x and k numbers as coefficients
|#

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coefs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL) accum
        (poly (cdr argsL) x (add1 power) (+ accum (* (car argsL) (expt x power))))
        )
    )
    (: polyX : Number -> Number )
    (define (polyX x)
      (poly coefs x 0 0)
      )
  polyX
)

;; tests for 2.a
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
(+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) =>
(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) =>
(+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) =>
(+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(test (p2345 -1) =>
(+ (* 2 (expt -1 0)) (* 3 (expt -1 1)) (* 4 (expt -1 2)) (* 5 (expt -1 3))))
(test (p2345 0.5) =>
(+ (* 2 (expt 0.5 0)) (* 3 (expt 0.5 1)) (* 4 (expt 0.5 2)) (* 5 (expt 0.5 3))))
(define pLarge (createPolynomial '(1000 2000 3000)))
(test (pLarge 100) =>
(+ (* 1000 (expt 100 0)) (* 2000 (expt 100 1)) (* 3000 (expt 100 2))))

(define pneg (createPolynomial '(1 -2 1)))
(test (pneg -1) =>
(+ (* 1 (expt -1 0)) (* -2 (expt -1 1)) (* 1 (expt -1 2))))


#|
Question 2.b
____________

Language PLANG that supports evaluating a polynomial on a sequence of points (numbers)

|#

#| I
  The grammar:
    <PLANG> ::= {{poly AEs} {AEs}}
    <AEs>   ::= <AE>
              | <AE> <AEs>
    <AE>    ::= <num>
              |{+ <AE> <AE>}
              |{- <AE> <AE>}
              |{* <AE> <AE>}
              |{/ <AE> <AE>}
|#

;; II


(define-type AE
    [Num  Number]
    [Add  AE AE]
    [Sub  AE AE]
    [Mul  AE AE]
    [Div  AE AE])

(define-type PLANG
    [Poly (Listof AE) (Listof AE)])




(: parse-sexpr : Sexpr -> AE)
  ;; to convert s-expressions into AEs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n) (Num n)]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PLANG)
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(list (list 'poly coeffs ...) (list points ...))
       (cond
         [(null? coeffs) (error 'parse "at least one coefficient is required in ~s" code)]
         [(null? points) (error 'parse "at least one point is required in ~s" code)]
         [else (Poly (map parse-sexpr coeffs) (map parse-sexpr points))]
                                                              )]
      [else
       (error 'parse "Invalid PLANG syntax")])))


;; parser tests
(test (parse "{{poly 1 2 3} {4 5 6}}")
     => (Poly (list (Num 1) (Num 2) (Num 3))
              (list (Num 4) (Num 5) (Num 6))))
(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 4/5 } {1/2 2/3 3}}")
      => (Poly (list (Num 4/5))
               (list (Num 1/2) (Num 2/3) (Num 3))))
(test (parse "{{poly 2 3} {4}}")
      => (Poly (list (Num 2) (Num 3))
               (list (Num 4))))
(test (parse "{{poly 1 1 0} {-1 3 3}}")
      => (Poly (list (Num 1) (Num 1) (Num 0))
               (list (Num -1) (Num 3) (Num 3))))
(test (parse "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}")
      => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1)))
               (list (Sub (Num 8) (Num 4)))))
(test (parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}")
      => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9)))
               (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9)))))
(test (parse "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "{{poly 2 3} {}}") 
      =error> 
      "parse: at least one point is required in ((poly 2 3) ())")
(test (parse "{{poly 1 1 3} }") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "{{1 1 3} {1 1 3}}") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "{{1 1 3}}") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "{1}") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "1") 
      =error> 
      "parse: Invalid PLANG syntax")
(test (parse "{}") 
      =error> 
      "parse: Invalid PLANG syntax")



;; III

;; evaluates AE expressions to numbers
(: eval : AE -> Number)
(define (eval expr)
  (cases expr
    [(Num n)  n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))


(: eval-poly : PLANG -> (Listof Number))
(define (eval-poly p-expr)
  (cases p-expr
    [(Poly coeffs points)
     (let* ([evaluated-coeffs (map eval coeffs)] ;; calculate coeffs for each value in list
           [evaluated-points (map eval points)] ;; calculate points for each value in list
           [polynome (createPolynomial evaluated-coeffs)]) ;; generate polynome function
       (map polynome evaluated-points)) ;; run polynome function for each point
     ]
    )) 


(: run : String -> (Listof Number))
            ;; evaluate a FLANG program contained in a string
            (define (run str)
              (eval-poly (parse str)))

;; run tests
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5))
(test (run "{{poly 1 2 3} {4}}") => '(57))
(test (run "{{poly 0 0 3} {2}}") => '(12))
(test (run "{{poly 1 -2 1} {-1 1/2}}") => '(4 1/4))
(test (run "{{poly {+ 1 1} {* 2 2} {- 5 2}} {3}}") => '(41))
(test (run "{{poly 3 2 1} {0}}") => '(3))
(test (run "{{poly 0 0 3} {1}}") => '(3))
(test (run "{{poly -1 -2 -3} {-1}}") => '(-2))
(test (run "{{poly 1000000 1000000} {1000}}") => '(1001000000))
(test (run "{{poly 1/2 1/3} {1/2}}") => '(2/3))
(test (run "{{poly 5} {10}}") => '(5))

(test (run "{{poly } {1 2 3}}") =error> "parse: at least one coefficient is required in")
(test (run "{{poly 1 2 3} {}}") =error> "parse: at least one point is required in")
(test (run "{{poly {+ 2}} {1}}") =error> "parse-sexpr: bad syntax in")
(test (run "{{poly {* 2 3} {/ 2}} {1}}") =error> "parse-sexpr: bad syntax in")
(test (run "{{poly 1 2 3} {1 2} extra}") =error> "parse: Invalid PLANG syntax")
(test (run "{{poly 1 two 3} {1}}") =error> "parse-sexpr: bad syntax in")
(test (run "{{poly 1 {+ 2 3}} {one}}") =error> "parse-sexpr: bad syntax in")


