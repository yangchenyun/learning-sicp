;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file ps5-code.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))


;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))



;;; THE NUMBER PACKAGE

;;; Methods for dealing with ordinary numbers.
;;;  RepNum = Sch-Num


;;;   (RepNum, RepNum) --> ({number} X RepNum)
(define (+number x y) (make-number (+ x y)))
(define (-number x y) (make-number (- x y)))
(define (*number x y) (make-number (* x y)))
(define (/number x y) (make-number (/ x y)))

;;;   RepNum --> ({number} X RepNum)
(define (negate-number x) (make-number (- x)))

;;;   RepNum --> Bool
(define (=zero-number? x) (= x 0))

;;;   RepNum --> ({number} X RepNum)
(define (make-number x) (attach-tag 'number x))


;;; Install the number methods in the generic operations.

(put 'add '(number number) +number)
(put 'sub '(number number) -number)
(put 'mul '(number number) *number)
(put 'div '(number number) /number)

(put 'negate '(number) negate-number)

(put '=zero? '(number) =zero-number?)


;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) (attach-tag 'number x))

(define n2 (create-number 2))

;;; THE GENERIC RATIONAL PACKAGE

;;; Methods for rationals.

;;;   (RepRat, RepRat) --> ({rational} X RepRat)
(define (+rational x y) (make-rational (+rat x y)))
(define (-rational x y) (make-rational (-rat x y)))
(define (*rational x y) (make-rational (*rat x y)))
(define (/rational x y) (make-rational (/rat x y)))

;;;   RepRat --> ({rational} X RepRat)
(define (negate-rational x) (make-rational (negate-rat x)))

;;;   RepRat --> Bool
(define (=zero-rational? x) (=zero-rat? x))

;;;   RepRat --> ({rational} X RepRat)
(define (make-rational x) (attach-tag 'rational x))


;;; Install the rational methods in the generic operations.

(put 'add '(rational rational) +rational)
(put 'sub '(rational rational) -rational)
(put 'mul '(rational rational) *rational)
(put 'div '(rational rational) /rational)

(put 'negate '(rational) negate-rational)
(put '=zero? '(rational) =zero-rational?)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define (create-rational x y)
   (make-rational (make-rat x y)))




;;; THE RATIONAL ARITHMETIC PACKAGE.

;;;   (RepRat, RepRat) --> RepRat

(define (+rat x y)
  (make-rat (add (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (-rat x y)
  (make-rat (sub (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (*rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))

(define (/rat x y)
  (make-rat (mul (numer x) (denom y))
            (mul (denom x) (numer y))))


;;;   RepRat --> RepRat

(define (negate-rat x)
  (make-rat (negate (numer x))
            (denom x)))

;;;   RepRat --> Bool

(define (=zero-rat? x)
  (=zero? (numer x)))


;;; Procedures for representing rationals

;;;   (GN, GN) --> RepRat
(define (make-rat numerator denominator)
  (cons numerator denominator))

;;;   RepRat --> GN
(define numer car)
(define denom cdr)

;;; Coercion procedure from rational/rational method
;;; to number/rational method

;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
(define (RRmethod->NRmethod method)
  (lambda (num rat)
    (method
     (repnum->reprat num)
     rat)))


;;; THE GENERIC POLYNOMIAL PACKAGE
;;; Methods for polynomials

;;;   (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (+polynomial p1 p2)
  (make-polynomial (+poly p1 p2)))


(define (*polynomial p1 p2)
  (make-polynomial (*poly p1 p2)))

;;;   RepPoly --> ({polynomial} X RepPoly)
(define (negate-polynomial x)
  (make-polynomial (negate-poly x)))

;;;   RepPoly --> Bool
(define (=zero-polynomial? p) (=zero-poly? p))


;;;   RepPoly --> ({polynomial} X RepPoly)
(define (make-polynomial poly)
  (attach-tag 'polynomial poly))


;;; Install the polynomial methods in the generic operations.

(put 'add '(polynomial polynomial) +polynomial)
(put 'mul '(polynomial polynomial) *polynomial)

(put '=zero? '(polynomial) =zero-polynomial?)

;;; Polynomial Package User Interface

;;; A convenient procedure for building a generic polynomial
;;; from a list of generic numbers, representing the 
;;; coefficients of the polynomial in dense form.

;;;    (Variable, List(GN)) --> ({polynomial} X RepPoly)
(define (create-polynomial var coeffs)
  (make-polynomial (make-poly var (dense/coeffs->sparse/terms coeffs))))


;;; Makes sparse term representation out of a dense coefficient list
;;;     List(GN) --> Repterms
(define (dense/coeffs->sparse/terms coeffs)
  (define (dt->st rev-coeffs terms degree)
    (if (null? rev-coeffs)
        terms
	(let ((coeff (car rev-coeffs))
	      (rev-coeffs (cdr rev-coeffs)))
	  (if (=zero? coeff)
	      (dt->st rev-coeffs terms (inc degree))
              (dt->st rev-coeffs
                      (adjoin-term (make-term degree coeff)
                                   terms)
                      (inc degree))))))
  (dt->st (reverse coeffs) (the-empty-termlist) 0))


;;; THE POLYNOMIAL ARITHMETIC PACKAGE.
;;;    RepPoly = Variable X RepTerms

;;;   (RepPoly, RepPoly) --> RepPoly
(define (+poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (+terms (term-list p1)
			 (term-list p2)))
      (error "Polys not in same var -- +POLY"
	     (list p1 p2))))

;;; Need -poly ****

(define (*poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (*terms (term-list p1)
			 (term-list p2)))
      (error "Polys not in same var -- *POLY"
	     (list p1 p2))))

;;;   RepPoly --> RepPoly
;;; Need negate-poly ****


;;;   RepPoly --> Bool
(define (=zero-poly? p)
  (empty-termlist? (term-list p)))

;;;   (Variable, RepTerms) --> RepPoly
(define (make-poly variable term-list)
  (cons variable term-list))

;;;   RepPoly --> Variable
(define (variable p) (car p))

;;;   RepPoly --> RepTerms
(define (term-list p) (cdr p))

;;;   (Variable, Variable) --> Bool
(define (same-variable? v1 v2) (eq? v1 v2))

;;; THE TERM LIST ARITHMETIC PACKAGE.

;;; procedures for dealing with lists of terms in order of 
;;; descending powers.

;;;   (RepTerms, RepTerms) --> RepTerms

(define (+terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (+terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (+terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term (make-term (order t1)
                                          (add (coeff t1)
                                               (coeff t2)))
                               (+terms (rest-terms L1)
                                       (rest-terms L2)))))))))

(define (*terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (+terms (*-term-by-all-terms (first-term L1) L2)
              (*terms (rest-terms L1) L2))))


;;;   (RepTerm, RepTerms) --> RepTerms
(define (*-term-by-all-terms t1 L)
  (map-terms (lambda (term) (*term t1 term)) L))



;;; Procedures for Representing Term Lists.

;;; RepTerms =  Empty-Term-List  U  (RepTerm X RepTerms)

;;;   constructor of type (RepTerm, RepTerms) --> RepTerms

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))                ;slight simplification
      term-list
      (cons term term-list)))


;;;   constructor of type  EmptyType --> RepTerms
(define (the-empty-termlist) '())

;;; selectors

;;;   RepTerms --> RepTerm
(define (first-term term-list) (car term-list))

;;;   RepTerms --> RepTerms
(define (rest-terms term-list) (cdr term-list))

;;;   RepTerms --> Bool
(define (empty-termlist? term-list) (null? term-list))


;;;  THE TERM ARITHMETIC PACKAGE

;;;   RepTerm = Sch-NatNum X GN

;;;   (Sch-NatNum, GN) --> RepTerm
(define (make-term order coeff) (list order coeff))

;;;   RepTerm --> Sch-NatNum
(define (order term) (car term))

;;;   RepTerm --> GN
(define (coeff term) (cadr term))

                   ;;;   (RepTerm, RepTerm) --> RepTerm
(define (*term t1 t2)
  (make-term
   (+ (order t1) (order t2))
   (mul (coeff t1) (coeff t2))))


;;;APPLYING POLYNOMIALS

(define (apply-term t gn)
  (mul (coeff t)
       (power gn (order t))))

(define (power gn k)
  (if (< k 1)
      (create-number 1)
      (mul gn (power gn (dec k)))))

(define (apply-polynomial p gn)
  (apply-terms
   (term-list (contents p))
   gn))














