;;;;COMPDATA.SCM
                   ;;; Lists as sets

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

;;; Instructions and instruction sequences

(define (make-instruction-sequence needed modified statements)
  (list needed modified statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                (list-difference (registers-needed seq2)
                                (registers-modified seq1)))
     (list-union (registers-modified seq1)
                (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
	(if (and (needs-register? seq2 first-reg)
		 (modifies-register? seq1 first-reg))
	    (preserving (cdr regs)
	     (make-instruction-sequence
	      (list-union (list first-reg) (registers-needed seq1))
	      (list-difference (registers-modified seq1)
			       (list first-reg))
	      (append `((save ,first-reg))
		      (statements seq1)
		      `((restore ,first-reg))))
	     seq2)
	    (preserving (cdr regs) seq1 seq2)))))


(define (tack-on-instruction-sequence seq body-seq)
  (append-instruction-sequences
   seq
   (make-instruction-sequence '() '() (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1) (registers-needed seq2))
   (list-union (registers-modified seq1) (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;;; arg1 arg2 ... registers reserved for passing arguments.

(define all-regs '(env proc val argl continue arg1 arg2))


(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)            ;implicit jump to next.
         (empty-instruction-sequence))
        (else
         ;; No needs or clobbers because 
         ;; code jumped to will be appended
         ;; in line -- not enforced.
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (make-label name)
  (generate-uninterned-symbol name))

