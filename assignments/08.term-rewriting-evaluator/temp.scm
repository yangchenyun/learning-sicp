This is the Scheme process buffer.
Type C-x C-e to evaluate the expression before point.
Type C-c C-c to abort evaluation.
Type C-h m for more information.

Scheme Microcode Version 11.148
MIT Scheme running under HP-UX

Scheme saved on Thursday February 9, 1995 at 8:57:48 PM
  Release 7.4.0 (alpha)
  Microcode 11.148
  Runtime 14.166

(load "ps4go")

;Loading "ps4go.scm"
;Loading "/zu/u6001/psets/ps4/smfresh.scm"
;Loading "hashtb.com" -- done -- done
;Loading "/zu/u6001/psets/ps4/smsyntax.scm" -- done
;Loading "/zu/u6001/psets/ps4/smscope.scm" -- done
;Loading "/zu/u6001/psets/ps4/smstep.scm" -- done
;Loading "/zu/u6001/psets/ps4/smeval.scm" -- done -- done
;Value: printable-version

(define (save-this-step? number body) #t)
;Value: save-this-step?

(smeval '((define (rec-fact n)
	    (if (<= n 0)
		1
		(* n (rec-fact (dec n)))))
	  (rec-fact 4)))

;==(0)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (rec-fact 4))

;==(1)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) ((lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n))))) 4))

;==(2)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (if (<= n#0 0) 1 (* n#0 (rec-fact (dec n#0)))))

;==(3)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (if (<= 4 0) 1 (* n#0 (rec-fact (dec n#0)))))

;==(4)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (if () 1 (* n#0 (rec-fact (dec n#0)))))

;==(5)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (* n#0 (rec-fact (dec n#0))))

;==(6)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (* 4 (rec-fact (dec n#0))))

;==(7)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (* 4 (rec-fact (dec 4))))

;==(8)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (* 4 (rec-fact 3)))

;==(9)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (* 4 ((lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n))))) 3)))

;==(10)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (* 4 (if (<= n#1 0) 1 (* n#1 (rec-fact (dec n#1))))))

;==(11)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (* 4 (if (<= 3 0) 1 (* n#1 (rec-fact (dec n#1))))))

;==(12)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (* 4 (if () 1 (* n#1 (rec-fact (dec n#1))))))

;==(13)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (* 4 (* n#1 (rec-fact (dec n#1)))))

;==(14)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (* 4 (* 3 (rec-fact (dec n#1)))))

;==(15)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (define n#1 3) (* 4 (* 3 (rec-fact (dec 3)))))

;==(16)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4) (define n#1 3) (* 4 (* 3 (rec-fact 2))))

;==(17)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (* 4 (* 3 ((lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n))))) 2))))

;==(18)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (if (<= n#2 0) 1 (* n#2 (rec-fact (dec n#2)))))))

;==(19)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (if (<= 2 0) 1 (* n#2 (rec-fact (dec n#2)))))))

;==(20)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (if () 1 (* n#2 (rec-fact (dec n#2)))))))

;==(21)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (* n#2 (rec-fact (dec n#2))))))

;==(22)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (* 2 (rec-fact (dec n#2))))))

;==(23)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (* 2 (rec-fact (dec 2))))))

;==(24)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (* 4 (* 3 (* 2 (rec-fact 1)))))

;==(25)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (* 4 (* 3 (* 2 ((lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n))))) 1)))))

;==(26)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (* 4 (* 3 (* 2 (if (<= n#3 0) 1 (* n#3 (rec-fact (dec n#3))))))))

;==(27)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (* 4 (* 3 (* 2 (if (<= 1 0) 1 (* n#3 (rec-fact (dec n#3))))))))

;==(28)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (* 4 (* 3 (* 2 (if () 1 (* n#3 (rec-fact (dec n#3))))))))

;==(29)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (* 4 (* 3 (* 2 (* n#3 (rec-fact (dec n#3)))))))

;==(30)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (* 4 (* 3 (* 2 (* 1 (rec-fact (dec n#3)))))))

;==(31)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (* 4 (* 3 (* 2 (* 1 (rec-fact (dec 1)))))))

;==(32)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (* 4 (* 3 (* 2 (* 1 (rec-fact 0))))))

;==(33)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (* 4 (* 3 (* 2 (* 1 ((lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n))))) 0))))))

;==(34)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (define n#4
   0)
 (* 4 (* 3 (* 2 (* 1 (if (<= n#4 0) 1 (* n#4 (rec-fact (dec n#4)))))))))

;==(35)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (define n#4
   0)
 (* 4 (* 3 (* 2 (* 1 (if (<= 0 0) 1 (* n#4 (rec-fact (dec n#4)))))))))

;==(36)==>
((define rec-fact
   (lambda (n)
     (if (<= n 0)
         1
         (* n (rec-fact (dec n))))))
 (define n#0
   4)
 (define n#1
   3)
 (define n#2
   2)
 (define n#3
   1)
 (define n#4
   0)
 (* 4 (* 3 (* 2 (* 1 (if #t 1 (* n#4 (rec-fact (dec n#4)))))))))

;==(37)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (define n#4 0)
                                                                         (* 4 (* 3 (* 2 (* 1 1)))))

;==(38)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (define n#4 0)
                                                                         (* 4 (* 3 (* 2 1))))

;==(39)==>
((define rec-fact (lambda (n) (if (<= n 0) 1 (* n (rec-fact (dec n)))))) (define n#0 4)
                                                                         (define n#1 3)
                                                                         (define n#2 2)
                                                                         (define n#3 1)
                                                                         (define n#4 0)
                                                                         (* 4 (* 3 2)))

;==(40)==>
(* 4 6)

;==(41)==>
24

;==(42)==>
24
Syntactic Value was returned
;Value: 24

(smeval '((define (iter-fact n)
	    (define (iter n result)
	      (if (<= n 0)
		  result
		  (iter (- n 1) (* n result))))
	    (iter n 1))
	  (iter-fact 4)))

;==(0)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (iter-fact 4))

;==(1)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 ((lambda (n) (define iter (lambda (n result) (if (<= n 0) result (iter (- n 1) (* n result))))) (iter n 1)) 4))

;==(2)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (iter#1 n#5 1))

;==(3)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (iter#1 4 1))

;==(4)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 ((lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result)))) 4 1))

;==(5)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (if (<= n#6 0)
     result#0
     (iter#1 (- n#6 1) (* n#6 result#0))))

;==(6)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (if (<= 4 0)
     result#0
     (iter#1 (- n#6 1) (* n#6 result#0))))

;==(7)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (if ()
     result#0
     (iter#1 (- n#6 1) (* n#6 result#0))))

;==(8)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 (- n#6 1) (* n#6 result#0)))

;==(9)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 (- 4 1) (* n#6 result#0)))

;==(10)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 3 (* n#6 result#0)))

;==(11)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 3 (* 4 result#0)))

;==(12)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 3 (* 4 1)))

;==(13)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (iter#1 3 4))

;==(14)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 ((lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result)))) 3 4))

;==(15)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (if (<= n#7 0)
     result#1
     (iter#1 (- n#7 1) (* n#7 result#1))))

;==(16)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (if (<= 3 0)
     result#1
     (iter#1 (- n#7 1) (* n#7 result#1))))

;==(17)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (if ()
     result#1
     (iter#1 (- n#7 1) (* n#7 result#1))))

;==(18)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 (- n#7 1) (* n#7 result#1)))

;==(19)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 (- 3 1) (* n#7 result#1)))

;==(20)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 2 (* n#7 result#1)))

;==(21)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 2 (* 3 result#1)))

;==(22)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 2 (* 3 4)))

;==(23)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (iter#1 2 12))

;==(24)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 ((lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result)))) 2 12))

;==(25)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (if (<= n#8 0)
     result#2
     (iter#1 (- n#8 1) (* n#8 result#2))))

;==(26)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (if (<= 2 0)
     result#2
     (iter#1 (- n#8 1) (* n#8 result#2))))

;==(27)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (if ()
     result#2
     (iter#1 (- n#8 1) (* n#8 result#2))))

;==(28)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 (- n#8 1) (* n#8 result#2)))

;==(29)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 (- 2 1) (* n#8 result#2)))

;==(30)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 1 (* n#8 result#2)))

;==(31)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 1 (* 2 result#2)))

;==(32)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 1 (* 2 12)))

;==(33)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (iter#1 1 24))

;==(34)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 ((lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result)))) 1 24))

;==(35)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (define n#9
   1)
 (define result#3
   24)
 (if (<= n#9 0)
     result#3
     (iter#1 (- n#9 1) (* n#9 result#3))))

;==(36)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (define n#9
   1)
 (define result#3
   24)
 (if (<= 1 0)
     result#3
     (iter#1 (- n#9 1) (* n#9 result#3))))

;==(37)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (define n#9
   1)
 (define result#3
   24)
 (if ()
     result#3
     (iter#1 (- n#9 1) (* n#9 result#3))))

;==(38)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (define n#9
   1)
 (define result#3
   24)
 (iter#1 (- n#9 1) (* n#9 result#3)))

;==(39)==>
((define iter-fact
   (lambda (n)
     (define iter
       (lambda (n result)
         (if (<= n 0)
             result
             (iter (- n 1) (* n result)))))
     (iter n 1)))
 (define n#5
   4)
 (define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#6
   4)
 (define result#0
   1)
 (define n#7
   3)
 (define result#1
   4)
 (define n#8
   2)
 (define result#2
   12)
 (define n#9
   1)
 (define result#3
   24)
 (iter#1 (- 1 1) (* n#9 result#3)))

;==(40)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (iter#1 0 (* n#9 result#3)))

;==(41)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (iter#1 0 (* 1 result#3)))

;==(42)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (iter#1 0 (* 1 24)))

;==(43)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (iter#1 0 24))

;==(44)==>
((define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#9
   1)
 (define result#3
   24)
 ((lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result)))) 0 24))

;==(45)==>
((define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#9
   1)
 (define result#3
   24)
 (define n#10
   0)
 (define result#4
   24)
 (if (<= n#10 0)
     result#4
     (iter#1 (- n#10 1) (* n#10 result#4))))

;==(46)==>
((define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#9
   1)
 (define result#3
   24)
 (define n#10
   0)
 (define result#4
   24)
 (if (<= 0 0)
     result#4
     (iter#1 (- n#10 1) (* n#10 result#4))))

;==(47)==>
((define iter#1
   (lambda (n result)
     (if (<= n 0)
         result
         (iter#1 (- n 1) (* n result)))))
 (define n#9
   1)
 (define result#3
   24)
 (define n#10
   0)
 (define result#4
   24)
 (if #t
     result#4
     (iter#1 (- n#10 1) (* n#10 result#4))))

;==(48)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (define n#10 0)
                                                                                        (define result#4 24)
                                                                                        result#4)

;==(49)==>
((define iter#1 (lambda (n result) (if (<= n 0) result (iter#1 (- n 1) (* n result))))) (define n#9 1)
                                                                                        (define result#3 24)
                                                                                        (define n#10 0)
                                                                                        (define result#4 24)
                                                                                        24)

;==(50)==>
24
Syntactic Value was returned
;Value: 24


(define (save-this-step? number body)
  (simple? (expression-of-body body)))
;Value: save-this-step?

(define (simple? exp)
  (or (number? exp)
      (boolean? exp)
      (variable? exp)
      (and (combination? exp)
	   (simple? (operator exp))
	   (every (operands exp)
		  simple?))))
;Value: simple?

(define (every things pred)
  (or (null? things)
      (and (pred (car things))
	   (every (cdr things) pred))))
;Value: every

(define (operator x) (operator-of x))
;Value: operator

(define (operands x) (operands-of x))
;Value: operands

(smeval '((define (fact n)
	    (if (<= n 0)
		1
		(* n (fact (- n 1)))))
	  (fact 4)))

;==(0)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (fact 4))

;==(5)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (* n#16 (fact (- n#16 1))))

;==(6)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (* 4 (fact (- n#16 1))))

;==(7)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (* 4 (fact (- 4 1))))

;==(8)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (* 4 (fact 3)))

;==(13)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (define n#17 3) (* 4 (* n#17 (fact (- n#17 1)))))

;==(14)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (define n#17 3) (* 4 (* 3 (fact (- n#17 1)))))

;==(15)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (define n#17 3) (* 4 (* 3 (fact (- 3 1)))))

;==(16)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4) (define n#17 3) (* 4 (* 3 (fact 2))))

;==(21)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (* 4 (* 3 (* n#18 (fact (- n#18 1))))))

;==(22)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (* 4 (* 3 (* 2 (fact (- n#18 1))))))

;==(23)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (* 4 (* 3 (* 2 (fact (- 2 1))))))

;==(24)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (* 4 (* 3 (* 2 (fact 1)))))

;==(29)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (* 4 (* 3 (* 2 (* n#19 (fact (- n#19 1)))))))

;==(30)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (* 4 (* 3 (* 2 (* 1 (fact (- n#19 1)))))))

;==(31)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (* 4 (* 3 (* 2 (* 1 (fact (- 1 1)))))))

;==(32)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (* 4 (* 3 (* 2 (* 1 (fact 0))))))

;==(37)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (define n#20 0)
                                                                 (* 4 (* 3 (* 2 (* 1 1)))))

;==(38)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (define n#20 0)
                                                                 (* 4 (* 3 (* 2 1))))

;==(39)==>
((define fact (lambda (n) (if (<= n 0) 1 (* n (fact (- n 1)))))) (define n#16 4)
                                                                 (define n#17 3)
                                                                 (define n#18 2)
                                                                 (define n#19 1)
                                                                 (define n#20 0)
                                                                 (* 4 (* 3 2)))

;==(40)==>
(* 4 6)

;==(41)==>
24

;==(42)==>
24
Syntactic Value was returned
;Value: 24

(define (print-stepped-message step-number body)
  (begin (newline)
         (pp (expression-of-body body))))
;Value: print-stepped-message

(smeval '((define (fact n)
	    (if (<= n 0)
		1
		(* n (fact (- n 1)))))
	  (fact 4)))

(fact 4)

(* n#21 (fact (- n#21 1)))

(* 4 (fact (- n#21 1)))

(* 4 (fact (- 4 1)))

(* 4 (fact 3))

(* 4 (* n#22 (fact (- n#22 1))))

(* 4 (* 3 (fact (- n#22 1))))

(* 4 (* 3 (fact (- 3 1))))

(* 4 (* 3 (fact 2)))

(* 4 (* 3 (* n#23 (fact (- n#23 1)))))

(* 4 (* 3 (* 2 (fact (- n#23 1)))))

(* 4 (* 3 (* 2 (fact (- 2 1)))))

(* 4 (* 3 (* 2 (fact 1))))

(* 4 (* 3 (* 2 (* n#24 (fact (- n#24 1))))))

(* 4 (* 3 (* 2 (* 1 (fact (- n#24 1))))))

(* 4 (* 3 (* 2 (* 1 (fact (- 1 1))))))

(* 4 (* 3 (* 2 (* 1 (fact 0)))))

(* 4 (* 3 (* 2 (* 1 1))))

(* 4 (* 3 (* 2 1)))

(* 4 (* 3 2))

(* 4 6)

24

24
Syntactic Value was returned
;Value: 24

(smeval '((define (iter-fact n)
	    (define (iter n result)
	      (if (<= n 0)
		  result
		  (iter (- n 1) (* n result))))
	    (iter n 1))
	  (iter-fact 4)))

(iter-fact 4)

(iter#3 n#26 1)

(iter#3 4 1)

(iter#3 (- n#27 1) (* n#27 result#5))

(iter#3 (- 4 1) (* n#27 result#5))

(iter#3 3 (* n#27 result#5))

(iter#3 3 (* 4 result#5))

(iter#3 3 (* 4 1))

(iter#3 3 4)

(iter#3 (- n#28 1) (* n#28 result#6))

(iter#3 (- 3 1) (* n#28 result#6))

(iter#3 2 (* n#28 result#6))

(iter#3 2 (* 3 result#6))

(iter#3 2 (* 3 4))

(iter#3 2 12)

(iter#3 (- n#29 1) (* n#29 result#7))

(iter#3 (- 2 1) (* n#29 result#7))

(iter#3 1 (* n#29 result#7))

(iter#3 1 (* 2 result#7))

(iter#3 1 (* 2 12))

(iter#3 1 24)

(iter#3 (- n#30 1) (* n#30 result#8))

(iter#3 (- 1 1) (* n#30 result#8))

(iter#3 0 (* n#30 result#8))

(iter#3 0 (* 1 result#8))

(iter#3 0 (* 1 24))

(iter#3 0 24)

result#9

24

24
Syntactic Value was returned
;Value: 24

(smeval '((define (rec-fib n)
	    (if (< n 3)
		1
		(+ (rec-fib (- n 1)) (rec-fib (- n 2)))))
	  (rec-fib 4)))


(rec-fib 4)

(+ (rec-fib (- n#33 1)) (rec-fib (- n#33 2)))

(+ (rec-fib (- 4 1)) (rec-fib (- n#33 2)))

(+ (rec-fib 3) (rec-fib (- n#33 2)))

(+ (+ (rec-fib (- n#34 1)) (rec-fib (- n#34 2))) (rec-fib (- n#33 2)))

(+ (+ (rec-fib (- 3 1)) (rec-fib (- n#34 2))) (rec-fib (- n#33 2)))

(+ (+ (rec-fib 2) (rec-fib (- n#34 2))) (rec-fib (- n#33 2)))

(+ (+ 1 (rec-fib (- n#34 2))) (rec-fib (- n#33 2)))

(+ (+ 1 (rec-fib (- 3 2))) (rec-fib (- n#33 2)))

(+ (+ 1 (rec-fib 1)) (rec-fib (- n#33 2)))

(+ (+ 1 1) (rec-fib (- n#33 2)))

(+ 2 (rec-fib (- n#33 2)))

(+ 2 (rec-fib (- 4 2)))

(+ 2 (rec-fib 2))

(+ 2 1)

3

3
Syntactic Value was returned
;Value: 3

(smeval '((define (iter-fib n)
	    (define (loop i fibi fib<i-1>)
	      (if (>= i n)
		  fibi
		  (loop (+ i 1) (+ fibi fib<i-1>) fibi)))
	    (loop 2 1 1))
	  (iter-fib 4)))

(iter-fib 4)

(loop#1 2 1 1)

(loop#1 (+ i#0 1) (+ fibi#0 fib<i-1>#0) fibi#0)

(loop#1 (+ 2 1) (+ fibi#0 fib<i-1>#0) fibi#0)

(loop#1 3 (+ fibi#0 fib<i-1>#0) fibi#0)

(loop#1 3 (+ 1 fib<i-1>#0) fibi#0)

(loop#1 3 (+ 1 1) fibi#0)

(loop#1 3 2 fibi#0)

(loop#1 3 2 1)

(loop#1 (+ i#1 1) (+ fibi#1 fib<i-1>#1) fibi#1)

(loop#1 (+ 3 1) (+ fibi#1 fib<i-1>#1) fibi#1)

(loop#1 4 (+ fibi#1 fib<i-1>#1) fibi#1)

(loop#1 4 (+ 2 fib<i-1>#1) fibi#1)

(loop#1 4 (+ 2 1) fibi#1)

(loop#1 4 3 fibi#1)

(loop#1 4 3 2)

fibi#2

3

3
Syntactic Value was returned
;Value: 3

(define (simple? exp)
  (or (number? exp)
      (boolean? exp)
      (variable? exp)
      (and (combination? exp)
	   (simple? (operator exp))
	   (every (operands exp)
		  (lambda (arg)
		    (and (not (variable? arg))
			 (simple? arg)))))))
;Value: simple?

(smeval '((define (fact n)
	    (if (<= n 0)
		1
		(* n (fact (- n 1)))))
	  (fact 4)))

(fact 4)

(* 4 (fact (- 4 1)))

(* 4 (fact 3))

(* 4 (* 3 (fact (- 3 1))))

(* 4 (* 3 (fact 2)))

(* 4 (* 3 (* 2 (fact (- 2 1)))))

(* 4 (* 3 (* 2 (fact 1))))

(* 4 (* 3 (* 2 (* 1 (fact (- 1 1))))))

(* 4 (* 3 (* 2 (* 1 (fact 0)))))

(* 4 (* 3 (* 2 (* 1 1))))

(* 4 (* 3 (* 2 1)))

(* 4 (* 3 2))

(* 4 6)

24

24
Syntactic Value was returned
;Value: 24

(smeval '((define (iter-fact n)
	    (define (iter n result)
	      (if (<= n 0)
		  result
		  (iter (- n 1) (* n result))))
	    (iter n 1))
	  (iter-fact 4)))

(iter-fact 4)

(iter#5 4 1)

(iter#5 3 (* 4 1))

(iter#5 3 4)

(iter#5 2 (* 3 4))

(iter#5 2 12)

(iter#5 1 (* 2 12))

(iter#5 1 24)

(iter#5 0 (* 1 24))

(iter#5 0 24)

result#14

24

24
Syntactic Value was returned
;Value: 24

(smeval '((define (rec-fib n)
	    (if (< n 3)
		1
		(+ (rec-fib (- n 1)) (rec-fib (- n 2)))))
	  (rec-fib 6)))

(rec-fib 6)

(+ 5 (rec-fib (- 6 2)))

(+ 5 (rec-fib 4))

(+ 5 (+ 2 (rec-fib (- 4 2))))

(+ 5 (+ 2 (rec-fib 2)))

(+ 5 (+ 2 1))

(+ 5 3)

8

8
Syntactic Value was returned
;Value: 8


(smeval '((define (iter-fib n)
	    (define (loop i fibi fib<i-1>)
	      (if (>= i n)
		  fibi
		  (loop (+ i 1) (+ fibi fib<i-1>) fibi)))
	    (loop 2 1 1))
	  (iter-fib 10)))

(iter-fib 10)

(loop#5 2 1 1)

(loop#5 3 2 1)

(loop#5 4 3 2)

(loop#5 5 5 3)

(loop#5 6 8 5)

(loop#5 7 13 8)

(loop#5 8 21 13)

(loop#5 9 34 21)

(loop#5 10 55 34)

fibi#14

55

55
Syntactic Value was returned
;Value: 55


(define let*? (tagged-pair? 'let*))
;Value: let*?



(define (make-let* bindings body)
  (list 'let* bindings body))
;Value: make-let*



(define (bindings-of-let* body)
  (cadr body))
;Value: bindings-of-let*



(define (body-of-let* body)
  (caddr body))
;Value: body-of-let*



(define (desugar body)
  (cond ((cond? body)
         (let ((clauses (clauses-of-cond body)))
           (if (null? clauses)
               submodel-useless-value
               (let ((first-clause (car clauses)))
                 (desugar
                  (if (else-clause? first-clause)
                      (expression-of-clause first-clause)
                      (make-if
                       (test-of-clause first-clause)
                       (expression-of-clause first-clause)
                       (make-cond (cdr clauses)))))))))
        ((let? body)
         (desugar
          (let ((bindings (bindings-of-let body)))
            (make-combination
             (make-lambda
              (map variable-of-binding bindings)
	      (body-of-let body))
             (map init-of-binding bindings)))))
	((let*? body)
	 (desugar
	  (let ((bindings (bindings-of-let* body)))
	    (if (null? bindings)
		(body-of-let* body)
		(make-combination 
		 (make-lambda (list (variable-of-binding (car bindings)))
			      (make-let* (cdr bindings) (body-of-let* body)))
		 (list (init-of-binding (car bindings))))))))
        ((or (self-evaluating? body)
             (symbol-expression? body)
             (variable? body)
             (submodel-null? body))
         body)
        ((lambda-expression? body)
         (make-lambda
          (formals-of-lambda body)
          (desugar (body-of-lambda body))))
        ((combination? body)
         (make-combination
          (desugar (operator-of body))
          (map desugar (operands-of body))))
        ((if? body)
         (make-if
          (desugar (test-of-if body))
          (desugar (consequent-of body))
          (desugar (alternative-of body))))
        (else
         (let ((defs (defines-of-body body)))
           (make-body
            (map make-define
                 (map variable-of-define defs)
                 (map desugar (map expression-of-define defs)))
            (desugar (expression-of-body body)))))))
;Value: desugar

(define let*-test1
  '((define (put-in-standard-position curve)
     (let* ((start-point (curve 0))
            (curve-started-at-origin
             ((translate (- (x-of start-point))
                         (- (y-of start-point)))
              curve))
            (new-end-point (curve-started-at-origin 1))
            (theta (atan (y-of new-end-point) (x-of new-end-point)))
            (curve-ended-at-x-axis
             ((rotate-around-origin (- theta)) curve-started-at-origin))
            (end-point-on-x-axis (x-of (curve-ended-at-x-axis 1))))
       ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis)))
    (put-in-standard-position (compose unit-circle double))))
;Value: let*-test1

(desugar '(let* ((x 1) (y x)) y))
;Value 16: ((lambda (x) ((lambda (y) y) x)) 1)

(smeval '(let* ((x 1) (y x)) y))

y#7

1

1
Syntactic Value was returned
;Value: 1

(pp (desugar let*-test1))
((define put-in-standard-position
   (lambda (curve)
     ((lambda (start-point)
        ((lambda (curve-started-at-origin)
           ((lambda (new-end-point)
              ((lambda (theta)
                 ((lambda (curve-ended-at-x-axis)
                    ((lambda (end-point-on-x-axis)
                       ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis))
                     (x-of (curve-ended-at-x-axis 1))))
                  ((rotate-around-origin (- theta)) curve-started-at-origin)))
               (atan (y-of new-end-point) (x-of new-end-point))))
            (curve-started-at-origin 1)))
         ((translate (- (x-of start-point)) (- (y-of start-point))) curve)))
      (curve 0))))
 (put-in-standard-position (compose unit-circle double)))
;No value

(define let*-test2
  '(let* ((a (+ 2 3))
          (b (inc a))
          (b ((lambda ()
                (define (y) (let* ((a (inc a))
                                   (b (inc a)))
                              (cons b y)))
                y))))
     (not (let* ((c (cdr (b))))
            (equal? c b)))))
;Value: let*-test2

(pp (desugar let*-test2))
((lambda (a)
   ((lambda (b)
      ((lambda (b)
         (not ((lambda (c) (equal? c b)) (cdr (b)))))
       ((lambda () (define y (lambda () ((lambda (a) ((lambda (b) (cons b y)) (inc a))) (inc a)))) y))))
    (inc a)))
 (+ 2 3))
;No value

