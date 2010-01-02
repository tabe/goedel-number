#!r6rs

(import (rnrs (6))
        (xunit)
        (goedel-number))

(define-syntax assert-encode-and-decode
  (syntax-rules ()
    ((_ ls)
     (let ((t (length ls)))
       (call-with-values
           (lambda () (encode-numbers ls t))
         (lambda (u v)
           (assert-equal? ls (decode-numbers u v t))))))))

(assert-encode-and-decode '(0 0 0))
(assert-encode-and-decode '(0 0 1))
(assert-encode-and-decode '(0 1 0))
(assert-encode-and-decode '(1 0 0))
(assert-encode-and-decode '(0 1 1))
(assert-encode-and-decode '(1 1 0))
(assert-encode-and-decode '(1 0 1))
(assert-encode-and-decode '(1 1 1))
(assert-encode-and-decode '(1 2 3))
(assert-encode-and-decode '(1 2 3 4))
(assert-encode-and-decode '(1 2 3 4 5))
(assert-encode-and-decode '(10 20 30 40 50))
(assert-encode-and-decode '(230 0 9 2354 121 9640222 1 77))
(let ((v (make-vector 1000 7)))
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (vector-set! v i (* i i)))
  (assert-encode-and-decode (vector->list v)))

(report)
