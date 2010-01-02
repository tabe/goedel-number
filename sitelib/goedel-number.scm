;;
;;   Copyright (c) 2010 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Based on:
;; Chang, J. C., Tsai, S. C., and Chen, R. J. (2002)
;; "A Space-efficient Goedel Numbering with Chinese Remainder Theorem,"
;; Proceedings of the 19th Workshop on Combinatorial Mathematics and Computation Theory, March, pp. 192-195.

(library (goedel-number)
  (export encode-numbers
          decode-numbers)
  (import (rnrs (6)))

  (define (non-negative-integer? x)
    (and (integer? x)
         (not (negative? x))))

  (define (positive-integer? x)
    (and (integer? x)
         (positive? x)))

  (define (extended-euclidean/ a b cont)
    (let lp ((a a)
             (b b)
             (s1 0)
             (t1 1)
             (s2 1)
             (t2 0))
      (call-with-values
          (lambda () (div-and-mod a b))
        (lambda (q r)
          (if (zero? r)
              (cont b s1 t1)
              (lp b r (- s2 (* q s1)) (- t2 (* q t1)) s1 t1))))))

  (define (extended-euclidean a b)
    (assert (positive-integer? a))
    (assert (positive-integer? b))
    (if (< a b)
        (extended-euclidean/ b a (lambda (x y z) (values x z y)))
        (extended-euclidean/ a b values)))

  (define (crt xs ys)
    (assert (list? xs))
    (assert (for-all non-negative-integer? xs))
    (assert (list? ys))
    (assert (for-all non-negative-integer? ys))
    (assert (= (length xs) (length ys)))
    (let* ((M (apply * ys))
           (ms (map (lambda (y) (/ M y)) ys))
           (hs (map (lambda (m y)
                      (call-with-values
                          (lambda () (extended-euclidean m y))
                        (lambda (_ s t) s)))
                    ms
                    ys)))
      (mod (fold-left (lambda (r x m h) (+ r (* x m h))) 0 xs ms hs) M)))

  (define (factorial n)
    (assert (non-negative-integer? n))
    (let lp ((n n)
             (r 1))
      (if (<= n 1)
          r
          (lp (- n 1) (* r n)))))

  ;; same as (iota t 1) of SRFI 1
  (define (iota-of-base-1 t)
    (let ((v (make-vector t)))
      (do ((i 0 (+ i 1)))
          ((= i t)
           (vector->list v))
        (vector-set! v i (+ i 1)))))

  (define-syntax assert-input-for-encoding
    (syntax-rules ()
      ((_ p t)
       (begin
         (assert (list? p))
         (assert (for-all non-negative-integer? p))
         (assert (integer? t))
         (assert (<= 3 t))
         (assert (= t (length p)))))))

  (define-syntax assert-input-for-decoding
    (syntax-rules ()
      ((_ u v t)
       (begin
         (assert (non-negative-integer? u))
         (assert (non-negative-integer? v))
         (assert (integer? t))
         (assert (<= 3 t))))))

  ;; OldEncoder(p,t)
  (define (old-encoder p t)
    (assert-input-for-encoding p t)
    (let* ((a (apply max p))
           (v (* 2 a (factorial (- t 1))))
           (u (crt p (map (lambda (i) (+ 1 (* v i))) (iota-of-base-1 t)))))
      (values u v)))

  ;; OldDecoder(u,v,t)
  (define (old-decoder u v t)
    (assert-input-for-decoding u v t)
    (map (lambda (i)
           (mod u (+ 1 (* i v))))
         (iota-of-base-1 t)))

  ;; EncodeNumbers(p,t)
  (define (encode-numbers p t)
    (assert-input-for-encoding p t)
    (if (= t 3)
        (old-encoder p t)
        (let* ((a (apply max p))
               (c (+ a (mod (+ a t) 2))))
          (let lp ((p p)
                   (p1 (list c))
                   (t1 1))
            (case (length p)
              ((0) (encode-numbers (reverse p1) t1))
              ((1) (encode-numbers (reverse (cons (car p) p1)) (+ t1 1)))
              (else (lp (cddr p)
                        (cons (crt (list (car p) (cadr p))
                                   (list (+ c 1) (+ c 2)))
                              p1)
                        (+ t1 1))))))))

  ;; DecodeNumbers(u,v,t)
  (define (decode-numbers u v t)
    (assert-input-for-decoding u v t)
    (let lp ((i 3)
             (p (old-decoder u v 3)))
      (if (>= i t)
          p
          (let ((c (car p)))
            (let ilp ((i 0)
                      (p (cdr p))
                      (p1 '()))
              (if (< 1 (length p))
                  (let ((u (car p)))
                    (ilp (+ i 2)
                         (cdr p)
                         (append p1 (list (mod u (+ c 1)) (mod u (+ c 2))))))
                  (let ((u (car p)))
                    (if (odd? c)
                        (lp (+ i 1) (append p1 (list u)))
                        (lp (+ i 2) (append p1 (list (mod u (+ c 1)) (mod u (+ c 2)))))))))))))

)
