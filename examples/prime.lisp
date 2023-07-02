(define (is-prime-helper n x)
  (if (< n (* x x))
      true
      (if (= 0 (% n x))
          false
          (is-prime-helper n (+ 1 x)))))

(define (is-prime n)
  (if (< n 2)
      false
      (is-prime-helper n 2)))

(is-prime 479001599)
