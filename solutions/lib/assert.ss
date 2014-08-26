(define (assert name res expect)
  (if (not (equal? res expect))
      (error "Failed test:" name (list res expect))
      'ok))
