(defun number_sequence (start end)
  (if (> start end)
      nil
      (cons start (number_sequence (1+ start) end))))

(defun primes (n)
  (remove-if-not
   (lambda (p)
     (null (remove-if-not (lambda (x) (= (mod p x) 0))
                          (number_sequence 2 (isqrt p)))))
   (number_sequence 2 n)))