;; (defun number_sequence (start end)
;;   (if (> start end)
;;       nil
;;       (cons start (number_sequence (1+ start) end))))

(defun totient (n)
  (length (remove-if-not (lambda (x) (= (gcd x n) 1)) (number_sequence 1 n))))