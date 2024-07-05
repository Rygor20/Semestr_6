(defun binomial (n k)
  (cond
    ((= k 0) 1)
    ((= n k) 1)
    ((= n 0) 0)
    (t (+ (binomial (1- n) (1- k)) (binomial (1- n) k)))))