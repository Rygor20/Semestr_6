(defun factor (n p)
  (cond
    ((> (* p p) n) (if (> n 1) (list n) nil))
    ((= (mod n p) 0) (cons p (factor (/ n p) p)))
    (t (factor n (1+ p)))))

(defun prime_factors (n)
  (factor n 2))
