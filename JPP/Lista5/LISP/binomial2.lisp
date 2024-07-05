(defun next_row (row)
  (mapcar #'+ (append row '(0)) (append '(0) row)))

(defun generate_pascal (n &optional (rows '((1))))
  (if (zerop n)
      (nreverse rows)
      (generate_pascal (1- n) (cons (next_row (first rows)) rows))))

(defun binomial2 (n k)
  (nth k (nth n (generate_pascal n))))