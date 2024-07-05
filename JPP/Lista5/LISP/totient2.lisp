;; (defun factor (n p)
;;   (cond
;;     ((> (* p p) n) (if (> n 1) (list n) nil))
;;     ((= (mod n p) 0) (cons p (factor (/ n p) p)))
;;     (t (factor n (1+ p)))))

;; (defun prime_factors (n)
;;   (factor n 2))

(defun totient2 (n)
  (let* ((factors (remove-duplicates (prime_factors n)))
         (result n))
    (dolist (p factors result)
      (setf result (* result (- 1 (/ 1.0 p)))))
    (multiple-value-bind (integer_part _)
        (floor result)
      integer_part)))