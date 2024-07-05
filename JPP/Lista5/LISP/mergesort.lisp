(defun merge_lists (xs ys)
  (cond
    ((null xs) ys)
    ((null ys) xs)
    ((<= (car xs) (car ys))
     (cons (car xs) (merge_lists (cdr xs) ys)))
    (t (cons (car ys) (merge_lists xs (cdr ys))))))

(defun split_sequence (seq)
  (let* ((half (truncate (/ (length seq) 2)))
         (ys (subseq seq 0 half))
         (zs (subseq seq half)))
    (values ys zs)))

(defun mergesort (xs)
  (if (or (null xs) (null (cdr xs)))
      xs
      (multiple-value-bind (ys zs) (split_sequence xs)
        (merge_lists (mergesort ys) (mergesort zs)))))