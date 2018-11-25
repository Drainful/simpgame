(defun add-nums (a b)
  (declare (type integer a))
  (declare (type integer b))
  (check-type a integer)
  (check-type b integer)
  (+ a b))

(defun fail ()
  (add-nums 1.3 34.4))
