(defclass vector2 ()
  ((x :initarg :x
      :reader get-x)
   (y :initarg :y
      :reader get-y)))

(defun add-x (vec amount)
  (make-vector2 (+ amount (get-x vec)) (get-y vec)))

(defun add-y (vec amount)
  (make-vector2 (get-x vec) (+ amount (get-y vec))))

(fn make-vector2 (x y) (integer integer) vector2
  (make-instance 'vector2 :x x :y y))

(fn sum-vector2 (vector-one vector-two) (vector2 vector2) vector2
  (make-instance 'vector2
                 :x (+ (get-x vector-one) (get-x vector-two))
                 :y (+ (get-y vector-one) (get-y vector-two))))

(defmethod format-class ((class vector2))
  (format nil "(~a, ~a)" (get-x class) (get-y class)))
