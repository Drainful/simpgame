(in-package :simple-game)

(defclass vector2 ()
  ((x :type integer
      :initarg :x
      :reader get-x)
   (y :type integer
      :initarg :y
      :reader get-y)))

(defun add-x (vec amount)
  (make-vector2 (+ amount (get-x vec)) (get-y vec)))

(defun add-y (vec amount)
  (make-vector2 (get-x vec) (+ amount (get-y vec))))

(defmethod magnitude ((vec vector2))
  (expt (+ (expt (get-x vec) 2) (expt (get-y vec) 2)) 0.5))

(defmethod euclidian-distance ((vector-one vector2) (vector-two vector2))
  (magnitude (difference-vector vector-one vector-two)))

(defmethod taxicab-distance ((vector-one vector2) (vector-two vector2))
  (let ((difference (difference-vector vector-one vector-two)))
    (+ (abs (get-x difference)) (abs (get-y difference)))))

(fn make-vector2 (x y) (integer integer) vector2
  (make-instance 'vector2 :x x :y y))

(defun make-vector2-random-walk ()
  (flet ((random-coord () (- (random 3) 1)))
    (make-vector2 (random-coord) (random-coord))))

(defmethod sum-vector ((vector-one vector2) (vector-two vector2))
  (make-instance 'vector2
                 :x (+ (get-x vector-one) (get-x vector-two))
                 :y (+ (get-y vector-one) (get-y vector-two))))

(defmethod map-vec (f (vector vector2))
  (make-vector2 (apply f `(,(get-x vector))) (apply f `(,(get-y vector)))))

(defmethod difference-vector ((vector-one vector2) (vector-two vector2))
  (sum-vector vector-one (invert-vector vector-two)))

(defmethod invert-vector ((vector vector2))
  (make-vector2 (- (get-x vector)) (- (get-y vector))))

(defmethod randomly-cardinalize ((vector vector2))
  (if (or (eql (abs (get-x vector)) 0) (eql (get-y vector) 0))
      vector
      (if (eql (random 2) 1)
          (make-vector2 (get-x vector) 0)
          (make-vector2 0 (get-y vector)))))

(defmethod format-class ((class vector2))
  (format nil "(~a, ~a)" (get-x class) (get-y class)))

(defmethod object-equal-p ((o1 vector2) (o2 vector2))
  (and (eql (get-x o1) (get-x o2)) (eql (get-y o1) (get-y o2))))

;; (defmethod object-hash ((object vector2))
;;   (base (+ (get-x object) (* 1000 (get-y object)))))

(defmethod object-hash ((object vector2))
  (let ((base (+ (get-x object) (* 1000 (get-y object)))))
    (if (< base 0)
        (expt base 2)
        base)))
