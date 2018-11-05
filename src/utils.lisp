(defgeneric format-class (class))
(defmethod format-class (class)
  (string "class"))

(defgeneric object-equal-p (o1 o2))
(defmethod object-equal-p (o1 o2)
  (equalp o1 o2))

(defgeneric object-hash (object))
