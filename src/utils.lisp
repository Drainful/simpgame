(in-package :simpgame)

(defun random-element (list)
  "Get a random element of a list"
  (nth (random (length list)) list))

(defgeneric format-class (class)
  (:documentation "Convert a CLOS object into a human-readable string"))
(defmethod format-class (class)
  (string "class"))

(defun hash-keys (hash-table)
  "keys of a hash table"
  (loop for key being the hash-keys of hash-table collect key))

(defun get-superclasses (object)
  "Get all superclasses of a CLOS object"
  (sb-mop:class-precedence-list (class-of object)))

(defgeneric object-equal-p (o1 o2)
  (:documentation "Generic object equality"))
(defmethod object-equal-p (o1 o2)
  (equalp o1 o2))

(defgeneric object-hash (object)
  (:documentation "Generic hash function"))

;; Make a generic hash table test with object equal p and object hash.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:define-hash-table-test object-equal-p object-hash))
