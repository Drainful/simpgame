(in-package :simpgame)

(defclass hash-set ()
  ((values :type hash-table
           :reader get-values
           :initform (make-hash-table :test 'object-equal-p))))

(defun hash-set (&rest values)
  (let ((set (make-instance 'hash-set)))
    (dolist (value values)
      (set-add value set))
    set))

(defgeneric set-add (value set)
  (:documentation "adds the given value to the given set."))
(defmethod set-add (value set)
  (let ((new-set (hash-set)))
    (set-add value new-set)
    new-set))

(defmethod set-add (value (set hash-set))
  (setf (gethash value (get-values set)) t)
  set)

(defgeneric set-remove (value set)
  (:documentation "removes the given value from the given set."))
(defmethod set-remove (value set)
  set)
(defmethod set-remove (value (set hash-set))
  (remhash value (get-values set))
  set)

(defmethod set-contains (value set)
  nil)
(defmethod set-contains (value (set hash-set))
  (gethash value (get-values set)))

(defmethod set-to-list (set)
  nil)
(defmethod set-to-list ((set hash-set))
  (hash-keys (get-values set)))

(defun set-union (&rest sets)
  (let ((union (hash-set)))
    (dolist (set sets)
      (iter (for value in (set-to-list set))
        (unless (set-contains value union)
          (set-add value union))))
    union))

(defmethod format-class ((set hash-set))
  (let ((final-string (string "")))
    (dolist (value (set-to-list set))
      (setf final-string (concatenate 'string
                                     final-string
                                     (concatenate 'string
                                                  (format-class value)
                                                  "~%"))))
    (format nil final-string)))
