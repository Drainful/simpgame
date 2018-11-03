(defclass event ()
  ((priority :allocation :class
             :reader get-priority)))

(defmethod format-class ((class event))
  (string "event"))

(defun priority-int (priority)
  (case priority
    (:movement 1)
    (:combat 0)))

(defclass move-event (event)
  ((subject :initarg :subject
            :reader get-subject)
   (delta :initarg :delta
          :reader get-delta)
   (priority :initform :movement)))

(fn make-move-event (subject vector) (has-position vector2) move-event
    (make-instance 'move-event :subject subject :delta vector))

(defmethod get-move-target ((move-event move-event))
  (sum-vector2 (pos (get-subject move-event)) (get-delta move-event)))

(defmethod format-class ((class move-event))
  (format-class (get-delta class)))

(defgeneric update (model event))

(declaim-ftype update (model event) nil)
(defmethod update (model (event move-event))
  (let* ((target (get-move-target event))
         (x (get-x target))
         (y (get-y target)))
    (when (aref (floor-tiles model) x y)
      (mutate-position-delta (get-subject event) (get-delta event)))))
