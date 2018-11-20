(defclass event ()
  ((priority :allocation :class
             :reader get-priority)))

(defmethod format-class ((class event))
  (string "event"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +priority+ #(:combat :movement)))

(defun priority-int (priority)
  (find priority +priority+))

(defun priority-symbol (index)
  (elt +priority+ index))

(defclass move-event (event)
  ((subject :initarg :subject
            :reader get-subject)
   (delta :initarg :delta
          :reader get-delta)
   (priority :initform :movement)))

(defmethod make-move-event ((subject has-position) (vector vector2))
    (make-instance 'move-event :subject subject :delta vector))

(defmethod get-move-target ((move-event move-event))
  (sum-vector2 (pos (get-subject move-event)) (get-delta move-event)))

(defmethod format-class ((class move-event))
  (format-class (get-delta class)))

(defun resolve-events (model)
  (let ((events (events model)))
    (dolist (priority +priority+)
      (let ((events (get-events-of-priority priority events)))
        (dolist (event events)
          (resolve-event model event))))
    (purge-events events)))

(defgeneric resolve-event (model event))

(declaim-ftype resolve-event (model event) *)
(defmethod resolve-event (model (event move-event))
  (let* ((target (get-move-target event))
         (x (get-x target))
         (y (get-y target)))
    (when (aref (get-floor-tiles model) x y)
      (mutate-position-delta (get-subject event) (get-delta event)))))

(defun update (model player-input)
  "Update the model based on both player input and actor behaviors"
  (let ((e (create-input-event player-input)))
    (resolve-event model e)))
