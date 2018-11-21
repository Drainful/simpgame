;;;; EVENTS

;;; event priority symbols
(defparameter +priority+ '(:combat :movement))

(defun priority-int (priority)
  (find priority +priority+))

(defun priority-symbol (index)
  (nth index +priority+))

;;; event classes
(defclass event ()
  ((priority :allocation :class
             :reader get-priority)))

(defmethod format-class ((class event))
  (string "event"))

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

(defun player-move-event (x y)
  (make-move-event
   *player*
   (make-vector2 x y)))

;; could create events other than move
(defun create-input-event (input)
   (case input
     (#\h (player-move-event -1 0))
     (#\j (player-move-event 0 1))
     (#\k (player-move-event 0 -1))
     (#\l (player-move-event 1 0))
     (otherwise (player-move-event 0 0))))

;;;; EVENT CREATION
(defgeneric generate-behavior-events (actor model)
  (:documentation "generates the events to do with the behavior of the given actor."))

(defmethod generate-behavior-events ((actor has-behavior) model)
  (list))
(defmethod generate-behavior-events ((actor random-walker) model)
  (list (make-move-event actor (make-vector2-random-walk))))

;;;; EVENT RESOLUTION
(defun resolve-events (model)
  (let ((events (events model)))
    (dolist (priority +priority+)
      (let ((events (get-events-of-priority priority events)))
        (dolist (event events)
          (resolve-event model event))))
    (purge-events events)))

(defgeneric resolve-event (model event))

(defmethod resolve-event (model (event move-event))
  (format-class event)
  (let* ((target (get-move-target event))
         (x (get-x target))
         (y (get-y target)))
    (when (aref (get-floor-tiles model) x y)
      (mutate-position-delta (get-subject event) (get-delta event)))))

;;;; UPDATE FUNCTION
(defun update (model player-input)
  "Update the model based on both player input and actor behaviors"
  (dolist (actor (set-to-list (get-game-objects-of-class (find-class 'has-behavior) (get-game-objects model))))
    (dolist (event (generate-behavior-events actor model))
      (add-event event (events model))))
  (add-event (create-input-event player-input) (events model))
  (resolve-events model))
