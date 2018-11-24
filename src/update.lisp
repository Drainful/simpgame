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
   (target :initarg :target
           :reader get-target)
   (priority :initform :movement)))

(defmethod make-move-event-delta ((subject has-position) (delta vector2))
  (make-instance 'move-event :subject subject :target (sum-vector (pos subject) delta)))

(defmethod make-move-event ((subject has-position) (target vector2))
  (make-instance 'move-event :subject subject :target target))

(defmethod get-delta ((move-event move-event))
  (difference-vector (get-target move-event) (pos (get-subject move-event))))

(defmethod format-class ((class move-event))
  (format-class (get-delta class)))

(defclass attack-event (event)
  ((target :type vector2
           :initarg :target
           :reader get-target)
   (damage :type integer
           :initarg :damage
           :reader get-damage)))

(defgeneric make-attack-event (target actor)
  (:documentation "
Make at attack event from the actor to the target location.
Damage depends on the qualities of the actor."))

(defmethod make-attack-event (target actor)
  (make-instance 'attack-event :target target :damage 1))

(defmethod move-or-attack-event (target actor model)
  (if (get-object-at target model)
      (make-attack-event target actor)
      (make-move-event actor target)))

(defun player-move-event (x y)
  (make-move-event-delta
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
  (list (make-move-event-delta actor (make-vector2-random-walk))))

;;;; EVENT RESOLUTION
(defun resolve-events (model)
  (let ((events (events model)))
    (dolist (priority +priority+)
      (let ((events (get-events-of-priority priority events)))
        (dolist (event events)
          (resolve-event model event))))
    (purge-events events)))

(defgeneric resolve-event (model event))

(defmethod resolve-event ((model model) (event move-event))
  (update-position-collide (get-subject event) (get-target event) model))

(defun update-position-collide (subject target model)
  (when (and (tile-at-p target model)
             (not (get-object-at target model)))
    (remhash (pos subject) (get-game-objects-by-position model))
    (setf (gethash target (get-game-objects-by-position model)) subject)
    (mutate-position subject target)))

;;;; UPDATE FUNCTION
(defun update (model player-input)
  "Update the model based on both player input and actor behaviors"
  (dolist (actor (set-to-list (get-game-objects-of-class (find-class 'has-behavior) (get-game-objects model))))
    (dolist (event (generate-behavior-events actor model))
      (add-event event (events model))))
  (add-event (create-input-event player-input) (events model))
  (resolve-events model))
