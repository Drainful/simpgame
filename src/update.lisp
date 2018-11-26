(in-package :simpgame)

;;;; EVENTS

;;; event priority symbols
(defparameter +priority+ '(:attack :movement))

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

(defmethod enforce-movement-limitation ((vector vector2))
  (randomly-cardinalize vector))

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
           :reader get-damage)
   (priority :initform :attack)))

(defgeneric make-attack-event (target actor)
  (:documentation "
Make at attack event from the actor to the target location.
Damage depends on the qualities of the actor."))

(defmethod make-attack-event (target actor)
  (make-instance 'attack-event :target target :damage 1))

(defgeneric move-or-attack-event (target actor model)
  (:documentation "
Creates either a move event or an attack event based on whether 
there is a valid target at the given location according to the model."))
(defmethod move-or-attack-event (target actor model)
  (if (get-object-at target model)
      (make-attack-event target actor)
      (make-move-event actor target)))

(defmethod move-by-or-attack-event (delta actor model)
  (let ((target (sum-vector (pos actor) delta)))
    (move-or-attack-event target actor model)))

(defmethod move-toward-or-attack-event (target actor model)
  (flet ((cap-to-step (x) (max -1 (min 1 x))))
    (let* ((delta (difference-vector target (pos actor)))
           (step (enforce-movement-limitation (map-vec #'cap-to-step delta))))
      (move-by-or-attack-event step actor model))))

(defun player-move-event (x y)
  (make-move-event-delta
   *player*
   (make-vector2 x y)))

(defun player-mattack-event (dx dy model)
  "creates a move or attack event based on the game object
at the location of the delta from player position and (dx, dy)"
  (let ((x (+ dx (get-x *player*)))
        (y (+ dy (get-y *player*))))
    (move-or-attack-event (make-vector2 x y)
                           *player*
                           model)))
;; could create events other than move
(defmethod create-input-event (input (model model))
  "Requires a reference to model in order to determine whether to create a move or attack event"
   (case input
     (#\h (player-mattack-event -1 0 model))
     (#\j (player-mattack-event 0 1 model))
     (#\k (player-mattack-event 0 -1 model))
     (#\l (player-mattack-event 1 0 model))
     (otherwise (player-move-event 0 0))))

;;;; EVENT CREATION
(define-method-combination behavior ()
    ((methods (or)))
  `(or ,@(mapcar #'(lambda (method)
                      `(call-method ,method))
                  methods)))

(defgeneric generate-behavior-events (actor model)
  (:documentation "generates the events to do with the behavior of the given actor."))

  ;;(:method-combination behavior)

(defmethod generate-behavior-events ((actor has-behavior) model)
  (list))
(defmethod generate-behavior-events ((actor random-walker) model)
  (list (make-move-event-delta actor (make-vector2-random-walk))))
(defmethod generate-behavior-events ((actor simple-attacker) model)
  (list (let ((dist (taxicab-distance (pos actor) (pos *player*))))
          (if (<= dist (get-aggression-range actor))
              (move-toward-or-attack-event (pos *player*) actor model)
              (make-move-event-delta actor (make-vector2-random-walk))))))

;;;; EVENT RESOLUTION
(defun resolve-events (model)
  (let ((events (events model)))
    (dolist (priority +priority+)
      (let ((events (get-events-of-priority priority events)))
        (dolist (event events)
          (resolve-event model event))))
    (purge-events events)))

(defgeneric resolve-event (model event))

(defmethod resolve-event (model (event attack-event))
  (take-damage-at (get-damage event) (get-target event) model))

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
  (add-event (create-input-event player-input model) (events model))
  (resolve-events model))
