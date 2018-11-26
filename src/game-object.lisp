(in-package :simpgame)

;;;; BUILDING BLOCKS
(defclass game-object ()
  ((id-counter :type integer
               :allocation :class
               :initform 0
               :accessor id-counter)
   (id :type integer
       :reader get-id)))

(defmethod object-hash ((object game-object))
  (get-id object))

(defmethod initialize-instance :after ((object game-object) &key)
  (setf (slot-value object 'id) (id-counter object))
  (incf (id-counter object)))

(defclass has-health ()
  ((health :type integer
           :initform 1
           :initarg :health
           :accessor health)))

(defun take-damage-at (damage location model)
  (check-type location vector2)
  (let ((object (get-object-at location model)))
    (when object (take-damage damage object model))))

;; TODO add a thing to do something about zero hp.
(defgeneric take-damage (damage object model)
  (:documentation "object takes damage amount of damage, 
and is deleted if its health drops to zero or below."))
(defmethod take-damage (damage (object has-health) model)
  (setf (health object) (- (health object) damage))
  (when (<= (health object) 0)
    (remove-game-object object model)))

(defclass has-position ()
  ((pos :type vector2
        :initform (make-vector2 0 0)
        :initarg :position
        :accessor pos)))

(defmethod mutate-position-delta ((subject has-position) (delta vector2))
  (setf (pos subject) (sum-vector
                         (pos subject)
                         delta)))

(defmethod mutate-position ((subject has-position) (target vector2))
  (setf (pos subject) target)
  target)

(defmethod format-class ((class has-position))
  (format-class (pos class)))

(defmethod get-x ((object has-position))
  (get-x (pos object)))

(defmethod get-y ((object has-position))
  (get-y (pos object)))

(defclass drawable ()
  ((glyph :type character
          :reader get-glyph)))

(defclass drawable-world-position (drawable has-position) ())

(defmethod draw-object ((drawable drawable-world-position) screen)
  (move screen (get-y (pos drawable)) (get-x (pos drawable)))
  (format screen (string (get-glyph drawable))))

;;; BEHAVIOR
(defclass has-behavior () ())
(defclass random-walker (has-position has-behavior) ())
(defclass simple-attacker (has-position has-behavior)
  ((aggression-range :type integer
                     :initform 10
                     :initarg :aggression-range
                     :reader get-aggression-range)))

;;;; GAME OBJECTS
(defclass player (drawable-world-position has-health game-object)
  ((glyph :initform #\@)
   (health :initform 10)))

(defmethod format-class ((object player))
  (concatenate 'string "player: " (call-next-method)))

(defclass confused-snake (simple-attacker random-walker drawable-world-position has-health game-object)
  ((glyph :initform #\s)))

(defmethod format-class ((object confused-snake))
  (concatenate 'string "snake: " (call-next-method)))

