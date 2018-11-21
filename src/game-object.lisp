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

(defclass has-position ()
  ((pos :type vector2
        :initform (make-vector2 0 0)
        :initarg :position
        :accessor pos)))

(defmethod mutate-position-delta ((subject has-position) (delta vector2))
  (setf (pos subject) (sum-vector2
                         (pos subject)
                         delta)))

(defmethod mutate-position ((subject has-position) (target vector2))
  (setf (pos subject) target)
  target)

(defmethod format-class ((class has-position))
  (format-class (pos class)))

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

;;;; GAME OBJECTS
(defclass player (drawable-world-position game-object)
  ((glyph :initform #\@)))

(defmethod format-class ((object player))
  (concatenate 'string "player: " (call-next-method)))

(defclass confused-snake (random-walker drawable-world-position game-object)
  ((glyph :initform #\s)))

(defmethod format-class ((object confused-snake))
  (concatenate 'string "snake: " (call-next-method)))

