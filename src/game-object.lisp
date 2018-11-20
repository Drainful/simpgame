(defclass has-position ()
  ((pos :type vector2
        :initarg :position
        :accessor pos)))

(defmethod mutate-position-delta ((subject has-position) (delta vector2))
  (setf (pos subject) (sum-vector2
                         (pos subject)
                         delta)))

(defmethod format-class ((class has-position))
  (format-class (pos class)))

(defclass drawable ()
  ((glyph :type character
          :reader get-glyph)))

(defclass has-behavior () ())
(defclass random-walker (has-position has-behavior) ())

(defgeneric generate-behavior-events (actor)
  (:documentation "generates the events to do with the behavior of the given actor."))

(defmethod generate-behavior-events ((actor has-behavior))
  (list))

(defmethod generate-behavior-events ((actor random-walker))
  (list (make-move-event actor (make-vector2-random-walk))))

(defclass player (has-position drawable)
  ((glyph :initform #\@)))

(defclass shade (hash-position drawable)
  ((glyph :initform #\s)))
