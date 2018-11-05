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

(defclass player (has-position drawable)
  ((glyph :initform #\@)))
