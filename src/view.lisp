(in-package :simpgame)

(defun view (model scr)
  (clear scr)
  (let* ((width (min (.width scr) (array-dimension (get-floor-tiles *model*) 0)))
         (height (min (.height scr) (array-dimension (get-floor-tiles *model*) 1))))
    (loop for x from 0 below width do
         (loop for y from 0 below height do
              (move scr y x)
              (format scr (if (aref (get-floor-tiles model) x y)
                              "."
                              " "))))
    (dolist (drawable (set-to-list (get-game-objects-of-class (find-class 'drawable) (get-game-objects model))))
      (draw-object drawable scr)))
  (refresh scr))

(defgeneric draw-object (drawable screen)
  (:documentation "draws a drawable object on the screen"))
