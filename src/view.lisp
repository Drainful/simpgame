(defun view (model scr)
  (clear scr)
  (let* ((player (get-player model))
         (p (pos player))
         (p-x (get-x p))
         (p-y (get-y p))
         (glyph (get-glyph player))
         (width (min (.width scr) (array-dimension (get-floor-tiles *model*) 0)))
         (height (min (.height scr) (array-dimension (get-floor-tiles *model*) 1))))
    (loop for x from 0 below width do
         (loop for y from 0 below height do
              (move scr y x)
              (format scr (if (aref (get-floor-tiles model) x y)
                              "."
                              " "))))
    (move scr p-y p-x)
    (format scr (string glyph)))
  (refresh scr))
