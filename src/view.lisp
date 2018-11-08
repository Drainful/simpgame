(defun view (model scr)
  (let* ((player (get-player model))
         (p (pos player))
         (p-x (get-x p))
         (p-y (get-y p))
         (glyph (get-glyph player))
         (width (min (.width scr) (array-dimension (get-floor-tiles *model*) 0)))
         (height (min (.height scr) (array-dimension (get-floor-tiles *model*) 1))))
    ;;(format scr (format-class p))
    (loop for x from 0 below width do
         (loop for y from 0 below height do
              (move scr y x)
              (format scr (if (aref (get-floor-tiles model) x y)
                              "."
                              " "))))
    (move scr p-y p-x)
    (format scr (string glyph)))
  ;; ((croatoan:add-char scr (get-glyph (get-player model)) 0 0))
  )
