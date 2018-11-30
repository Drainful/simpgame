(in-package simpgame-qt-view)
(in-readtable :qtools)

;; (defun view (model scr)
;;   (clear scr)
;;   (let* ((width (min (.width scr) (array-dimension (get-floor-tiles *model*) 0)))
;;          (height (min (.height scr) (array-dimension (get-floor-tiles *model*) 1))))
;;     (loop for x from 0 below width do
;;          (loop for y from 0 below height do
;;               (move scr y x)
;;               (format scr (if (aref (get-floor-tiles model) x y)
;;                               "."
;;                               " "))))
;;     (dolist (drawable (set-to-list (get-game-objects-of-class (find-class 'drawable) (get-game-objects model))))
;;       (draw-object drawable scr)))
;;   (refresh scr))

;; (defgeneric draw-object (drawable screen)
;;   (:documentation "draws a drawable object on the screen"))

;; (defvar *canvas-width* 1200)
;; (defvar *canvas-height* 800)
;; (defvar *black* (vec4 0 0 0 1))
;; (defvar *origin* (vec2 0 0))
  ; window's title

;; (gamekit:defgame simpgame () ())
;; (defmethod gamekit:draw ((app simpgame))
;;   ;; Let's draw a black box in the bottom-left corner
;;   (gamekit:draw-rect *origin* 100 100 :fill-paint *black*))

;; (defun main ()
;;   (model-init)
;;   (start 'simpgame))

(defun main ())

;; could create events other than move
(defmethod create-input-event (input (model model))
  "Requires a reference to model in order to determine whether to create a move or attack event"
   (case input
     (#\h (player-mattack-event -1 0 model))
     (#\j (player-mattack-event 0 1 model))
     (#\k (player-mattack-event 0 -1 model))
     (#\l (player-mattack-event 1 0 model))
     (otherwise (player-mattack-event 0 0 model))))
