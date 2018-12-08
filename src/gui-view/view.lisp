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

(defvar *assets-path* (asdf:system-relative-pathname :qtools-game "assets/"))

(defun asset (pathname)
  (merge-pathnames pathname *assets-path*))

;; VIEW
(define-widget view (QGraphicsView) ())
;; send keypresses to *model*, then call next qmethod.
(define-override (view key-press-event) (ev)
  (update simpgame-model::*model* `(,(create-input-event (q+:key ev) simpgame-model::*model*)))
  (call-next-qmethod))

;; SCENE
(define-widget scene (QGraphicsScene) ())

;; Main widget
(define-widget simpgame (QWidget) ())

(define-subwidget (simpgame view) (make-instance 'view))

(define-subwidget (simpgame layout) (q+:make-qvboxlayout simpgame)
  (setf (q+:window-title simpgame) "Simpgame")
  (q+:add-widget layout view))

(defun main ()
  (model-init)
  (with-main-window (window (make-instance 'simpgame))))

;; could create events other than move
(defmethod create-input-event (input (model model))
  "Requires a reference to model in order to determine whether to create a move or attack event"
   (cond
     ((= input (q+:qt.key_h)) (player-mattack-event -1 0 model))
     ((= input (q+:qt.key_j)) (player-mattack-event 0 1 model))
     ((= input (q+:qt.key_k)) (player-mattack-event 0 -1 model))
     ((= input (q+:qt.key_l)) (player-mattack-event 1 0 model))
     (t (player-mattack-event 0 0 model))))
