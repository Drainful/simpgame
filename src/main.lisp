(defmacro declaim-ftype (fname argument-types return-type)
  `(declaim (ftype (function ,argument-types ,return-type) ,fname)))

(defmacro declaim-ftypes (&rest body)
  "declaims ftypes (in reverse order)"
  (let ((altered-body '()))
    (dolist (form body)
      ;;(push `(check-type ,(first form) (function ,(second form) ,(third form))) altered-body)
      (push (push 'declaim-ftype form) altered-body))
    `(progn ,@altered-body)))

(defmacro fn (fname lambda-list argument-types return-type &rest body)
  `(progn
     (declaim (ftype (function ,argument-types ,return-type) ,fname))
     (defun ,fname ,lambda-list ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SB-DEBUG:BACKTRACE-AS-LIST)
  (sb-ext:restrict-compiler-policy 'safety 3)
  (ql:quickload :croatoan :silent t)
  (ql:quickload :iterate :silent t)
  (ql:quickload :generic-comparability :silent t)
  (use-package ':generic-comparability)
  (use-package ':croatoan))

(defun load-relative (filespec)
  (load (merge-pathnames filespec *load-pathname*)))
(load-relative "utils.lisp")
(load-relative "vector2.lisp")
(load-relative "game-object.lisp")
(load-relative "model.lisp")
(load-relative "view.lisp")
(load-relative "update.lisp")

(defparameter +world-size+ 1000)

(defparameter *player* (make-instance 'player :position (make-vector2 11 11)))

(defun player-move-event (x y)
  (make-move-event
   *player*
   (make-vector2 x y)))

;; could create events other than move
(defun create-input-event (input)
   (case input
     (#\h (player-move-event -1 0))
     (#\j (player-move-event 0 1))
     (#\k (player-move-event 0 -1))
     (#\l (player-move-event 1 0))
     (otherwise (player-move-event 0 0))))
(model-init *player*)

(defun map-state ()
  (model-init *player*)
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visibility nil)
    (clear scr)
    ;; (move scr 2 0)
    ;; (format scr "Type chars. Type q to quit.~%~%")
    (refresh scr)
    ;; (setf (.color-pair scr) '(:yellow :red)
    ;;       (.attributes scr) '(:bold))
    (event-case (scr event)
      (#\q (return-from event-case))
      (otherwise

       (let ((e (create-input-event event)))
         (update *model* e)
         (clear scr)
         (view *model* scr)
         (refresh scr)))
         ;;(format scr (format-class *player*))
      ;; (otherwise (princ event scr)
      ;;            (refresh scr))
      )))

(defun start ()
  (map-state))

(defvar *thread* nil)

(defun threaded-start ()
  (defparameter *thread* (sb-thread:make-thread #'start)))

(defun force-quit ()
  (sb-thread:terminate-thread *thread*))
