(in-package :simple-game)

;; PACKAGES
(eval-when (:compile-toplevel :load-toplevel :execute)
  (SB-DEBUG:BACKTRACE-AS-LIST)
  (sb-ext:restrict-compiler-policy 'safety 3))

;; INITIALIZE PARAMETERS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +world-size+ 1000)
  (model-init))

;; CORE LOOP, update then view.
(defun map-state ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visible nil)
    (event-case (scr event)
      (#\q (return-from event-case))
      (otherwise
       (update *model* event)
       (view *model* scr)))))

(defun start ()
  (map-state))
