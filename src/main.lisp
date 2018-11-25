;; MACROS
(sb-ext:restrict-compiler-policy 'debug 3)
(defmacro declaim-ftype (fname argument-types return-type)
  "create function type declaration"
  `(declaim (ftype (function ,argument-types ,return-type) ,fname)))

(defmacro declaim-ftypes (&rest body)
  "declaims ftypes (in reverse order)"
  (let ((altered-body (list)))
    (dolist (form body)
      (push (push 'declaim-ftype form) altered-body))
    `(progn ,@altered-body)))

(defmacro fn (fname lambda-list argument-types return-type &rest body)
  "combined function and function type declatation"
  `(progn
     (declaim (ftype (function ,argument-types ,return-type) ,fname))
     (defun ,fname ,lambda-list ,@body)))

;; PACKAGES
(eval-when (:compile-toplevel :load-toplevel :execute)
  (SB-DEBUG:BACKTRACE-AS-LIST)
  (sb-ext:restrict-compiler-policy 'safety 3)
  (ql:quickload :croatoan :silent t)
  (ql:quickload :iterate :silent t)
  (ql:quickload :generic-comparability :silent t)
  (use-package ':generic-comparability)
  (use-package ':croatoan))

;; LOAD FILES
(defun load-relative (filespec)
  (load (merge-pathnames filespec *load-pathname*)))
(load-relative "utils.lisp")
(load-relative "vector2.lisp")
(load-relative "game-object.lisp")
(load-relative "model.lisp")
(load-relative "view.lisp")
(load-relative "update.lisp")

;; INITIALIZE PARAMETERS
(defparameter +world-size+ 1000)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (model-init))

;; CORE LOOP, update then view.
(defun map-state ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visibility nil)
    (event-case (scr event)
      (#\q (return-from event-case))
      (otherwise
       (update *model* event)
       (view *model* scr)))))

(defun start ()
  (map-state))
