(in-package :simpgame)

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
