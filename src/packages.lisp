(defpackage :simpgame-utils
  (:use
   #:common-lisp
   #:iterate)
  (:export
   ;; macros.lisp
   :declaim-ftype
   :declaim-ftypes
   :fn

   ;; set.lisp
   :hash-set
   :set-add
   :set-remove
   :set-contains
   :set-to-list
   :set-union

   ;; utils.lisp
   :random-element
   :format-class
   :hash-keys
   :get-superclasses
   :object-equal-p
   :object-hash

   ;; vector2.lisp
   :vector2
   :get-x
   :get-y
   :make-vector2
   :sum-vector
   :map-vec
   :difference-vector
   :invert-vector
   :randomly-cardinalize
   :make-vector2-random-walk
   :add-x
   :add-y
   :magnitude
   :euclidian-distance
   :taxicab-distance))

(defpackage :simpgame-model
  (:use
   #:common-lisp
   #:iterate
   #:simpgame-utils)
  (:export
   :model
   :model-init
   ;; controls
   :update
   :player-mattack-event
   ;; view
   :get-visible-objects
   ))

(defpackage simpgame-gamekit-view
  (:use
   #:common-lisp
   #:trivial-gamekit
   #:iterate
   #:simpgame-model
   #:simpgame-utils)
  (:export
   ;; main entry point
   :main))

(defpackage simpgame-qt-view
  (:use
   #:cl+qt
   #:iterate
   #:simpgame-model
   #:simpgame-utils)
  (:export
   ;; main entry point
   :main))

;; DEBUG OPTIONS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (SB-DEBUG:BACKTRACE-AS-LIST)
  (sb-ext:restrict-compiler-policy 'safety 3))
