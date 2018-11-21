(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :iterate :silent t)
  (use-package 'iterate))

(defclass model ()
  (
   ;; (player :type player
   ;;         :initform (make-instance 'player :position (make-vector2 11 11))
   ;;         ;;:reader get-player
   ;;         )
   (floor-tiles :type array
                :initform (make-array (list +world-size+ +world-size+) :initial-element nil)
                :reader get-floor-tiles)
   (dungeon :type hash-region
            :initform (make-hash-region)
            :initarg :dungeon
            :reader get-dungeon)
   (game-objects :type game-object-hash-storage
                 :initform (make-game-object-storage)
                 :accessor get-game-objects)
   (events :type event-hash-storage
           :initform (make-event-hash-storage)
           :reader events)))

(defmethod get-player ((model model))
  (first (set-to-list (get-game-objects-of-class (find-class 'player) (get-game-objects model)))))

(defun clear-game-objects (model)
  (setf (get-game-objects model) (make-game-object-storage)))

(defclass game-object-hash-storage ()
  ((game-objects :type hash-table
                 :initform (make-hash-table)
                 :reader get-game-objects
                 :documentation "map of superclass name to set of game object")))

(defun make-game-object-storage ()
  (make-instance 'game-object-hash-storage))

(defmethod add-game-object (game-object (objects game-object-hash-storage))
  (dolist (superclass (get-superclasses game-object))
    (setf (gethash (class-name superclass) (get-game-objects objects))
          (set-add game-object (gethash (class-name superclass) (get-game-objects objects))))))

(defmethod remove-game-object (game-object (objects game-object-hash-storage))
  (dolist (superclass (get-superclasses game-object))
    (setf (gethash (class-name superclass) (get-game-objects objects))
          (set-remove game-object (gethash (class-name superclass) (get-game-objects objects))))))

(defmethod get-game-objects-of-class (class (objects game-object-hash-storage))
  (gethash (class-name class) (get-game-objects objects)))

(defclass event-hash-storage ()
  ((priority-to-events :type hash-table
                       :initform (make-hash-table)
                       :reader by-priority)))

(defun make-event-hash-storage ()
  (make-instance 'event-hash-storage))

(defmethod get-events-of-priority (priority (events event-hash-storage))
  (gethash priority (by-priority events)))

(defmethod add-event (event (events event-hash-storage))
  (push event (gethash (get-priority event) (by-priority events))))

(defmethod purge-events ((events event-hash-storage))
  (setf (slot-value events 'priority-to-events) (make-hash-table)))

;;(deftype event-queue () hash-table)

;; (defclass layout-builder
;;     ())

(defun model-init ()
  (let ((dungeon (assemble-dungeon 10 70)))
    (defparameter *model* (make-instance 'model :dungeon dungeon))
    (carve-region dungeon *model*)
    (spawn (make-instance 'player) *model*)
    (defparameter *player* (get-player *model*))
    (dotimes (_ 4)
      (spawn (make-instance 'confused-snake) *model*))
    ;;(setf (pos (get-player *model*)) (random-element (get-locations dungeon)))
    ))

;; (spawn (make-instance 'confused-shade) *model*)

(defun spawn (has-position model)
  (setf (pos has-position) (random-element (get-locations (get-dungeon model))))
  (add-game-object has-position (get-game-objects model)))

(defun between (x y)
  (if (< x y)
      (iter (for i from x to y)
        (collect i))
      (iter (for i from y to x)
        (collect i))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:define-hash-table-test object-equal-p object-hash))

(defclass hash-region ()
  ((locations :type hash-table
              :reader locations
              :initform (make-hash-table :test 'object-equal-p))))

(defun make-hash-region (&rest points)
  (let ((region (make-instance 'hash-region)))
    (dolist (point points)
      (set-location point region t))
    region))

(defgeneric set-location (location region value)
  (:documentation "Sets the object contained at location in region to value"))

(defmethod set-location (location (region hash-region) value)
  (setf (gethash location (locations region)) value))
(defmethod set-location (location (region hash-table) value)
  (setf (gethash location region) value))

(defgeneric at-location (location region)
  (:documentation "Returns the object stored at location in region"))

(defmethod at-location (location (region hash-region))
  (gethash location (locations region)))
(defmethod at-location (location (region hash-table))
  (gethash location region))

(defgeneric get-locations (region)
  (:documentation "Get the locations of a region"))

(defmethod get-locations ((region hash-table))
  (hash-keys region))
(defmethod get-locations ((region hash-region))
  (hash-keys (locations region)))

(defun add-to-region (region &rest points)
  (dolist (point points)
    (set-location point region t)))

;; FUNCTION TYPE DECLARATIONS
;; (declaim-ftypes
;;  (visualize-region (hash-table &optional integer integer) *)
;;  (carve-location (vector2 model) *)
;;  (carve-region (hash-table model) *)
;;  (merge-regions (&rest hash-table) hash-table)
;;  (room-region (vector2 integer integer) hash-table)
;;  (random-room-region (vector2 vector2 vector2 vector2) hash-table)
;;  (box-region (integer integer integer integer) hash-table)
;;  (box-region-points (vector2 vector2) hash-table))

;; write a driver for regionS

(defun visualize-region (region &optional (width 16) (height width))
  "prints a graphical representation of the region for debugging"
  (iter (for row from (- (floor height 2)) to (floor height 2))
    (iter (for column from (- (floor width 2)) to (floor width 2))
      (case (at-location (make-vector2 column row) region)
        ((:door) (write-char #\+))
        ((nil) (write-char #\ ))
        ((t) (write-char #\.))
        (otherwise (write-char #\?))))
    (write-line ""))
  region)

(defun carve-location (location model)
    (let ((x (get-x location))
          (y (get-y location))) 
      (setf (aref (get-floor-tiles model) x y) t)))

(defun carve-region (region model)
  (iter (for location in (get-locations region))
    (carve-location location model)))

(defun merge-regions (&rest regions)
    (let ((merged (make-hash-region)))
      (dolist (region regions)
        (iter (for location in (get-locations region))
          ;;unless (at-location location merged)
          (set-location location merged (at-location location region))))
      merged))

(defun room-region (location w h)
  (let ((new-region (make-hash-region)))
    (dotimes (x w)
      (dotimes (y h)
        (set-location (make-vector2
                       (+ x (get-x location))
                       (+ y (get-y location)))
                      new-region t)))
    new-region))

(defun random-room-region (min-position max-position min-dims max-dims)
  (let ((x (+ (get-x min-position) (random (get-x max-position))))
        (y (+ (get-y min-position) (random (get-y max-position))))
        (width (+ (get-x min-dims) (random (get-x max-dims))))
        (height (+ (get-y min-dims) (random (get-y max-dims)))))
    (room-region (make-vector2 x y) width height)))

(defun assemble-dungeon (num-rooms size)
  (let ((rooms (list))
        (hallways (list))
        (final-region (make-hash-region)))
    (dotimes (_ num-rooms)
      (push (random-room-region
             (make-vector2 0 0)
             (make-vector2 size (floor size 2))
             (make-vector2 3 3)
             (make-vector2 10 10))
            rooms))
    (dolist (room-1 rooms)
      (dolist (room-2 rooms)
        (when (> 15 (random 100)) (push (make-connecting-regions-hallway room-1 room-2) hallways))))
    (dolist (room rooms)
      (dolist (hallway hallways)
        (setf final-region (merge-regions final-region room hallway))))
    final-region))

(defun box-region (x1 y1 x2 y2)
  "defines a region from any 2 points"
  (let ((new-region (make-hash-region)))
    (dolist (x (between x1 x2))
      (dolist (y (between y1 y2))
        (set-location (make-vector2 x y) new-region t)))
    new-region))

(defun box-region-points (location1 location2)
  "defines a region from any 2 points, formatted as vector2's"
  (let ((x1 (get-x location1))
        (x2 (get-x location2))
        (y1 (get-y location1))
        (y2 (get-y location2)))
    (box-region x1 y1 x2 y2)))

(defun path-region (starting-location steps
                    &key (region-to-merge (make-hash-region)) (initial-door nil) (ending-door nil))
  "usage: (path-region (make-vector2 10 10) '((:h 10) (:v 5)))"
  (let ((current-step (first steps)))
    (if current-step
        (let* ((orientation (first current-step))
               (distance (second current-step))
               (new-location (ccase orientation
                                ((:horizontal :h) (add-x starting-location distance))
                                ((:vertical :v) (add-y starting-location distance))))
               (new-region (box-region-points starting-location new-location)))
          (when initial-door (progn (print (at-location starting-location new-region))
                                    (set-location starting-location new-region :door)))
          (unless (or (not ending-door) (rest steps)) (set-location new-location new-region :door))
          (path-region new-location (rest steps)
                       :region-to-merge (merge-regions new-region region-to-merge)
                       :initial-door nil))
        region-to-merge)))

;; change to do multiple twists/turns?
(defun make-connecting-hallway (location1 location2)
  "Make a nice hallway region between two locations (vector2s)"
  (let ((dx (- (get-x location2) (get-x location1)))
        (dy (- (get-y location2) (get-y location1))))
    (if (eql (random 2) 0)
        (path-region location1 `((:h ,dx) (:v ,dy)))
        (path-region location1 `((:v ,dy) (:h ,dx))))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun make-connecting-regions-hallway (region-one region-two)
  (let* ((r1-list (get-locations region-one))
         (r2-list (get-locations region-two))
         (location1 (random-element r1-list))
         (location2 (random-element r2-list)))
    (make-connecting-hallway location1 location2)))

(defun test-locations-hallway ()
  (dotimes (_ 3) (visualize-region (let* ((location1 (make-vector2 -1 -1))
                                          (location2 (make-vector2 1 1))
                                          (hallway (make-connecting-hallway location1 location2)))
                                     (merge-regions hallway (point-region 0 0))))))

(defun test-regions-hallway ()
  (dotimes (_ 3) (visualize-region (let* ((room1 (box-region -5 -5 -1 -1))
                                          (room2 (box-region 3 3 7 7))
                                          (hallway (make-connecting-regions-hallway room1 room2)))
                                     (merge-regions room1 room2 hallway)) 30 20)))

(defun point-region (x y)
  (make-hash-region (make-vector2 x y)))
