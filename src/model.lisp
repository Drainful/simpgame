(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :iterate :silent t)
  (use-package 'iterate))

(defclass model ()
  ((player :type player
           :initarg :player
           :reader get-player)
   (floor-tiles :type array
                :initform (make-array (list +world-size+ +world-size+) :initial-element nil)
                :reader get-floor-tiles)))

;; (defclass layout-builder
;;     ())

(defun model-init (player)
  (defparameter *model* (make-instance 'model :player player))
  (let ((x1 10)
        (x2 60)
        (y1 10)
        (y2 25))
    (iter (for x from x1 below x2)
      (iter (for y from y1 below y2)
        (setf (aref (get-floor-tiles *model*) x y) t)))))

(defun between (x y)
  (if (< x y)
      (iter (for i from x to y)
        (collect i))
      (iter (for i from y to x)
        (collect i))))

(eval-when (:compile-toplevel :load-toplevel :execute)
           (sb-ext:define-hash-table-test object-equal-p object-hash))

;;(deftype region hash-table :test `object-equal-p)

(defun make-region (&rest points)
  (let ((region (make-hash-table :test 'object-equal-p)))
    (dolist (point points)
      (setf (gethash point region) t))
    region))

;; FUNCTION TYPE DECLARATIONS
(declaim-ftypes
 (visualize-region (hash-table &optional integer integer) *)
 (carve-location (vector2 model) nil)
 (carve-region (hash-table model) nil)
 (merge-regions (&rest hash-table) hash-table)
 (room-region (vector2 integer integer) hash-table)
 (box-region (integer integer integer integer) hash-table)
 (box-region-points (vector2 vector2) hash-table))

;; write a driver for region

(defun visualize-region (region &optional (width 16) (height width))
  "prints a graphical representation of the region for debugging"
  (iter (for row from (- (floor height 2)) to (floor height 2))
    (iter (for column from (- (floor width 2)) to (floor width 2))
      (case (gethash (make-vector2 column row) region)
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
  (iter (for (location _) in-hashtable region)
    (carve-location location model)))

(defun merge-regions (&rest regions) 
    (let ((merged (make-region)))
      (dolist (region regions)
        (iter (for (location _) in-hashtable region)
          ;;unless (gethash location merged)
          (setf (gethash location merged) (gethash location region))))
      merged))

(defun room-region (location w h)
  (let ((new-region (make-region)))
    (dotimes (x w)
      (dotimes (y h)
        (setf (gethash
               (make-vector2
                (+ x (get-x location))
                (+ y (get-y location)))
               new-region) t)))
    new-region))

(defun box-region (x1 y1 x2 y2)
  "defines a region from any 2 points"
  (let ((new-region (make-region)))
    (dolist (x (between x1 x2))
      (dolist (y (between y1 y2))
        (setf (gethash (make-vector2 x y) new-region) t)))
    new-region))

(defun box-region-points (location1 location2)
  "defines a region from any 2 points, formatted as vector2's"
  (let ((x1 (get-x location1))
        (x2 (get-x location2))
        (y1 (get-y location1))
        (y2 (get-y location2)))
    (box-region x1 y1 x2 y2)))

(defun path-region (starting-location steps
                    &key (region-to-merge (make-region)) (initial-door t) (ending-door t))
  "usage: (path-region (make-vector2 10 10) '((:h 10) (:v 5)))"
  (let ((current-step (first steps)))
    (if current-step
        (let* ((orientation (first current-step))
               (distance (second current-step))
               (new-location (ccase orientation
                                ((:horizontal :h) (add-x starting-location distance))
                                ((:vertical :v) (add-y starting-location distance))))
               (new-region (box-region-points starting-location new-location)))
          (when initial-door (progn (print (gethash starting-location new-region)) (setf (gethash starting-location new-region) :door)))
          (unless (or (not ending-door) (rest steps)) (setf (gethash new-location new-region) :door))
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

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun random-element (list)
  (nth (random (length list)) list))

(defun make-connecting-regions-hallway (region-one region-two)
  (let* ((r1-list (hash-keys region-one))
         (r2-list (hash-keys region-two))
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
  (make-region (make-vector2 x y)))
