(ql:quickload :iterate :silent t)
(use-package 'iterate)

(defclass model ()
  ((player :type player
           :initarg :player
           :reader get-player)
   (floor-tiles :type array
                :initform (make-array (list +world-size+ +world-size+) :initial-element nil)
                :reader get-floor-tiles)))

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
      (iter (for i from x below y)
        (collect i))
      (iter (for i from y below x)
        (collect i))))

(defun make-region ()
  (make-hash-table :test 'equalp))

;; FUNCTION TYPE DECLARATIONS
(declaim-ftypes
 (visualize-region (hash-table) nil)
 (carve-location (vector2 model) nil)
 (carve-region (hash-table model) nil)
 (merge-regions (&rest hash-table) hash-table)
 (room-region (vector2 integer integer) hash-table)
 (box-region (integer integer integer integer) hash-table)
 (box-region-points (vector2 vector2) hash-table))

;; (defun visualize-region (region)
;;   "prints a graphical representation of the region for debugging"
;;   (iter (for (location _) in-hashtable region)
;;         (carve-location location model)))

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
              (unless (gethash 'location merged)
                (setf (gethash 'location merged) t))))
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
    (box-region x1 x2 y1 y2)))

(defun path-region (starting-location steps
                    &key (merge-region (make-region)))
  "usage: (path-region (make-vector2 10 10) '((:h 10) (:v 5)))"
  (let ((step (first steps)))
    (if step
        (let* ((orientation (car step))
               (distance (cdr step))
               (new-location (apply (case-orientation orientation :if-h #'add-x
                                                                  :if-v #'add-y) starting-location distance))
               (new-region (box-region-points starting-location new-location)))
          (path-region new-location (rest steps)
                       :merge-region (merge-regions merge-region new-region)))
        merge-region)))

(defun case-orientation (orientation &key if-h if-v)
    (ccase orientation
       ((:horizontal :h) if-h)
       ((:vertical :v) if-v)))

;; (defun path-region (starting-location &rest steps)
;;   (let ((region (make-region))
;;         (current-location starting-location)
;;         (next-location ))
;;     (dolist (step steps)
;;       ;; assign current location to the result of the step,
;;       ;; and as a side effect add every location along the step to the region
;;       (setf current-location
;;             (ccase (first step)
;;               ;; if horizontal, make box starting 
;;               ((:horizontal :h) (progn (merge-regions region )))
;;               ((:vertical :v)))))
;;     region))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun random-element (list)
  (nth (random (length list)) list))

;; (defun ordering (val1 val2)
;;   (if (< x1 x2)
;;       :less-than
;;       (if (> x1 x2)
;;           :greater-than
;;           :equal)))

;; (defun make-connecting-hallway (location-one location-two)
;;   (let* ((x1 (get-x location-one))
;;          (x2 (get-x location-two))
;;          (y1 (get-y location-one))
;;          (y2 (get-y location-two))
;;          (horizontal-ordering (ordering x1 x2))
;;          (vertical-ordering (ordering y1 y2)))
;;     (ccase `(,horizontal-ordering ,vertical-ordering) ((`(,:less-than ,:less-than))
;;                                                        ('(,:less-than ,:greater-than))
;;                                                        ('(,:less-than ,:equal))))))

;; (defun make-connecting-regions-hallway (region-one region-two)
;;   (let* ((r1-list (hash-keys region-one))
;;          (r2-list (hash-keys region-two))
;;          (location-one (random-element r1-list))
;;          (location-two (random-element r2-list))
;;          )))
