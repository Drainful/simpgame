;; ;; CORE LOOP, update then view.
;; (defun map-state ()
;;   (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visibility nil)
;;     (event-case (scr event)
;;       (#\q (return-from event-case))
;;       (otherwise
;;        (update *model* event)
;;        (view *model* scr)))))
