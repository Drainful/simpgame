(ql:quickload :slynk)
;;(asdf:load-system :slynk)
(slynk:create-server :port 4005)
;; (defvar *console-io* *terminal-io*)
;; (ql:quickload '(:swank) :silent t)
;; (swank:create-server :port 4005 :style :fd-handler :dont-close t)
;; ;;(loop (sleep 1))