;;; Drones.

;;; Ensure we have Quicklisp running
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load libs we need
(ql:quickload 'lispbuilder-sdl)

;;; Display constants
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +window-title+ "Drones")

;;; Simulation constants
(defparameter +fixed-dt+ (floor (/ 1000 30))) ; 30 steps / second
(defparameter +maximum-dt+ 500) ; maximum dt allowed - time step
                                ;will be trimmed to that value


(defparameter *keybindings* '((:sdl-key-r :sdl-key-f)
                              (:sdl-key-u :sdl-key-j)
                              (:sdl-key-e :sdl-key-d)
                              (:sdl-key-i :sdl-key-k)
                              (:sdl-key-w :sdl-key-s)
                              (:sdl-key-o :sdl-key-l)
                              (:sdl-key-q :sdl-key-a)))


;;; World
(defparameter *available-bindings* *keybindings*)

(defconstant *drone-angular-velocity* (/ pi 2))
(defconstant *drone-speed* 150)

;;; Drones
(defclass drone ()
  ((position)
   (velocity)
   (keybindings :initarg :keybindings)))

(defun make-drone ()
  "Spawn a new drone, giving (TODO) its position and velocity."
  (if (null *available-bindings*)
      nil
      (prog1 (make-instance 'drone :keybindings (car *available-bindings*))
        (setf *available-bindings* (cdr *available-bindings*)))))

(defun destroy-drone (drone)
  (setf *available-bindings* (cons (slot-value drone 'keybindings)
                                   *available-bindings*)))

;; Update and draw methods

(defmethod update ((a-drone drone))
  (with-slots (position velocity keybindings) a-drone
    ;; rotate vectors based on keypresses
    (setf velocity (rotated-vector-2d velocity
                                      (cond ((sdl:key-down-p (car keybindings))
                                             (* *drone-angular-velocity* (sdl:dt)))
                                            ((sdl:key-down-p (cdr keybindings))
                                             (* *drone-angular-velocity* (sdl:dt) -1))
                                            (t 0))))
    ;; update position
    (setf position (add-vectors (scaled-vector velocity (sdl:dt))
                                position))))

(defmethod draw ((a-drone drone)))



;;; Main window and stuff.

(defun run-game ()
  (sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO)
    (sdl:window +screen-width+ +screen-height+  ;screen resolution
                :title-caption +window-title+
                :fps (make-instance 'sdl:fps-timestep
                                    :max-dt +maximum-dt+ ; timestep upper bound
                                    :dt +fixed-dt+); fixed time step
                :video-driver "directx"
                :double-buffer t)
    (sdl:with-events ()
      (:idle
       ;; TODO run game
       (sdl:update-display))

      (:quit-event ()
                   (format t "Quitting")
                   t))))