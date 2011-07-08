;;; Drones.

;;; Ensure we have Quicklisp running
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load libs we need
(ql:quickload 'lispbuilder-sdl)

(load "math.lisp")
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

(defconstant *drone-angular-velocity* (* (/ pi 2) 3))
(defconstant *drone-speed* 150)

(defconstant *drone-launch-vector* #(0 -1))

(defconstant *jumper-position* (make-vector-2d (round (/ +screen-height+ 2))
                                               (- +screen-height+ 42)))

(defparameter *drones* '())             ; list of drones in game

;;; 
(defun dt-s ()
  (float (/ (sdl:dt) 1000)))

;;; Drones
(defclass drone ()
  ((position :initarg :position)
   (velocity :initarg :velocity)
   (keybindings :initarg :keybindings)))

(defun make-drone ()
  "Spawn a new drone, giving (TODO) its position and velocity."
  (if (null *available-bindings*)
      nil
      (prog1 (make-instance 'drone
                            :keybindings (car *available-bindings*)
                            :position (copy-seq *jumper-position*)
                            :velocity (scaled-vector *drone-launch-vector* *drone-speed*))
        (pop *available-bindings*))))

(defun destroy-drone (drone)
  (format t "Drone destroyed~%")
  (setf *available-bindings* (cons (slot-value drone 'keybindings)
                                   *available-bindings*)))

(defun spawn-a-new-drone ()
  (format t "Spawning a new drone; list: ~a~%" *drones*)
  (if *available-bindings*
      (push (make-drone)
            *drones*)))

;; Update and draw methods

(defmethod update ((a-drone drone))
  (with-slots (position velocity keybindings) a-drone
    ;; rotate vectors based on keypresses
    (setf velocity (rotated-vector-2d velocity
                                      (cond ((sdl:key-down-p (car keybindings))
                                             (* *drone-angular-velocity* (dt-s) -1))
                                            ((sdl:key-down-p (cadr keybindings))
                                             (* *drone-angular-velocity* (dt-s) ))
                                            (t 0))))
    ;; update position
    (setf position (add-vectors (scaled-vector velocity (dt-s))
                                position))))

(defmethod draw ((a-drone drone))
  (with-slots (position velocity) a-drone
    (sdl:draw-circle-* (round (elt position 0)) (round (elt position 1)) 5 :color sdl:*yellow*)
    (let ((line-end (add-vectors position (scaled-vector (normalized-vector velocity)
                                                         5))))
      (sdl:draw-line-* (round (elt position 0))
                       (round (elt position 1))
                       (round (elt line-end 0))
                       (round (elt line-end 1))
                       :color sdl:*yellow*))))

;;; Game stuff
(defun update-game ()
  (process-input)
  ;; update world
  (mapc #'update *drones*))

(defun render-game ()
  (sdl:clear-display sdl:*black*)
  (mapc #'draw *drones*))

;;; FIXME this function looks like crap.
(let ((space-pressed nil))
  (defun process-input ()
    "Process input not directly related to controlling the drones."
    ;; Process keys
    ;; Space = spawn new drone
    (let ((current-space-state (sdl:key-down-p :sdl-key-space)))
      (if (and current-space-state (not space-pressed))
          (spawn-a-new-drone))
      (setf space-pressed current-space-state))))



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
       (sdl:with-timestep ()
                          (update-game))
       (render-game)
       (sdl:update-display))

      (:quit-event ()
                   (format t "Quitting")
                   t))))