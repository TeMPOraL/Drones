;;; Drones.

;;; Ensure we have Quicklisp running
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load libs we need
(ql:quickload 'lispbuilder-sdl)

(defparameter *keybindings* '((:sdl-key-r :sdl-key-f)
                              (:sdl-key-u :sdl-key-j)
                              (:sdl-key-e :sdl-key-d)
                              (:sdl-key-i :sdl-key-k)
                              (:sdl-key-w :sdl-key-s)
                              (:sdl-key-o :sdl-key-l)
                              (:sdl-key-q :sdl-key-a)))


;;; World
(defparameter *available-bindings* *keybindings*)

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
    ;; update position
    (setf position nil
          ;; fixme todo finish it
          )
    ))

(defmethod draw ((a-drone drone)))