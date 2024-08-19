(in-package :cluarena)

(defclass rendering ()
  ((color
    :initarg :color
    :accessor color)))

(defmethod component-type ((component rendering))
  :rendering)

(defclass movable ()
  ((pos
    :initarg :pos
    :accessor pos)
   (next-pos
    :initform nil
    :accessor next-pos)
   (velocity
    :initarg :velocity
    :initform 0
    :accessor velocity)))

(defmethod component-type ((component movable))
  :movable)

(defclass player-intent ()
  ((dist
    :initform 0
    :accessor dist)))

(defun make-default-intent ()
  (make-instance 'player-intent))

(defclass lua-controlled ()
  ((lua-state
    :initarg :lua-state
    :accessor lua-state)
   (intent
    :initform (make-default-intent)
    :accessor intent)))

(defmethod component-type ((component lua-controlled))
  :lua-controlled)

(defclass collidable ()
  ((entity-id
    :initarg :entity-id
    :reader :entity-id)))

(defmethod component-type ((component collidable))
  :collidable)

(defun make-collidable (e)
  (let ((comp (make-instance 'collidable :entity-id (id e))))
    (add-component comp e)))

(defparameter *player-radius* 25.0)
(defparameter *width* 800)
(defparameter *height* 600)

(defun run-render-system (ecs)
  (let ((tups (find-components ecs :movable :rendering)))
    (loop
      for tup in tups
      do (progn
           (let* ((comp (car tup))
                  (p (pos comp)))
             (raylib:draw-circle
              (car p)
              (cadr p)
              *player-radius*
              (color (cadr tup))))))))

(defun valid-position? (p)
  (let ((x (car p))
        (y (cadr p)))
    (and (>= x *player-radius*)
         (<= x (- *width* *player-radius*))
         (>= y *player-radius*)
         (<= y (- *height* *player-radius*)))))

(defun run-movement-system (ecs)
  (let ((tups (find-components ecs :movable)))
    (loop
      for tup in tups
      do (progn
           (let* ((comp (car tup))
                  (p (pos comp))
                  (v (velocity comp))
                  (next (list (+ (car p) v)
                              (cadr p))))
             (if (valid-position? next)
                 (setf (next-pos comp) next)
                 (setf (next-pos comp) p)))))))

(defun dist-sqr (p q)
  (+ (expt (- (car p) (car q))
           2.0)
     (expt (- (cadr p) (cadr q))
           2.0)))

(defun distance (p q)
  (sqrt (dist-sqr p q)))

(defun players-collide? (radius p q)
  (<= (distance p q) (* 2 radius)))

(defun run-collision-system (ecs)
  (let ((all (find-components ecs :movable :collidable)))
    (loop
      for tup in all
      do (let ((p (car tup)))
           (if (some
                (lambda (pc)
                  (players-collide? *player-radius* (next-pos p) (next-pos (car pc))))
                (remove tup all :test #'eq))
               (setf (next-pos p) (pos p))
               (setf (pos p) (next-pos p)))))))

(defun lua-getx (movable)
  (lambda (l)
    (lua::pushnumber l (coerce (car (pos movable)) 'double-float))
    1))

(defun lua-gety (movable)
  (lambda (l)
    (lua::pushnumber l (coerce (cadr (pos movable)) 'double-float))
    1))

(defun lua-turn (ls)
  (let ((angle (lua::tonumber ls -1)))
    (lua::newtable ls)
    (lua::pushstring ls "turn_right")
    (lua::setfield ls -2 "tag")
    (lua::pushnumber ls angle)
    (lua::setfield ls -2 "angle")
    1))

(defun create-lua-player (ecs file-path movable)
  (let* ((ls (lua::newstate))
         (comp (make-instance 'lua-controlled :lua-state ls)))
    (lua::openlibs ls)
    (lua::dofile ls file-path)
    (lua::create-module
     ls "me"
     ("x" (lua-getx movable))
     ("y" (lua-gety movable))
     ("turn" #'lua-turn))
    (make-collidable
     (new-entity
      ecs
      comp
      (make-instance 'rendering :color :red)
      movable))
    comp))

(defun run-lua-control-system (ecs tick)
  (let ((cts (find-components ecs :lua-controlled)))
    (dolist (c cts)
      (let ((ls (lua-state (car c))))
        (lua::assert-stack-size ls 1)
        (lua::getfield ls 1 "on_tick")
        (lua::pushnumber ls (coerce tick 'double-float))
        (lua::pcall-ex ls 1 1 0)
        (read-player-command ls)
        (lua::pop ls 1)))))

(defclass turn-cmd ()
  ((angle
    :initarg :angle
    :reader turn-angle)))

(defun read-player-command (ls)
  (if (lua::istable ls -1)
      (progn
        (lua::getfield ls -1 "tag")
        (let ((tag (lua::tostring ls -1)))
          (cond
            ((equal tag "turn_right")
             (lua::pop ls 1)
             (lua::getfield ls -1 "angle")
             (let ((angle (lua::tonumber ls -1)))
               (lua::pop ls 1)
               (make-instance 'turn-cmd :angle angle)))
            (t (format t "Unexpected command tag: ~a~%" tag)))))
      (format t "Unexpected object on the stack when reading player command~%")))

(defun main ()
  (let* ((ecs (make-instance 'ecs))
         (player-1 (create-lua-player ecs "foo.lua" (make-instance 'movable :pos '(100 250) :velocity 2)))
         (player-2 (create-lua-player ecs "foo.lua" (make-instance 'movable :pos '(500 250) :velocity -1))))
    (raylib:with-window (*width* *height* "HALLO")
      (raylib:set-target-fps 60)
      (loop
        until (raylib:window-should-close)
        for tick from 1
        do (raylib:with-drawing
             (raylib:clear-background :darkgray)
             (raylib:draw-fps 20 20)
             (run-render-system ecs)
             (run-lua-control-system ecs tick)
             (run-movement-system ecs)
             (run-collision-system ecs))))
    (lua::close (lua-state player-1))
    (lua::close (lua-state player-2))))
