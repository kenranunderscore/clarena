(in-package :cluarena)

(defconstant +max-head-turn-rate+ 8.0)
(defconstant +player-radius+ 25.0)
(defconstant +width+ 1600)
(defconstant +height+ 1200)
(defconstant +player-vision-color+ (raylib:make-rgba 20 70 200 70))
(defconstant +player-vision-border-color+ (raylib:make-rgba 20 70 200 255))
(defconstant +bg-color+ (raylib:make-rgba 10 10 10 255))

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

(defclass head ()
  ((head-heading
    :initarg :heading
    :accessor head-heading)
   (next-head-heading
    :initform nil
    :accessor next-head-heading)))

(defmethod component-type ((component head))
  :head)

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

(defun draw-line-in-direction (x y angle length color)
  (raylib:draw-line
   x y
   (round (+ x (* length (sin angle))))
   (round (- y (* length (cos angle))))
   color))

(defun render-player-vision (px py heading)
  (draw-line-in-direction
   px py heading
   (* 2 +player-radius+)
   +player-vision-border-color+))

(defun render-players (ecs)
  (let ((tups (find-components ecs :rendering :movable :head)))
    (loop
      for tup in tups
      do (progn
           (let* ((p (pos (cadr tup)))
                  (px (car p))
                  (py (cadr p))
                  (heading (head-heading (caddr tup))))
             (render-player-vision px py heading)
             (raylib:draw-circle px py +player-radius+ (color (car tup))))))))

(defun run-render-system (ecs)
  (render-players ecs))

(defun valid-position? (p)
  (let ((x (car p))
        (y (cadr p)))
    (and (>= x +player-radius+)
         (<= x (- +width+ +player-radius+))
         (>= y +player-radius+)
         (<= y (- +height+ +player-radius+)))))

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

(defun run-head-movement-system (ecs)
  (let ((tups (find-components ecs :head)))
    (loop
      for tup in tups
      do (progn
           (let* ((comp (car tup)))
             (setf (head-heading comp)
                   (next-head-heading comp)))))))

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
                  (players-collide? +player-radius+ (next-pos p) (next-pos (car pc))))
                (remove tup all :test #'eq))
               (setf (next-pos p) (pos p))
               (setf (pos p) (next-pos p)))))))

(defun lua-getx (movable)
  (lambda (ls)
    (lua::pushnumber ls (coerce (car (pos movable)) 'double-float))
    1))

(defun lua-gety (movable)
  (lambda (ls)
    (lua::pushnumber ls (coerce (cadr (pos movable)) 'double-float))
    1))

(defun lua-turn-body-part (ls tag)
  (let ((angle (lua::tonumber ls -1)))
    (lua::newtable ls)
    (lua::pushstring ls tag)
    (lua::setfield ls -2 "tag")
    (lua::pushnumber ls angle)
    (lua::setfield ls -2 "angle")
    1))

(defun lua-turn (ls)
  (lua-turn-body-part ls "turn_right"))

(defun lua-turn-head (ls)
  (lua-turn-body-part ls "turn_head_right"))

(defun create-lua-player (ecs file-path movable)
  (let* ((ls (lua::newstate))
         (lua-controlled (make-instance 'lua-controlled :lua-state ls))
         (head (make-instance 'head :heading 0.0)))
    (lua::openlibs ls)
    (lua::dofile ls file-path)
    (lua::create-module
        ls "me"
      ("x" (lua-getx movable))
      ("y" (lua-gety movable))
      ("turn" #'lua-turn)
      ("turn_head" #'lua-turn-head))
    (make-collidable
     (new-entity
      ecs
      lua-controlled
      head
      (make-instance 'rendering :color :red)
      movable))
    lua-controlled))

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
            ((equal tag "turn_head_right")
             (lua::pop ls 1)
             (lua::getfield ls -1 "angle")
             (let ((angle (lua::tonumber ls -1)))
               (lua::pop ls 1)
               (make-instance 'turn-head-cmd :angle angle)))
            (t (error (make-condition 'unknown-player-command :tag tag))))))
      (error (make-condition 'player-command-could-not-be-read))))

(defun call-on-tick (ls tick)
  (lua::getfield ls 1 "on_tick")
  (lua::pushnumber ls (coerce tick 'double-float))
  (lua::pcall-ex ls 1 1 0)
  (let ((cmd (read-player-command ls)))
    (lua::pop ls 1)
    cmd))

(defun update-intents (head cmds)
  (mapc (lambda (cmd)
          (update-intent head cmd))
        cmds))

(defgeneric update-intent (head cmd)
  (:documentation "Given a player command CMD, update the current intent accordingly."))

(defmethod update-intent ((head head) (cmd turn-head-cmd))
  (setf (next-head-heading head)
        (+ (head-heading head)
           (head-turn-angle cmd))))

(defmethod update-intent ((head head) (cmd turn-cmd))
  nil)

(defun run-lua-control-system (ecs player-events)
  (let ((cts (find-components ecs :lua-controlled :head)))
    (dolist (c cts)
      (let ((ls (lua-state (car c)))
            (head (cadr c)))
        (lua::assert-stack-size ls 1)
        (let ((cmds
                (mapcan
                 (lambda (e)
                   (let ((event-type (car e)))
                     (cond
                       ((eq event-type :tick)
                        (call-on-tick ls (cadr e)))
                       (t (format t "unhandled player event: ~a~%" event-type)))))
                 player-events)))
          (update-intent head cmds))))))

(defclass turn-cmd ()
  ((angle
    :initarg :angle
    :reader turn-angle)))

(defclass turn-head-cmd ()
  ((angle
    :initarg :angle
    :reader head-turn-angle)))

(define-condition unknown-player-command (error)
  ((tag
    :initarg :tag
    :reader tag)))

(define-condition player-command-could-not-be-read (error)
  ())

(defclass game-state ()
  ((round
    :initform 1
    :accessor current-round)
   (tick
    :initform 0
    :accessor tick)))

(defun tick-events (state)
  (let* ((tick (tick state))
         (evts (list (list :tick tick))))
    (if (zerop tick)
        (cons (list :round-started (current-round state))
              evts)
        evts)))

(defun determine-game-events (state)
  (concatenate
   'list
   (tick-events state)))

(defun game-events->player-events (game-events)
  (reduce
   (lambda (acc e)
     (let ((event-type (car e)))
       (cond
         ((eq event-type :tick)
          (cons e acc))
         ((eq event-type :round-started)
          (cons e acc))
         (t (format t "encountered unknown game event type: ~a~%" (car e))))))
   game-events
   :initial-value nil))

(defun advance-game-state (state)
  (incf (tick state)))

(defun run-all-systems (ecs state)
  (run-render-system ecs)
  (let* ((evts (determine-game-events state))
         (player-events (game-events->player-events evts)))
    (run-lua-control-system ecs player-events)
    (run-movement-system ecs)
    (run-head-movement-system ecs)
    (run-collision-system ecs)
    (advance-game-state state)))

(defun main ()
  (let* ((state (make-instance 'game-state))
         (ecs (make-instance 'ecs))
         (player-1 (create-lua-player ecs "foo.lua" (make-instance 'movable :pos '(100 250) :velocity 2)))
         (player-2 (create-lua-player ecs "foo.lua" (make-instance 'movable :pos '(500 250) :velocity -1))))
    (raylib:with-window (+width+ +height+ "HALLO")
      (raylib:set-target-fps 60)
      (loop
        until (raylib:window-should-close)
        do (raylib:with-drawing
             (raylib:clear-background +bg-color+)
             (raylib:draw-fps 20 20)
             (run-all-systems ecs state))))
    (lua::close (lua-state player-1))
    (lua::close (lua-state player-2))))
