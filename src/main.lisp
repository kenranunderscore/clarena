(in-package :cluarena)

(defconstant +max-head-turn-rate+ 8.0)
(defconstant +player-radius+ 25.0)
(defconstant +width+ 1600)
(defconstant +height+ 1200)
(defconstant +player-vision-color+ (raylib:make-rgba 20 70 200 70))
(defconstant +player-vision-border-color+ (raylib:make-rgba 50 10 250 255))
(defconstant +bg-color+ (raylib:make-rgba 10 10 10 255))

(defclass event-manager ()
  ((current-events
    :initform nil
    :accessor current-events)))

(defclass rendering ()
  ((color
    :initarg :color
    :accessor color)))

(defmethod component-type ((component rendering))
  :rendering)

(defclass movable ()
  ((current-position
    :initarg :pos
    :accessor current-position)
   (next-position
    :initform nil
    :accessor next-position)))

(defmethod component-type ((component movable))
  :movable)

(defclass player-intent ()
  ((distance
    :initform 0
    :accessor intent-distance)
   (turn-angle
    :initform 0
    :accessor intent-turn-angle)
   (attack-angle
    :initform 0
    :accessor intent-attack-angle)
   (attack
    :initform 0
    :accessor intent-attack)
   (head-angle
    :initform 0
    :accessor intent-head-angle)))

(defun make-default-intent ()
  (make-instance 'player-intent))

(defclass lua-player ()
  ((lua-state
    :initarg :lua-state
    :accessor lua-state)
   (position
    :initarg :position
    :accessor player-position)
   (next-position
    :initform nil
    :accessor next-player-position)
   (head-heading
    :initform 0
    :accessor head-heading)
   (intent
    :initform (make-default-intent)
    :accessor intent)
   (next-intent
    :initform (make-default-intent)
    :accessor next-intent)))

(defmethod component-type ((component lua-player))
  :lua-player)

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
   (round x)
   (round y)
   (round (+ x (* length (sin angle))))
   (round (- y (* length (cos angle))))
   color))

(defun render-player-vision (px py heading)
  (draw-line-in-direction
   px py heading
   (* 2 +player-radius+)
   +player-vision-border-color+))

(defun render-players (ecs)
  (let ((tups (find-components ecs :rendering :lua-player)))
    (loop
      for tup in tups
      do (progn
           (let* ((player (cadr tup))
                  (heading (head-heading player))
                  (p (player-position player))
                  (px (car p))
                  (py (cadr p)))
             (render-player-vision px py heading)
             (raylib:draw-circle
              (round px)
              (round py)
              +player-radius+
              (color (car tup))))))))

(defun run-render-system (ecs)
  (render-players ecs))

(defun valid-position? (p)
  (when p
    (let ((x (car p))
          (y (cadr p)))
      (and (>= x +player-radius+)
           (<= x (- +width+ +player-radius+))
           (>= y +player-radius+)
           (<= y (- +height+ +player-radius+))))))

(defun run-player-movement-system (ecs)
  (let ((tups (find-components ecs :lua-player)))
    (loop
      for tup in tups
      do (progn
           (let* ((player (car tup))
                  (p (player-position player))
                  (next-p (list
                           (+ (car p)
                              (intent-distance (intent player)))
                           (cadr p)))
                  (next-head-heading (+ (head-heading player)
                                        (intent-head-angle (intent player)))))
             (setf (head-heading player) next-head-heading)
             (if (valid-position? next-p)
                 (setf (next-player-position player) next-p)
                 (setf (next-player-position player) p)))))))

(defun run-player-collision-system (ecs)
  (let ((all (find-components ecs :lua-player)))
    (loop
      for tup in all
      do (let ((player (car tup)))
           (if (some
                (lambda (pc)
                  (players-collide? +player-radius+
                                    (next-player-position player)
                                    (next-player-position (car pc))))
                (remove tup all :test #'eq))
               (progn
                 (setf (next-player-position player) (player-position player))
                 (setf (next-intent player) (intent player)))
               (progn
                 (setf (player-position player) (next-player-position player))
                 (setf (intent player) (next-intent player))))))))

(defun run-head-movement-system (ecs)
  (let ((tups (find-components ecs :head)))
    (loop
      for tup in tups
      do (progn
           (let* ((comp (car tup)))
             (when (next-head-heading comp)
               (setf (head-heading comp)
                     (next-head-heading comp))))))))

(defun dist-sqr (p q)
  (+ (expt (- (car p) (car q))
           2.0)
     (expt (- (cadr p) (cadr q))
           2.0)))

(defun distance (p q)
  (sqrt (dist-sqr p q)))

(defun players-collide? (radius p q)
  (<= (distance p q) (* 2 radius)))

(defun lua-getx (player)
  (lambda (ls)
    (lua::pushnumber
     ls
     (coerce (car (player-position player)) 'double-float))
    1))

(defun lua-gety (player)
  (lambda (ls)
    (lua::pushnumber
     ls
     (coerce (cadr (player-position player)) 'double-float))
    1))

(defun lua-create-tagged-table (ls tag)
  (lua::newtable ls)
  (lua::pushstring ls tag)
  (lua::setfield ls -2 "tag"))

(defun lua-turn-body-part (ls tag)
  (let ((angle (lua::tonumber ls -1)))
    (lua-create-tagged-table ls tag)
    (lua::pushnumber ls angle)
    (lua::setfield ls -2 "angle")
    1))

(defun lua-turn (ls)
  (lua-turn-body-part ls "turn_right"))

(defun lua-turn-head (ls)
  (lua-turn-body-part ls "turn_head_right"))

(defun lua-move (ls)
  (let ((distance (lua::tonumber ls -1)))
    (lua-create-tagged-table ls "move")
    (lua::pushnumber ls distance)
    (lua::setfield ls -2 "distance")
    1))

(defun create-lua-player (ecs file-path position)
  (let* ((ls (lua::newstate))
         (lua-player (make-instance
                      'lua-player
                      :lua-state ls
                      :position position)))
    (lua::openlibs ls)
    (lua::dofile ls file-path)
    (lua::create-module
        ls "me"
      ("x" (lua-getx lua-player))
      ("y" (lua-gety lua-player))
      ("move" #'lua-move)
      ("turn" #'lua-turn)
      ("turn_head" #'lua-turn-head))
    (new-entity
     ecs
     lua-player
     (make-instance 'rendering :color :red)
     lua-player)
    lua-player))

(defun read-n-player-commands (ls n)
  (labels
      ((rec (i acc)
         (lua::pushinteger ls i)
         (lua::gettable ls -2)
         (lua::getfield ls -1 "tag")
         (let* ((tag (lua::tostring ls -1))
                (cmd
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
                    ((equal tag "move")
                     (lua::pop ls 1)
                     (lua::getfield ls -1 "distance")
                     (let ((distance (lua::tonumber ls -1)))
                       (lua::pop ls 1)
                       (make-instance 'move-cmd :distance distance)))
                    (t (error (make-condition 'unknown-player-command :tag tag)))))
                (new-acc (cons cmd acc)))
           ;; pop the command
           (lua::pop ls 1)
           (if (= i n)
               new-acc
               (rec (1+ i) new-acc)))))
    (rec 1 nil)))

(defun read-player-commands (ls)
  (if (lua::istable ls -1)
      (let ((nresults (lua::rawlen ls -1)))
        (unless (zerop nresults)
          (read-n-player-commands ls nresults)))
      (error (make-condition 'player-command-could-not-be-read))))

(defun call-on-tick (ls tick)
  (lua::getfield ls 1 "on_tick")
  (lua::pushnumber ls (coerce tick 'double-float))
  (lua::pcall-ex ls 1 1 0)
  (let ((cmds (read-player-commands ls)))
    (mapc #'print-command cmds)
    ;; pop the list of commands
    (lua::pop ls 1)
    cmds))

(defun call-on-round-started (ls)
  (lua::getfield ls 1 "on_round_started")
  (if (lua::isnil ls -1)
      (progn
        (lua::pop ls 1)
        nil)
      (progn
        (lua::pcall-ex ls 0 1 0)
        (let ((cmds (read-player-commands ls)))
          (lua::pop ls 1)
          cmds))))

(defgeneric print-command (cmd)
  (:documentation "Print a debug representation of some player command."))

(defclass turn-cmd ()
  ((angle
    :initarg :angle
    :reader turn-angle)))

(defmethod print-command ((cmd turn-cmd))
  (format t "  turn    angle: ~d~%" (turn-angle cmd)))

(defclass turn-head-cmd ()
  ((angle
    :initarg :angle
    :reader turn-head-angle)))

(defmethod print-command ((cmd turn-head-cmd))
  (format t "  turn-head    angle: ~d~%" (turn-head-angle cmd)))

(defclass turn-arms-cmd ()
  ((angle
    :initarg :angle
    :reader turn-arms-angle)))

(defmethod print-command ((cmd turn-arms-cmd))
  (format t "  turn arms    angle: ~d~%" (turn-angle cmd)))

(defclass move-cmd ()
  ((distance
    :initarg :distance
    :reader move-distance)))

(defmethod print-command ((cmd move-cmd))
  (format t "  move    distance: ~d~%" (move-distance cmd)))

(defclass attack-cmd ()
  ())

(defmethod print-command ((cmd attack-cmd))
  (format t "  attack~%"))

(defun update-intents (intent cmds)
  (mapc (lambda (cmd)
          (update-intent intent cmd))
        cmds))

(defgeneric update-intent (intent cmd)
  (:documentation "Given a player command CMD, update the current intent accordingly."))

(defmethod update-intent ((intent player-intent) (cmd turn-head-cmd))
  (setf (intent-head-angle intent)
        (turn-head-angle cmd)))

(defmethod update-intent ((intent player-intent) (cmd turn-cmd))
  nil)

(defmethod update-intent ((intent player-intent) (cmd turn-head-cmd))
  (setf (intent-head-angle intent)
        (turn-head-angle cmd)))

(defmethod update-intent ((intent player-intent) (cmd turn-arms-cmd))
  nil)

(defmethod update-intent ((intent player-intent) (cmd attack-cmd))
  nil)

(defmethod update-intent ((intent player-intent) (cmd move-cmd))
  (setf (intent-distance intent)
        (move-distance cmd)))

(defun run-lua-control-system (ecs event-manager)
  (let ((events (current-events event-manager)))
    (when events
      (let ((player-events (game-events->player-events events))
            (cts (find-components ecs :lua-player)))
        (dolist (c cts)
          (let* ((player (car c))
                 (intent (intent player))
                 (ls (lua-state player)))
            (lua::assert-stack-size ls 1)
            (let ((cmds
                    (mapcan
                     (lambda (e)
                       (let ((event-type (car e)))
                         (cond
                           ((eq event-type :tick)
                            (call-on-tick ls (cadr e)))
                           ((eq event-type :round-started)
                            (call-on-round-started ls))
                           (t (format t "unhandled player event: ~a~%" event-type)))))
                     player-events)))
              (update-intents intent cmds))))))))

(define-condition unknown-player-command (error)
  ((tag
    :initarg :tag
    :reader tag)))

(define-condition player-command-could-not-be-read (error)
  ())

(defclass game-state ()
  ((current-round
    :initform 1
    :accessor current-round)
   (current-tick
    :initform 0
    :accessor current-tick)))

(defun tick-events (state)
  (let* ((tick (current-tick state))
         (evts (list (list :tick tick))))
    (if (zerop tick)
        (cons (list :round-started (current-round state))
              evts)
        evts)))

(defun determine-initial-tick-events (state)
  (concatenate
   'list
   (tick-events state)))

(defmethod next-tick ((event-manager event-manager) (state game-state))
  (setf (current-events event-manager)
        (determine-initial-tick-events state)))

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
  (incf (current-tick state)))

(defun run-intent-application-system (ecs)
  (let ((cts (find-components ecs :lua-player :head :movable)))
    (dolist (c cts)
      (let* ((intent (intent (car c)))
             (head (cadr c))
             (movable (caddr c)))
        (setf (next-position movable)
              (list (+ (car (current-position movable))
                       (intent-distance intent))
                    (cadr (current-position movable))))
        (setf (next-head-heading head)
              (+ (head-heading head)
                 (intent-head-angle intent)))))))

(defun run-all-systems (ecs state event-manager)
  (run-render-system ecs)
  (next-tick event-manager state)
  ;; FIXME: both at once?
  (run-player-movement-system ecs)
  (run-player-collision-system ecs)
  (run-head-movement-system ecs)
  (run-lua-control-system ecs event-manager)
  (advance-game-state state))

(defun main ()
  (let* ((state (make-instance 'game-state))
         (event-manager (make-instance 'event-manager))
         (ecs (make-instance 'ecs))
         (player-1 (create-lua-player ecs "foo.lua" '(100 250)))
         (player-2 (create-lua-player ecs "bar.lua" '(500 250))))
    (raylib:with-window (+width+ +height+ "HALLO")
      (raylib:set-target-fps 60)
      (loop
        until (raylib:window-should-close)
        do (raylib:with-drawing
             (raylib:clear-background +bg-color+)
             (raylib:draw-fps 5 5)
             (raylib:draw-text (format nil "Tick: ~a" (current-tick state))
                               5 (- +height+ 20) 20 :green)
             (run-all-systems ecs state event-manager))))
    (lua::close (lua-state player-1))
    (lua::close (lua-state player-2))))
