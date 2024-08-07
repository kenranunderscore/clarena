(in-package :cluarena)

(defclass ecs ()
  ((highest-entity-id
    :initform 0
    :accessor highest-entity-id)
   (entities
    :initform nil
    :accessor entities)))

(defclass entity ()
  ((id
    :initarg :id
    :accessor id)
   (components
    :initform nil
    :accessor components)))

(defgeneric new-entity (obj &rest cts)
  (:documentation "Adds a new entity"))

(defmethod new-entity ((ecs ecs) &rest cts)
  (let* ((new-id (1+ (highest-entity-id ecs)))
         (new-entity (make-instance 'entity :id new-id))
         (entities (entities ecs)))
    (setf (highest-entity-id ecs) new-id)
    (setf (entities ecs) (cons new-entity entities))
    (mapc (lambda (comp) (add-component comp new-entity)) cts)
    new-entity))

(defun add-component (component entity)
  "Add a component to ENTITY."
  (setf (components entity)
        (cons component (components entity))))

(defgeneric component-type (component)
  (:documentation "Return the type of the component."))

(defun find-component (entity ctype)
  "Given an entity ENTITY, find its component of type CTYPE (or nil)."
  (find-if (lambda (x)
             (eq (component-type x)
                 ctype))
           (components entity)))

(defun find-components (ecs &rest cts)
  (let ((res nil))
    (loop
      for e in (entities ecs)
      do (let ((tup (mapcar
                     (lambda (ct) (find-component e ct))
                     cts)))
           (unless (some #'null tup)
             (setq res (cons tup res)))))
    res))
