(in-package :lua)

(defconstant +tnil+ 0)
(defconstant +tboolean+ 1)
(defconstant +tlightuserdata+ 2)
(defconstant +tnumber+ 3)
(defconstant +tstring+ 4)
(defconstant +ttable+ 5)
(defconstant +tfunction+ 6)
(defconstant +tuserdata+ 7)
(defconstant +tthread+ 8)

(defmacro call (ls nargs nresults)
  `(callk ,ls ,nargs ,nresults 0 (null-pointer)))

(define-condition lua-call-error (error)
  ((function-name
    :initarg :function-name
    :reader function-name)
   (error-msg
    :initarg :error-msg
    :reader error-msg)
   (error-code
    :initarg :error-code
    :reader error-code)))

(defmacro pcall (ls nargs nresults errfunc)
  `(pcallk ,ls ,nargs ,nresults ,errfunc 0 (null-pointer)))

(defmacro pcall-ex (ls nargs nresults errfunc)
  `(let ((res (pcall ,ls ,nargs ,nresults ,errfunc)))
     (unless (= 0 res)
       (let ((msg (tostring ,ls -1)))
         (error (make-condition 'lua-call-error
                                :function-name "pcall"
                                :error-msg msg
                                :error-code res))))))

(defmacro pushcfunction (ls f)
  `(pushcclosure ,ls ,f 0))

(defmacro loadfile (ls filename)
  `(loadfilex ,ls ,filename (null-pointer)))

(define-condition lua-dofile-failure (error)
  ((filename
    :initarg :filename
    :initform nil
    :reader filename)))

(defun dofile (ls filename)
  (let ((res (loadfile ls filename)))
    (if (= 0 res)
        (pcall ls 0 -1 0)
        (error (make-condition 'lua-dofile-failure :filename filename)))))

(defmacro pushfunction (ls f)
  "Push a Lisp function/closure f to Lua. The function
must take the Lua state as argument and then evaluate to the number of values
that have been pushed onto the Lua stack."
  (let ((callback (gensym "lua-callback")))
    `(progn
       (cffi:defcallback ,callback :int ((l lua-state)) (funcall ,f l))
       (pushcfunction ,ls (cffi:callback ,callback)))))

(defmacro newtable (ls)
  `(createtable ,ls 0 0))

;; TODO: use better data structure for bindings (alist?)
(defmacro create-module (ls module-name &rest bindings)
  `(progn
     (newtable ,ls)
     ,@(loop
         for pair in bindings
         collect
         `(progn
            (pushfunction ,ls ,(cadr pair))
            (setfield ,ls -2 ,(car pair))))
     (setglobal ,ls ,module-name)))

(define-condition unexpected-lua-stack-size (error)
  ((stack-size
    :initarg :stack-size
    :initform nil
    :reader stack-size)))

(defun assert-stack-size (ls expected-size)
  "Assert that the Lua stack currently has size EXPECTED-SIZE."
  (let ((size (gettop ls)))
    (unless (eq expected-size (gettop ls))
      (error (make-condition 'unexpected-lua-stack-size :stack-size size)))))

(defmacro pop (ls n)
  `(settop ,ls ,(- (1+ n))))

(defmacro tostring (ls index)
  `(tolstring ,ls ,index ,(null-pointer)))

(defmacro tonumber (ls index)
  `(tonumberx ,ls ,index ,(null-pointer)))

(defmacro istable (ls index)
  `(eq (type ,ls ,index) +ttable+))
