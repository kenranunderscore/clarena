(in-package :lua)

(defun call (ls nargs nresults)
  (callk ls nargs nresults 0 (null-pointer)))

(defun pcall (ls nargs nresults errfunc)
  (pcallk ls nargs nresults errfunc 0 (null-pointer)))

(defun pushcfunction (ls f)
  (pushcclosure ls f 0))

(defun loadfile (ls filename)
  (loadfilex ls filename (null-pointer)))

(defun dofile (ls filename)
  (let ((res (loadfile ls filename)))
    (when (= 0 res)
      (pcall ls 0 -1 0))))

(defun pushfunction (ls f)
  "Push a Lisp function/closure f to Lua. The function
must take the Lua state as argument and then evaluate to the number of values
that have been pushed onto the Lua stack."
  (cffi:defcallback cb :int ((l lua-state)) (funcall f l))
  (pushcfunction ls (cffi:callback cb)))

(defun register (ls name f)
  (pushfunction ls f)
  (setglobal ls name))

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
