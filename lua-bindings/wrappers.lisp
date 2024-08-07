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
