(cl:defpackage :lua
  (:use :cffi))
(cl:in-package :lua)

(define-foreign-library lua-5.4
  (:unix (:or "liblua.so.5.4" "liblua.so"))
  (t (:default "liblua")))

(use-foreign-library lua-5.4)

(defctype lua-state :pointer)

(defcfun (newstate "luaL_newstate") lua-state)

(defcfun (close "lua_close") :void
  (ls lua-state))

(defcfun (pushstring "lua_pushstring") :string
  (ls lua-state)
  (s :string))

(defcfun (tolstring "lua_tolstring") :string
  (ls lua-state)
  (index :int)
  (len (:pointer :size)))

(cl:defun tostring (ls index)
  (tolstring ls index (null-pointer)))
