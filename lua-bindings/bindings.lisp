(in-package :lua)

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

(defcfun (pushnumber "lua_pushnumber") :void
  (ls lua-state)
  (x :double))

(defcfun (pushinteger "lua_pushinteger") :void
  (ls lua-state)
  (n :int))

(defcfun (tonumberx "lua_tonumberx") :double
  (ls lua-state)
  (index :int)
  (isnum (:pointer :int)))

(defcfun (tolstring "lua_tolstring") :string
  (ls lua-state)
  (index :int)
  (len (:pointer :size)))

(defcfun (setglobal "lua_setglobal") :void
  (ls lua-state)
  (s :string))

(defcfun (getglobal "lua_getglobal") :int
  (ls lua-state)
  (s :string))

(defcfun (callk "lua_callk") :void
  (ls lua-state)
  (nargs :int)
  (nresults :int)
  (fixme :int)
  (k :pointer))

(defcfun (pcallk "lua_pcallk") :int
  (ls lua-state)
  (nargs :int)
  (nresults :int)
  (errfunc :int)
  (fixme :int)
  (k :pointer))

(defcfun (pushcclosure "lua_pushcclosure") :void
  (ls lua-state)
  (f :pointer)
  (n :int))

(defcfun (loadfilex "luaL_loadfilex") :int
  (ls lua-state)
  (filename :string)
  (mode :string))

(defcfun (openselectedlibs "luaL_openselectedlibs") :void
  (ls lua-state)
  (load :int)
  (preload :int))

(defcfun (openlibs "luaL_openlibs") :void
  (ls lua-state))

(defcfun (createtable "lua_createtable") :void
  (ls lua-state)
  (narr :int)
  (nrec :int))

(defcfun (setfield "lua_setfield") :void
  (ls lua-state)
  (index :int)
  (k :string))

(defcfun (gettable "lua_gettable") :int
  (ls lua-state)
  (index :int))

(defcfun (getfield "lua_getfield") :int
  (ls lua-state)
  (index :int)
  (name :string))

(defcfun (gettop "lua_gettop") :int
  (ls lua-state))

(defcfun (type "lua_type") :int
  (ls lua-state)
  (index :int))

(defcfun (settop "lua_settop") :void
  (ls lua-state)
  (index :int))

(defcfun (rawlen "lua_rawlen") :unsigned-int
  (ls lua-state)
  (index :int))
