(asdf:defsystem "cluarena"
  :version "0.0.1"
  :author "Johannes Maier"
  :maintainer "Johannes Maier"
  :mailto "johannes.maier@mailbox.org"
  :license "MIT"
  :depends-on (:cl-raylib)
  :components ((:module "lua-bindings"
                :components
                ((:file "package")
                 (:file "bindings")))
               (:module "src"
                :components
                ((:file "package")
                 (:file "ecs")
                 (:file "main"))))
  :description ""
  :in-order-to ((asdf:test-op (test-op "cluarena/tests"))))

(asdf:defsystem "cluarena/tests"
  :author "Johannes Maier"
  :license "MIT"
  :depends-on (:cluarena
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cluarena"
  :perform (asdf:test-op (op c) (asdf::symbol-call :rove :run c)))
