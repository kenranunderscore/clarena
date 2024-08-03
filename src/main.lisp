(defpackage cluarena
  (:use :cl))
(in-package :cluarena)

(defconstant +width+ 1600)
(defconstant +height+ 900)

(defun main ()
  (raylib:with-window (+width+ +height+ "CLUARENA")
    (raylib:set-target-fps 60)
    (loop
      until (raylib:window-should-close)
      do (raylib:with-drawing
           (raylib:clear-background :darkgray)
           (raylib:draw-fps 20 (- +height+ 20))
           (raylib:draw-circle 200 300 25.0 :red)))))
