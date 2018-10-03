#lang racket/gui
;;;;; frame

(provide gl-run)

(require racket/date
         racket/draw
         racket/class
         racket/math
         sgl
         sgl/gl
         
         sgl/gl-vectors)

(define (gl-run)
  (let* ((frame (new frame% (label "OpenGL Window") 
                     (width 640) 
                     (height 480)))
         (glcanvas (new canvas% (parent frame) (style '(gl))))
         (gl-context  (send (send glcanvas get-dc) get-gl-context)))
    (unless (and gl-context (send gl-context ok?))
        (display "Error: OpenGL context failed to initialize")
      (newline)
      (exit))
    (send frame show #t)))

(gl-run)