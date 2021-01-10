;;;; package.lisp

(uiop:define-package #:map-generator
    (:use #:cl #:cepl                 ;; Basic graphics support. Includes cepl.sdl2
          #:cepl-utils                ;; For defn-inline and other niceties
          #:rtg-math                  ;; For vectors
          #:rtg-math.base-maths       ;; For sfzero-p, k-epsilon, etc
          #:rtg-math.types            ;; For vecX, matX ...
          #:vari #:nineveh            ;; misc
          #:glsl-symbols              ;; For vecX ... needed by rtg-math.types
          #:cepl.skitter
          #:livesupport)
  (:import-from :rtg-math.base-maths :sfzero-p
                :+k-epsilon+))

(in-package :map-generator)

