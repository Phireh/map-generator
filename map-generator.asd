;;;; map-generator.asd

(asdf:defsystem #:map-generator
  :description "Map generator"
  :author "Phireh <Phireh@fukurokuju.dev>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:rtg-math.vari #:cepl.sdl2 #:swank #:livesupport #:cepl.skitter.sdl2 #:dirt #:nineveh)
  :components ((:file "package")
               (:file "macros")
               (:file "types")
               (:file "voronoi-lazy")
               (:file "voronoi")
               (:file "map-generator")))
