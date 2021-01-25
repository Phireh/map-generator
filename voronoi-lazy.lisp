;; !! This file is just for benchmarking and can be ignored. !!

(in-package #:map-generator)

(defvar *voronoi-lazy-cone-radius* 0.2)
(defvar *voronoi-lazy-ntriangles* 64)
(defvar *voronoi-lazy-height* 0.05)

;; SBCL emits style-warnings when the compiler encounters the type :vec3
;; even if it works correctly. This reduces compiler verbosity while compiling
;; NOTE: could this use a list instead of an array?
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(defun create-cones (points)
  ;; Sanity check: we're dealing with a list of vectors
  (etypecase points
    (list
     (etypecase (car points)
       (vec2 t))))

  ;; For each center, create a 64-triangle fan as described in
  ;; https://nullprogram.com/blog/2014/06/01/
  ;; Note that this could also be done with dentrite

  ;; Note: do not change original list as to not to pollute the original points,
  ;; in case we want to compare them later.
  (let ((cone-vertices '()))
    ;; To define a cone with a triangle fan we create 64 points in a circle around it
    ;; The Z coord of the center must be closer to the camera
    ;; Note that we create it counterclock-wise as per GL convention
    (dolist (cone-center points cone-vertices)
      (let ((cone-vert-list '()))
        ;; Set cone center at arr idx 0
        (push (v! cone-center *voronoi-lazy-height*) cone-vert-list)
        (loop :for i :from 0 :to *voronoi-lazy-ntriangles*
              :for angle = (* (/ 2pi *voronoi-lazy-ntriangles*) i)
              :for offset-x = (* (cos angle) *voronoi-lazy-cone-radius*)
              :for offset-y = (* (sin angle) *voronoi-lazy-cone-radius*)
              :for offset-vector = (v2! offset-x offset-y)
              :do
                 (push (v! (rtg-math.vectors:+ cone-center offset-vector) 0.1) cone-vert-list)
              :finally (push (nreverse cone-vert-list) cone-vertices))))))
;; Return compiler verbosity to the usual
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

