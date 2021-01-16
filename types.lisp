(in-package #:map-generator)

;; Custom types for additional sanity checking
;; Inspired by https://lispcookbook.github.io/cl-cookbook/type.html
(defun list-of-vec3-p (list)
  (and (consp list)
       (every (lambda (v) (and (= (length v) 3)
                               (arrayp v)))
              list)))

(deftype list-of-vec3 ()
  `(satisfies list-of-vec3-p))


(defun list-of-vec2-p (list)
  (and (consp list)
       (every (lambda (v) (and (= (length v) 2)
                               (arrayp v)))
              list)))

(deftype list-of-vec2 ()
  `(satisfies list-of-vec3-p))

(defun edge= (edge1 edge2)
(flet ((inside-edge (p edge)
           (loop :for p2 :in edge
                 :when (rtg-math.vector2:= p p2) :do (return p2))))

    (let ((count 2))
      (when (inside-edge (first edge1) edge2) (decf count))
      (when (inside-edge (second edge1) edge2) (decf count))
      (zerop count))))

(defun triangle-2d-p (tri)
  (and (list-of-vec2-p tri)
       (= (length tri) 3)))

(defun triangle= (tri1 tri2)
  (flet ((inside-tri (v tri)
           (loop :for v2 :in tri
                 :when (rtg-math.vector2:= v v2) :do (return v2))))

    (let ((count 3))
      (when (inside-tri (first tri1) tri2) (decf count))
      (when (inside-tri (second tri1) tri2) (decf count))
      (when (inside-tri (third tri1) tri2) (decf count))
      (zerop count))))

(defun tri-area (tri)
  "Returns the area filled by triangle tri using Heron's formula."
  (let* ((dab (rtg-math.vector2:distance (first tri) (second tri)))
         (dbc (rtg-math.vector2:distance (second tri) (third tri)))
         (dca (rtg-math.vector2:distance (third tri) (first tri)))
         ;; Half perimeter
         (s (/ (+ dab dbc dca) 2.0)))
    (sqrt (- s dab)) (- s dbc) (- dca)))

(defun triangle-vertex-p (tri p)
  "Returns t if point p is one of the vertices of triangle tri."
  (or (rtg-math.vector2:= p (first tri))
      (rtg-math.vector2:= p (second tri))
      (rtg-math.vector2:= p (third tri))))

(defun adjacentp (tri1 tri2)
  (flet ((inside-tri (v tri)
           (loop :for v2 :in tri
                 :when (rtg-math.vector2:= v v2) :do (return v2))))
    (let ((count 3))
      
      (when (inside-tri (first tri1) tri2) (decf count))
      (when (inside-tri (second tri1) tri2) (decf count))
      (when (inside-tri (third tri1) tri2) (decf count))
      (= count 1))))

(defun shared-edge-p (tri1 tri2)  
  (let* ((ab (list (first tri1) (second tri1)))
         (bc (list (second tri1) (third tri1)))
         (ca (list (third tri1) (first tri1)))
         (de (list (first tri2) (second tri2)))
         (ef (list (second tri2) (third tri2)))
         (gd (list (third tri2) (first tri2))))
    (or (edge= ab de)
        (edge= ab ef)
        (edge= ab gd)
        (edge= bc de)
        (edge= bc ef)
        (edge= bc gd)
        (edge= ca de)
        (edge= ca ef)
        (edge= ca gd))))

(deftype triangle-2d ()
  `(satisfies triangle-2d-p))

(defun list-of-tri-2d-p (list)
  (and (consp list)
       (every #'triangle-2d-p list)))

(deftype list-of-tri-2d ()
  `(satisfies list-of-tri-2d-p))
