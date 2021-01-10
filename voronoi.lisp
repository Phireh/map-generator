(in-package #:map-generator)

;; Ad-hoc priority queue ordered by Y coord
;; NOTE: for the time being, our map uses coords from 0f (bottom left to 1f (top right)
;; Fortune's algorithm is usually done from top to bottom,
;; therefore this implementation of pqueue uses higher Y as higher prio

(declaim (type (or null list-of-vec2) *voronoi-pqueue*))
(defvar *voronoi-pqueue* nil)
(defvar *gnuplot-handle* nil)

(defun debug-triangle-image (triangle-list)
  "Launches a gnuplot process to draw the process."
  ;; TODO: gnuplot stays alive in the background and recreates the window at every plot,
  ;; add a function to kill the process
  (dump-triangles triangle-list)
  (unless (and *gnuplot-handle*
               (uiop:process-alive-p *gnuplot-handle*))
    (setf *gnuplot-handle* (uiop:launch-program "gnuplot plot-triangles.plg")))
  (break))

(defun dump-points (point-list)
  (with-open-file (str "points.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop :for point :in point-list :do
      (format str "~A ~A~%" (elt point 0) (elt point 1)))))

;; plot 'triangle-data.txt' using 1:2 with lines
(defun dump-triangles (triangle-list)
  (with-open-file (str "triangle-data.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop :for triangle :in triangle-list :do
      (format str "~A ~A~%~A ~A~%~A ~A~%~A ~A~%~%"
              (elt (first triangle) 0)
              (elt (first triangle) 1)
              (elt (second triangle) 0)
              (elt (second triangle) 1)
              (elt (third triangle) 0)
              (elt (third triangle) 1)
              (elt (first triangle) 0)
              (elt (first triangle) 1)))))

(defun counterclockwise-p (a b c)
  "Checks if three points are counterclockwise."
  (let ((acx (- (x a) (x c)))
        (bcx (- (x b) (x c)))
        (acy (- (y a) (y c)))
        (bcy (- (y b) (y c))))
    (> (- (* acx bcy) (* acy bcx)) 0)))

(defun counterclockwise (tri)
  "Returns a counterclockwise version of tri, or just tri unchanged if it already is counterclockwise.
This is important for two reasons:
1. During Delaunay triangulation we rely on triangles being counterclockwise to calculate their circumsphere.
2. OpenGL treats clockwise triangles as facing away and does not render them."
  (etypecase tri
    (triangle-2d
     (let ((a (first tri))
           (b (second tri))
           (c (third tri)))
       (if (counterclockwise-p a b c)
           tri              ; return triangle if already counterclockwise
           (list b a c)))))) ; otherwise turn triangle into counterclockwise

(defmacro pqueue-npush (vec &optional (queue '*voronoi-pqueue*))
  (let ((queue-name (symb queue)))
    `(setf ,queue-name (pqueue-push ,vec ,queue-name))))

(defun pqueue-push (vec queue)
  (etypecase vec
    (vec2
     (setf queue (sort (push vec queue) #'> :key #'y)))
    (vec3
     (error "Not yet implemented"))))

(defstruct v-node
  (position (v! 0.0 0.0) :type vec2)
  (edges '() :type list)
  (parent nil :type (or null v-node))
  (left-child nil :type (or null v-node))
  (right-child nil :type (or null v-node)))

(defun leafp (node)
  (etypecase node
    (v-node (if (or (v-node-left-child node) (v-node-right-child node))
                nil
                t))))

(defun make-points (&optional (n 100))
  (loop :with arr = '()
        :for i :below n
        :do
           (push (v2! (random 1.0) (random 1.0)) arr)
        :finally (return arr)))

(defun random-colors (&optional (n 100))
  (loop :with arr = '()
        :for i :below n
        :do
           (push (v3! (random 1.0) (random 1.0) (random 1.0)) arr)
        :finally (return arr)))

;; Naive Delaunay O(n²) implementation below

(declaim (type (or null list-of-tri-2d) *delaunay-triangle-list*))
(defvar *delaunay-triangle-list* nil)

(defun inside-circle-p (tri p)
  "Checks if point P is inside the circle defined by the triangle ABC."
  (let* ((a (first tri))
         (b (second tri))
         (c (third tri))
         (adx (- (x a) (x p)))
         (ady (- (y a) (y p)))
         (bdx (- (x b) (x p)))
         (bdy (- (y b) (y p)))
         (cdx (- (x c) (x p)))
         (cdy (- (y c) (y p)))
         (abdet (- (* adx bdy) (* bdx ady)))
         (bcdet (- (* bdx cdy) (* cdx bdy)))
         (cadet (- (* cdx ady) (* adx cdy)))
         (alift (+ (* adx adx) (* ady ady)))
         (blift (+ (* bdx bdx) (* bdy bdy)))
         (clift (+ (* cdx cdx) (* cdy cdy))))
    (>
     (+ (* alift bcdet) (* blift cadet) (* clift abdet)) 0)))


(defun delaunay-condition-p (tri1 tri2)
  "Checks if two adjacent triangles meet the Delaunay condition.
   The triangles are defined as ABC and CAD, where B and D are the non-common points.
   If the point D is inside the circumcircle of ABC the triangles violate the Delaunay condition"
  (flet ((inside-tri (v tri)
           (loop :for v2 :in tri
                 :when (rtg-math.vector2:= v v2) :do (return v2))))
    
    (let ((d nil))
      (cond
        ((not (inside-tri (first tri2) tri1)) (setf d (first tri2)))
        ((not (inside-tri (second tri2) tri1)) (setf d (second tri2)))
        ((not (inside-tri (third tri2) tri1)) (setf d (third tri2))))

      (not (inside-circle-p tri1 d)))))

(defun flip-triangles (tri1 tri2)
  "Flips two triangles ABC and CAD into ABD and BCD."
  (flet ((inside-tri (v tri)
           (loop :for v2 :in tri
                 :when (rtg-math.vector2:= v v2) :do (return v2))))

    ;; Guessing values of ABC
    (let ((a (first tri1))
          (b (second tri1))
          (c (third tri1))
          (d nil))

      ;; Check for common points to correctly guess ABC
      (when (inside-tri b tri2)
        (if (not (inside-tri a tri2))
            (psetq a b b a)
            (psetq b c c b)))

      ;; Guess D
      (cond
        ((not (inside-tri (first tri2) tri1)) (setf d (first tri2)))
        ((not (inside-tri (second tri2) tri1)) (setf d (second tri2)))
        ((not (inside-tri (third tri2) tri1)) (setf d (third tri2))))
      (values
       (counterclockwise (list a b d))
       (counterclockwise (list b c d))))))

;; Bowyer Watson algorithm
(defun delaunay-slow (point-list)
  (let* ((triangle-list '())
         ;; Our super-triangle to initiate the Delaunay algorithm. It is large enough to hold
         ;; all of the plane inside it.
         ;; TODO: Remove magic numbers         
         (super-vertex-a (v! -1 0))
         (super-vertex-b (v! 2 0))
         (super-vertex-c (v! 0.5 2))
         (super-triangle  (counterclockwise (list super-vertex-a super-vertex-b super-vertex-c))))

    (when (>= (length point-list) 3)
      (dump-points point-list) ;; For gnuplot visualization
      ;; First triangle
      (setf triangle-list (push super-triangle triangle-list))
      
      (loop :for vertex :in point-list
            :for polygon-edges = '()
            :for bad-triangles = '()
            :do               
               (loop :for triangle :in triangle-list
                     :when (inside-circle-p triangle vertex) :do                       
                       (npush triangle bad-triangles))
               
               (let ((candidate-edges '()))
                 (dolist (tri bad-triangles)
                   (npush (list (first tri) (second tri)) candidate-edges)
                   (npush (list (second tri) (third tri)) candidate-edges)
                   (npush (list (third tri) (first tri)) candidate-edges)
                   (ndelete tri triangle-list))
                 
                 (dolist (edge candidate-edges)
                   (when (= 1 (count edge candidate-edges :test #'edge=))
                     (npush edge polygon-edges))))
               
               (dolist (edge polygon-edges)
                 (npush (counterclockwise (list (first edge) (second edge) vertex)) triangle-list))
               ;(debug-triangle-image triangle-list)
            ))
    
    ;; Cleanup: remove supertriangle-related triangles
    (flet ((contains-super-vertex-p (triangle)
             "Check if a super-vertex is included in the triangle"
             (loop :for vertex :in triangle
                   :when (or (rtg-math.vectors:= vertex super-vertex-a)
                             (rtg-math.vectors:= vertex super-vertex-b)
                             (rtg-math.vectors:= vertex super-vertex-c))
                     :do
                        (return t))))
      
      (loop :for triangle :in triangle-list
            :when (contains-super-vertex-p triangle) :do
              (setf triangle-list (remove triangle triangle-list))))

    (debug-triangle-image triangle-list)
    ;; Return our triangulation as a list of lists of 3 2dvecs
    triangle-list))



