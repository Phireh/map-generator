(in-package #:map-generator)

;; Ad-hoc priority queue ordered by Y coord
;; NOTE: for the time being, our map uses coords from 0f (bottom left to 1f (top right)

;; Debugging globals
(defvar *gnuplot-handle* nil)
(defvar *debugging-voronoi* t)
(defvar *debugging-delaunay* nil)


(defvar *super-triangle-padding* 0.01f0)
(defvar *bounding-box-min-x* 0)
(defvar *bounding-box-max-x* 1)
(defvar *bounding-box-min-y* 0)
(defvar *bounding-box-max-y* 1)
(defvar *bounding-box-vertices* (list (v! *bounding-box-min-x* *bounding-box-min-y*)
                                      (v! *bounding-box-min-x* *bounding-box-max-y*)
                                      (v! *bounding-box-max-x* *bounding-box-min-y*)
                                      (v! *bounding-box-max-x* *bounding-box-max-y*)))



(defun start-debug-process ()
  (when *debugging-voronoi*
      (unless (and *gnuplot-handle*
                   (uiop:process-alive-p *gnuplot-handle*))
        (setf *gnuplot-handle* (uiop:launch-program "gnuplot plot-voronoi.plg")))))

(defun stop-debug-process ()
  (when *debugging-voronoi*
    (when (and *gnuplot-handle*
              (uiop:process-alive-p *gnuplot-handle*))
     (uiop:terminate-process *gnuplot-handle*))))


(defun dump-points (point-list)
  (when *debugging-voronoi*
    (with-open-file (str "points.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
     (loop :for point :in point-list :do
       (format str "~A ~A~%" (elt point 0) (elt point 1))))))

;; plot 'triangle-data.txt' using 1:2 with lines
(defun dump-triangles (triangle-list)
  (when (and *debugging-voronoi*
             *debugging-delaunay*)
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
                (elt (first triangle) 1))))))

(defun dump-circles (circle-list)
  (when *debugging-voronoi*
    (with-open-file (str "circle-data.txt"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (loop :for circle :in circle-list :do
        (format str "~A ~A ~A~%~%"
                (elt circle 0)
                (elt circle 1)
                (elt circle 2))))))

(defun dump-cells (cell-list)
  (when *debugging-voronoi*
    (with-open-file (str "cells.txt"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      
      (loop :for cell :in cell-list
            :do
               (loop :for edge :in (cdr cell)
                     :for p1 = (first edge)
                     :for p2 = (second edge)
                     :do
                        (format str "~A ~A~%~A ~A~%~%" (x p1) (y p1) (x p2) (y p2)))))))

(defun clean-debug-data ()
  (when *debugging-voronoi*
    (uiop:delete-file-if-exists "cells.txt")
    (uiop:delete-file-if-exists "circle-data.txt")
    (uiop:delete-file-if-exists "triangle-data.txt")
    (uiop:delete-file-if-exists "points.txt")))

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

(defun make-points (&optional (n 100))
  (loop :with arr = '()
        :for i :below n
        :do
           (push (v2! (random 1.0) (random 1.0)) arr)
        :finally (return arr)))

(defun bounding-box (point-list)
  "Returns the bounding rectangle that contains all points in the list."
  ;; TODO: find a way to ask for maximum possible single float
  (let ((min-x 99999999f0)
        (min-y 99999999f0)
        (max-x 0)
        (max-y 0))
    (dolist (point point-list)
      (setf min-x (min min-x (x point)))
      (setf max-x (max max-x (x point)))
      (setf min-y (min min-y (y point)))
      (setf max-y (max max-y (y point))))
    (list (- min-x *super-triangle-padding*)
          (- min-y *super-triangle-padding*)
          (+ max-x *super-triangle-padding*)
          (+ max-y *super-triangle-padding*))))

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
  (clean-debug-data)
  (start-debug-process)
  (let* ((triangle-list '())
         ;; Our super-triangle to initiate the Delaunay algorithm. It is large enough to hold
         ;; all of the plane inside it.
         (box (bounding-box point-list))
         (super-vertex-a (v! (- (first box) (third box)) (* (second box) 2f0)))    ; Left vertex
         (super-vertex-b (rtg-math.vector2:*s (v! (third box) (second box)) 2f0)) ; Right vertex
         (super-vertex-c (v! (/ (+ (first box) (third box)) 2) (* 2 (fourth box))))
         (super-triangle  (counterclockwise (list super-vertex-a super-vertex-b super-vertex-c))))

    (format t "Bounding box is ~A~%" box)
    (format t "Super triangle is ~A~%" super-triangle)

    (when (>= (length point-list) 3)
      (dump-points point-list) ;; For gnuplot visualization
      ;; First triangle
      (push super-triangle triangle-list))
      
    (loop :for vertex :in point-list
          :for polygon-edges = '()
          :for bad-triangles = '()
          :do               
             (loop :for triangle :in triangle-list
                   :when (inside-circle-p triangle vertex) :do                       
                     (push triangle bad-triangles))
             
             (let ((candidate-edges '()))
               (dolist (tri bad-triangles)
                 (push (list (first tri) (second tri)) candidate-edges)
                 (push (list (second tri) (third tri)) candidate-edges)
                 (push (list (third tri) (first tri)) candidate-edges)
                 (ndelete tri triangle-list))
               
               (dolist (edge candidate-edges)
                 (when (= 1 (count edge candidate-edges :test #'edge=))
                   (push edge polygon-edges))))
             
             (dolist (edge polygon-edges)
               (push (counterclockwise (list (first edge) (second edge) vertex)) triangle-list))
                                        (dump-triangles triangle-list))
    
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

    (dump-triangles triangle-list)
    ;; Return our triangulation as a list of lists of 3 2dvecs
    triangle-list))

(defun line-from-points (p1 p2)
  "Returns the line between two points in ax + by = c form."
  (let* ((a (- (y p2) (y p1)))
        (b (- (x p1) (x p2)))
        (c (+ (* a (x p1)) (* b (y p1)))))
    (values a b c)))

(defun line-bisector (p1 p2 a b)
  "Takes a line in form ax + by = c and a two points
   and returns the bisector that passes through the midpoint."
  (let* ((midpoint-x (/ (+ (x p2) (x p1)) 2))
         (midpoint-y (/ (+ (y p2) (y p1)) 2))
         (bisec-a (- b))
         (bisec-b a)
         (bisec-c (+ (* bisec-a midpoint-x) (* bisec-b midpoint-y))))

    (values bisec-a bisec-b bisec-c)))

;; TODO: Handle case determinant = 0
(defun line-intersection (a1 b1 c1 a2 b2 c2)
  "Returns the meeting point of two lines in ax + by = c form."
  (let* ((determinant (- (* a1 b2) (* a2 b1)))
         (x (/ (- (* b2 c1) (* b1 c2)) determinant))
         (y (/ (- (* a1 c2) (* a2 c1)) determinant)))
    (values x y)))

(defun circumcircle (tri)
  "Returns the X, Y coord and the radius of the circumcircle described by a triangle."

  (let ((a (first tri))
        (b (second tri))
        (c (third tri)))
    
    (multiple-value-bind (center-x center-y)
        (multiple-value-bind (abx aby) (line-from-points a b)
        (multiple-value-bind (bcx bcy) (line-from-points b c)
          (multiple-value-call #'line-intersection
            (line-bisector a b abx aby)
            (line-bisector b c bcx bcy))))

      (list center-x center-y
            (rtg-math.vector2:distance (v! center-x center-y) a)))))

(defun push-bounding-box (list)
  "Adds a bounding box of 4 points around the point list."
  (push (v! 0 0) list)
  (push (v! 0 1) list)
  (push (v! 1 0) list)
  (push (v! 1 1) list))

(defun inside-bounding-box-p (point)
  (let ((xp (x point))
        (yp (y point)))
    (and (<= *bounding-box-min-x* xp *bounding-box-max-x*)
         (<= *bounding-box-min-y* yp *bounding-box-max-y*))))

(defun voronoi (n &key (points nil))
  "Takes a number n of points in (0,0-1,1) range and returns a list
   of cells. A cell being a list where the first member is the center
   and the rest are tuples of 2d vectos representing edges."
  (let ((point-list (if points points (make-points n)))
        (triangulation nil))
    (setf triangulation (delaunay-slow point-list))
    
    (loop :for point :in point-list
          ;:for i :below 1
          :for point-triangle-list = '()
          :with test-circle-list = '()
          :with cell-edges = '()
          :do
             (format t "Point ~A~%" point)
             ;; Find the triangles that contain the point
             (setf point-triangle-list (remove-if-not
                                        (lambda (tri) (when (triangle-vertex-p tri point) tri))
                                        triangulation))
             ;; Find the adjacent triangles to these triangles inside the current triangle list
             (loop
               :for triangle1 :in point-triangle-list
               :for point-adjacent-triangle-list = '()
               :for point-adjacent-circumcircle-list = '()
               :for triangle-circumcircle = (circumcircle triangle1)
               :with current-cell = '()
               :do
                  (pop point-triangle-list)
                  (setf point-adjacent-triangle-list
                        (remove-if-not (lambda (triangle2) (shared-edge-p triangle1 triangle2))
                                       point-triangle-list))
                  (setf point-adjacent-circumcircle-list
                        (map 'list #'circumcircle point-adjacent-triangle-list))
                  (dolist (circle point-adjacent-circumcircle-list)
                    (push circle test-circle-list))
                  (push triangle-circumcircle test-circle-list)
                  
                  (dolist (circle point-adjacent-circumcircle-list)
                    (push (list (v! (first triangle-circumcircle)
                                    (second triangle-circumcircle))
                                (v! (first circle)
                                    (second circle))) current-cell))
               :finally
                  (format t "~A edges~%" (length current-cell))
                  (push point current-cell)
                  (push current-cell cell-edges))
          :finally (dump-circles test-circle-list)
                   (dump-cells cell-edges)
                   (return cell-edges))))


;; TODO: Fix this function. Don't know the cause yet but it diverges instead of converging.
;; TODO: Apply bounding box.
(defun lloyd (voronoi-diagram &optional (n 1))
  "Applies Lloyd's relaxation to a Voronoi diagram."
  (let ((new-diagram voronoi-diagram))
    (flet ((cell-centroid (cell)
             "Computes the centroid of a Voronoi cell by accumulating the area and center of each of its
              constituent triangles. The centroid is calculated by the formula C = (1/A_c)*sum(a_i*c_i), where:
              C = cell centroid.
              A_c = accumulated area of all the triangles.
              a_i = individual area of each triangle.
              c_i = individual centroid of each triangle."
             (let ((center (first cell)))
               (loop :for edge :in (cdr cell)
                     :for triangle = (counterclockwise (list center (first edge) (second edge)))
                     :for circumcenter-x = (first (circumcircle triangle))
                     :for circumcenter-y = (second (circumcircle triangle))
                     :for circumcenter = (v! circumcenter-x circumcenter-y)
                     :for triangle-area = (tri-area triangle)
                     :for weighted-centroid = (rtg-math.vector2:*s circumcenter triangle-area)
                     :with acc-area = 0
                     :with weighted-sum = (v! 0 0)
                     :do
                        (incf acc-area triangle-area)
                        (rtg-math.vector2:incf weighted-sum weighted-centroid)
                     :finally
                        (let ((centroid-candidate (rtg-math.vector2:*s weighted-sum (/ 1 acc-area))))
                          (if (inside-bounding-box-p centroid-candidate)
                              (return centroid-candidate)
                              ;; TODO: Handle border case moving the centroid in a more clever way
                              (return center)))))))
      (loop :for i :below n
            :for new-centroids = '()
            :do
               (loop :for cell :in new-diagram
                     :for centroid = (car cell)
                     ;; Only try to compute new centroid if a cell is well formed (aka is not a single point).
                     ;; Also do not try to modify the position of the bounding box guardrails
                     :when (and (> (length cell) 1)
                                (not (or (rtg-math.vector2:= centroid (first *bounding-box-vertices*))
                                         (rtg-math.vector2:= centroid (second *bounding-box-vertices*))
                                         (rtg-math.vector2:= centroid (third *bounding-box-vertices*))
                                         (rtg-math.vector2:= centroid (fourth *bounding-box-vertices*)))))
                       :do
                           (format t "Moving ~A to ~A~%" (first cell) (cell-centroid cell))
                           (setf centroid (cell-centroid cell))
                       :do
                          (push centroid new-centroids)
                     :finally
                        (setf new-diagram (voronoi 0 :points new-centroids)))
            :finally (return new-diagram)))))
