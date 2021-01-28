;;;; map-generator.lisp

(in-package #:map-generator)

(defvar *initialized* nil)

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)


(defvar *voronoi-verts-arr* nil)
(defvar *voronoi-stream* nil)

(defvar *voronoi-lazy-verts-arr* nil)
(defvar *voronoi-lazy-verts-arr-list* nil)

(defvar *voronoi-lazy-colors-arr* nil)
(defvar *voronoi-lazy-stream* nil)
(defvar *voronoi-lazy-stream-list* nil)


(defvar *testing-gradient* nil)
(defvar *testing-points* nil)
(defvar *testing-voronoi-lazy* nil)

(defvar *testing-voronoi-tmp-points* nil)
(defvar *testing-voronoi-tmp-colors* nil)

(defvar *testing-voronoi-flat* t)
(defvar *voronoi-flat-verts-arr-list* nil)
(defvar *voronoi-flat-stream-list* nil)
(defvar *voronoi-flat-tmp-colors* nil)
(defvar *voronoi-map* nil)

;; This is here in case we use DCEL to represent edges as explained by
;; https://lispcookbook.github.io/cl-cookbook/data-structures.html#circular-lists
(setf *print-circle* t)

(defun-g draw-verts-vert-stage ((vert :vec2) &uniform (c :float))
  (values (v! vert 0 0) ; gl_Position
          (v! (cos c) (sin c) 0)))

(defun-g draw-verts-frag-stage ((cv :vec3))
  (v! cv 0))

(defpipeline-g draw-verts-pipeline ()
  :vertex (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage :vec3))

(defun-g draw-voronoi-vert-stage ((pos :vec2))
  (v! (* 2 (- pos (v! 0.5 0.5))) -100 1))

(defun-g draw-voronoi-frag-stage ()
  (v! 1 0.5 0.5 0))

(defpipeline-g draw-voronoi-pipeline (:points)
  :vertex (draw-voronoi-vert-stage :vec2)
  :fragment (draw-voronoi-frag-stage))

(defun-g draw-voronoi-lazy-vert-stage ((pos :vec3))
  (* (v! (* 2 (- pos (v! 0.5 0.5 0))) 1) ; Map coords to OpenGL coords for X,Y
     (v! 1 1 1 1)))                    

(defun-g draw-voronoi-lazy-frag-stage (&uniform (c :vec3))
  (v! c 0))

(defpipeline-g draw-voronoi-lazy-pipeline (:triangle-fan)
  :vertex (draw-voronoi-lazy-vert-stage :vec3)
  :fragment (draw-voronoi-lazy-frag-stage))

(defun-g draw-voronoi-flat-vert-stage ((pos :vec2))
  (v! (* 2 (- pos (v! 0.5 0.5))) 0 1))

(defun-g draw-voronoi-flat-frag-stage (&uniform (c :vec3))
  (v! c 0))

(defpipeline-g draw-voronoi-flat-pipeline (:triangle-fan)
  :vertex (draw-voronoi-flat-vert-stage :vec2)
  :fragment (draw-voronoi-flat-frag-stage))


(defun init ()
  (unless *initialized*
    (cepl:initialize-cepl)
    (if (cepl.context::legacy-add-surface (cepl-context)  ; context
                                          "map-generator" ; title
                                          400 400         ; xy size
                                          nil             ; fullscreen
                                          t               ; resizable
                                          nil             ; no-frame
                                          nil             ; hidden
                                          t               ; make-current
                                          nil)            ; gl-version
        (setf *initialized* t)
        (format t "Error adding drawing surface~%")))
  
  (when *gpu-verts-arr* (free *gpu-verts-arr*))
  (when *gpu-index-arr* (free *gpu-index-arr*))
  (when *voronoi-verts-arr* (free *voronoi-verts-arr*))
  (when *voronoi-lazy-verts-arr* (free *voronoi-lazy-verts-arr*))
  (when *voronoi-lazy-colors-arr* (free *voronoi-lazy-colors-arr*))
  (when *voronoi-lazy-verts-arr-list* (dolist (arr *voronoi-lazy-verts-arr-list*) (free arr)))
  (when *voronoi-lazy-stream-list* (dolist (stream *voronoi-lazy-stream-list*) (free stream)))

  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -0.5  0.5)
               (v!  -0.5 -0.5)
               (v!   0.5 -0.5)
               (v!   0.5 0.5))
         :element-type :vec2))

  (setf *gpu-index-arr*
        (make-gpu-array
         (list 0 1 2
               0 2 3)
         :element-type :uint))

  ;; TODO: Remove magic numbers
  (setf *testing-voronoi-tmp-points* (make-points 100))
  (setf *testing-voronoi-tmp-colors* (random-colors (length *testing-voronoi-tmp-points*)))


  ;; NOTE: Doing a VAO for each cone. I can't think of a good way to use just 1 VAO while using
  ;; triangle fans.
  (when *testing-voronoi-lazy*
    (setf *voronoi-lazy-verts-arr-list*
          (let ((arr-list '()))
            (dolist (cone (create-cones *testing-voronoi-tmp-points*) arr-list)
              (push (make-gpu-array
                     cone
                     :element-type :vec3) arr-list))))

    (setf *voronoi-lazy-stream-list*
          (let ((stream-list '()))
            (dolist (cone *voronoi-lazy-verts-arr-list* stream-list)
              (push
               (make-buffer-stream
                cone
                :primitive :triangle-fan) stream-list)))))
  

  (when *testing-gradient*
    (setf *vert-stream* (make-buffer-stream *gpu-verts-arr* :index-array *gpu-index-arr*)))

  (when *testing-points*
    (setf *voronoi-verts-arr* (make-gpu-array *testing-voronoi-tmp-points* :element-type :vec2))
    (setf *voronoi-stream* (make-buffer-stream *voronoi-verts-arr* :primitive :points)))

  ;; TODO: Remove magic numbers
  (when *testing-voronoi-flat*
    (setf *voronoi-flat-tmp-colors* (random-colors 200))
    (setf *voronoi-map* (map 'list #'cell-to-fan (lloyd (voronoi 10) 10)))
    (setf *voronoi-flat-verts-arr-list*
          (let ((fan-list '()))
            (dolist (cell *voronoi-map* fan-list)
              (push (make-gpu-array cell :element-type :vec2) fan-list))))
    (setf *voronoi-flat-stream-list*
          (let ((stream-list '()))
            (dolist (fan *voronoi-flat-verts-arr-list* stream-list)
              (push (make-buffer-stream fan :primitive :triangle-fan) stream-list))))))

(defun draw ()
  (clear)
  (when *testing-gradient*
    (map-g #'draw-verts-pipeline *vert-stream* :c (float (/ (get-internal-real-time) 500))))
  
  (when *testing-points*
    (cl-opengl:point-size 5)
    (map-g #'draw-voronoi-pipeline *voronoi-stream*))

  (when *testing-voronoi-lazy*
    (loop :for cone-stream :in *voronoi-lazy-stream-list*
          :for color :in *testing-voronoi-tmp-colors*
          :do (map-g #'draw-voronoi-lazy-pipeline cone-stream :c color)))

  (when *testing-voronoi-flat*
    (loop :for cell-stream :in *voronoi-flat-stream-list*
          :for color :in *voronoi-flat-tmp-colors*
          :do (map-g #'draw-voronoi-flat-pipeline cell-stream :c color)))
  
  (swap)
  (decay-events))

(def-simple-main-loop main-loop (:on-start #'init)
  (when (and *initialized* :start)
    (draw)))


;; NOTE: We can't really quit in the proper sense since cepl:free-context is not implemented
;; https://github.com/cbaggers/cepl/blob/master/core/context/cepl-context.lisp#L53
;; Instead of trying to quit CEPL proper we just close the application window
(defun quit ()
  (main-loop :stop)
  ;; TODO: Conditional freeing of pipelines
  ;; (free-pipeline #'draw-verts-pipeline)
  ;; (free-pipeline #'draw-voronoi-pipeline)
  ;; (free-pipeline #'draw-voronoi-lazy-pipeline)
  ;; (free-pipeline #'draw-voronoi-flat-pipeline)
  (when (and (cepl-context) (current-surface))
    (setf *initialized* nil)
    (remove-surface (cepl-context) (current-surface))))
