(in-package :pixel-spirit-deck)

(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (v2! .5) (* vert .5))))

(defun-g frag ((uv :vec2))
  (v! 0 0 1 0))

(defpipeline-g pipe ()
  (vert :vec2)
  (frag :vec2))

(defun init ())

(defun draw! ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface)))
  (as-frame
    (map-g #'pipe (get-quad-stream-v2))))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
