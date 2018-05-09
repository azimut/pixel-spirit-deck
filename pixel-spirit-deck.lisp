(in-package :pixel-spirit-deck)

(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (v2! .5) (* vert .5))))

;;; 000-void.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2)
               (time :float))
  (v! 0 0 0 1))

(defpipeline-g pipe ()
  (vert :vec2)
  (frag :vec2))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    (as-frame
      (map-g #'pipe (get-quad-stream-v2)
             :resolution res
             :time (/ (get-internal-real-time) 1000f0)))))

(def-simple-main-loop play ()
  (draw!))
