(in-package :pixel-spirit-deck)

;;; 000-void.frag
(defun-g frag ((uv :vec2)
               &uniform (resolution :vec2)
               (time :float))
  (v! 0 0 0 1))

;;; 048-trinity.frag
;;; Using the defcard() macro
(defcard ()
  (let* ((st (v! (x st) (- 1.0 (y st))))
         (s .25)
         (t1 (poly-sdf (+ st (v2! .0 .175)) 3))
         (t2 (poly-sdf (+ st (v2! .1 .0)) 3))
         (t3 (poly-sdf (- st (v2! .1 .0)) 3))
         (color (+ color
                   (stroke t1 s .08)
                   (stroke t2 s .08)
                   (stroke t3 s .08)))
         (bridges (mix (mix t1 t2 (step .5 (y st)))
                       (mix t3 t2 (step .5 (y st)))
                       (step .5 (x st))))
         (color (bridge color bridges s .08)))
    (v! color 1)))

;;--------------------------------------------------

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))

(defvar *bs*
  (make-buffer-stream nil :primitive :points))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    (as-frame
      (map-g #'pipe *bs*
             :resolution res
             :time (/ (get-internal-real-time)
                      1000f0)))))

(def-simple-main-loop play ()
  (draw!))
