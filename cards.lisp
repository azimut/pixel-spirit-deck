(in-package :pixel-spirit-deck)

;;; 001-justice.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v! 0 0 0))
         (color (+ color (step .5 (x st)))))
    (v! color 1)))

;;; 002-strenght.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v! 0 0 0))
         (color (+ color (step (+ .5
                                  (* .25
                                     (cos (* (y st)
                                             3.1415))))
                               (x st)))))
    (v! color 1)))

;;; 003-death.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v! 0 0 0))
         (color (+ color (step .5
                               (* .5 (+ (x st) (y st)))))))
    (v! color 1)))


;;; 004-the_wall.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (v! 0 0 0))
         (color (+ color (stroke (x st) .5 .15))))
    (v! color 1)))

;;; 005-temperance.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (v! 0 0 0))
         (offset (* .15 (cos (* (y st) 3.1415))))
         (color (+ color (stroke (x st)
                                 (+ offset .28)
                                 .1)))
         (color (+ color (stroke (x st)
                                 (+ offset .5)
                                 .1)))
         (color (+ color (stroke (x st)
                                 (+ offset .72)
                                 .1))))
    (v! color 1)))

;;; 006-branch.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st     (/ (s~ gl-frag-coord :xy)
                    resolution))
         (color  (v! 0 0 0))
         (sdf    (+ .5 (* .5 (- (x st) (y st)))))
         (color  (+ color (stroke sdf .5 .1))))
    (v! color 1)))

;;; 007-thm.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st      (/ (s~ gl-frag-coord :xy)
                     resolution))
         (color   (v! 0 0 0))
         (sdf     (+ .5 (* .5 (- (x st) (y st)))))
         (color   (+ color (stroke sdf .5 .1)))
         (sdf-inv (* .5 (+ (x st) (y st))))
         (color   (+ color (stroke sdf-inv .5 .1))))
    (v! color 1)))

;;; 008-the_high_priestess.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st      (/ (s~ gl-frag-coord :xy)
                     resolution))
         (color   (v! 0 0 0))
         (color  (+ color (stroke (circle-sdf st)
                                  .5
                                  .05))))
    (v! color 1)))

;;; 009-the_moon.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (v! 0 0 0))
         (color (+ color (g-fill (circle-sdf st)
                                 .65)))
         (offset (v! .1 .05))
         (color (- color (g-fill (circle-sdf (- st offset))
                                 .5))))
    (v! color 1)))

;;; 010-the_emperor.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (v! 0 0 0))
         (sdf   (rect-sdf st (v! 1.0 1.0)))
         (color (+ color (stroke sdf .5 .125)))
         (color (+ color (g-fill sdf .1))))
    (v! color 1)))

;;; 011-hierophant.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   resolution))
         (color (v! 0 0 0))
         (rect  (rect-sdf st (v! 1.0 1.0)))
         ;; strips
         (color (+ color (g-fill rect .5)))
         (cross (cross-sdf st 1.0))
         (color (* color
                   (step .5 (fract (* 4.0
                                      cross)))))
         ;; remove strips
         (color (* color (step 1. cross)))
         ;; inner cross
         (color (+ color (g-fill cross .5)))
         ;; inner border
         (color (+ color (stroke rect .65 .05)))
         ;; outer border
         (color (+ color (stroke rect .75 .025))))
    (v! color 1)))

;;; 012-the_tower.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v! 0 0 0))
         (rect  (rect-sdf st (v! .5 1.0)))
         (diag  (* .5 (+ (x st) (y st))))
         (color (+ color (flip (g-fill rect .6)
                               (stroke diag .5 .01)))))
    (v! color 1)))

;;; 013-the_merge.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color  (v3! 0 0 0))
         (offset (v2! .15 0))
         (left  (circle-sdf (+ st offset)))
         (right (circle-sdf (- st offset)))
         (color (+ color (flip (stroke left .5 .05)
                               (g-fill right .525)))))
    (v! color 1)))

;;; 014-hope.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color  (v3! 0 0 0))
         (sdf (vesica-sdf st .2))
         (color (+ color (flip (g-fill sdf .5)
                               (step (* .5 (+ (x st) (y st)))
                                     .5)))))
    (v! color 1)))

;;; 015-the_temple.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color  (v3! 0 0 0))
         (st (v2! (x st)
                  (- 1.0 (y st))))
         (ts (v2! (x st) (- .82 (y st))))
         ;; upside-down triangle
         (color (+ color (g-fill (tri-sdf st) .7)))
         ;; inner triangle
         (color (- color (g-fill (tri-sdf ts) .36))))
    (v! color 1)))

;;; 016-the_sumit.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color  (v3! 0 0 0))
         (circle (circle-sdf (- st (v2! .0 .1))))
         (triangle (tri-sdf (+ st (v2! .0 .1))))
         (color (+ color (stroke circle .45 .1)))
         ;; make a cut
         (color (* color (step .55 triangle)))
         (color (+ color (g-fill triangle .45))))
    (v! color 1)))

;;; 017-diamond.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0 0 0))
         (sdf   (rhomb-sdf st))
         ;; inner rhomb
         (color (+ color (g-fill sdf .425)))
         ;; outer
         (color (+ color (stroke sdf .5 .05)))
         ;; outer-er
         (color (+ color (stroke sdf .6 .03))))
    (v! color 1)))

;;; 018-the_hermit.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (color (+ color (flip (g-fill (tri-sdf st) .5)
                               (g-fill (rhomb-sdf st) .4)))))
    (v! color 1)))

;;; 019-intuition.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (st (rotate st (radians -25)))
         (sdf (tri-sdf st))
         ;; cut the triangle
         (sdf (/ sdf (tri-sdf (+ st (v2! 0.0 0.2)))))
         (color (+ color (g-fill (abs sdf) .56))))
    (v! color 1)))

;;; 020-the_stone.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (st (rotate st (radians 45)))
         (color (+ color (g-fill (rect-sdf st (v2! 1.0))
                                 .4)))
         ;; 45° line
         (color (* color (- 1.0 (stroke (x st) .5 .02))))
         ;; -45°
         (color (* color (- 1.0 (stroke (y st) .5 .02)))))
    (v! color 1)))

;;; 021-mountains.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         ;; rotate
         (st (rotate st (radians -45)))
         (off .12)
         (s (v2! 1.0))
         ;; place 2 rectangles
         (color (+ color (g-fill (rect-sdf (+ st off) s)
                                 .2)))
         (color (+ color (g-fill (rect-sdf (- st off) s)
                                 .2)))
         (r (rect-sdf st s))
         ;; place 3rd rectangle that hides the prev
         (color (* color (step .33 r)))
         (color (+ color (g-fill r .3))))
    (v! color 1)))

;;; 022-the_shadow.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (st (rotate (v2! (x st) (- 1.0 (y st)))
                     (radians 45)))
         (s  (v2! 1.0))
         (color (+ color (g-fill (rect-sdf (- st .025) s)
                                 .4)))
         (color (+ color (g-fill (rect-sdf (+ st .025) s)
                                 .4)))
         (color (* color (step .38 (rect-sdf (+ st .025) s)))))
    (v! color 1)))

;;; 023-opposite.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (st (rotate st (radians -45)))
         (s (v2! 1.0))
         (o .05)
         (color (+ color
                   (flip (g-fill (rect-sdf (- st o) s) .4)
                         (g-fill (rect-sdf (+ st o) s) .4)))))
    (v! color 1)))

;;; 024-the_oak.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0 0 0))
         (st (rotate st (radians 45.)))
         (r1 (rect-sdf st (v2! 1.0)))
         (r2 (rect-sdf (+ st .15) (v2! 1.0)))
         (color (+ color (stroke r1 .5 .05)))
         (color (* color (step .325 r2)))
         ;;; adds a second rect
         (color (* (g-fill r1 .525) ;; this rm the outside part
                   (+ color (stroke r2 .325 .05))))
         ;; draws a smaller rect
         (color (+ color (stroke r2 .2 .05))))
    (v! color 1)))

;;; 025-ripples.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0 0 0))
         (st (- (rotate st (radians -45)) .08)))
    (for (i 0) (< i 4) (++ i)
         (let* ((r (rect-sdf st (v2! 1.0))))
           (incf color (v3! (stroke r .19 .04)))
           (incf st    (v2! .05))))
    (v! color 1)))

;;; 026-the_emperatris.frag
;;; !?
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (d1 (poly-sdf st 5))
         (ts (v2! (x st) (- 1.0 (y st))))
         (d2 (poly-sdf ts 5))
         (color (+ color (* (g-fill d1 .75)
                            (g-fill (fract (* 5.0 d1)) .5))))
         (color (- color (* (g-fill d1 .6)
                            (g-fill (fract (* 4.9 d2)) .45)))))
    (v! color 1)))

;;; 027-bundle.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (st (v! (y st) (x st)))
         ;; outer
         (color (+ color (stroke (hex-sdf st) .6 .1)))
         ;; inner things
         (color (+ color (g-fill (hex-sdf (- st (v! -.06 -.1)))
                                 .15)))
         (color (+ color (g-fill (hex-sdf (- st (v! -.06 .1)))
                                 .15)))
         (color (+ color (g-fill (hex-sdf (- st (v! .11 .0)))
                                 .15))))
    (v! color 1)))

;;; 028-the_devil.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (color (+ color (stroke (circle-sdf st) .8 .05)))
         ;; ? rotates?
         (st    (v! (x st) (- 1.0 (y st))))
         (s     (star-sdf (s~ st :yx) 5 .1))
         ;; cuts circle in 5 places
         (color (* color (step .7 s)))
         ;; place a inner star
         (color (+ color (stroke s .4 .1))))
    (v! color 1)))

;;; 029-the_sun.frag
;; !?!
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         (bg (star-sdf st 16 .1))
         (color (+ color (g-fill bg 1.3)))
         (l 0.0))
    (for (i 0) (< i 8) (++ i)
         (let* ((xy (rotate st (* i 0.785398)))
                (xy (v! (x xy) (- (y xy) .3)))
                (tri (poly-sdf xy 3)))
           (setf color (+ color (g-fill tri .3)))
           (incf l (stroke tri .3 .03))))
    (let* (;; draws the inner strokes
           (color (* color (- 1.0 l)))
           ;; inner poly
           (c (poly-sdf st 8))
           (color (- color (stroke c .15 .04))))
      (v! color 1))))


;;; 030-the_star.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0 0 0))
         ;; rays
         (color (+ color (stroke (rays-sdf st 8)
                                 .5 .15)))
         ;; stars
         (inner (star-sdf (s~ st :xy) 6 .09))
         (outer (star-sdf (s~ st :yx) 6 .09))
         (color (* color (step .7 outer)))
         (color (+ color (g-fill outer .5)))
         (color (- color (stroke inner .25 .06)))
         (color (+ color (stroke outer .6 .05))))
    (v! color 1)))

;;; 031-Jugment.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0.0))
         (color (+ color
                   (flip (stroke (rays-sdf st 28) .5 .2)
                         (g-fill (y st) .5))))
         (rect  (rect-sdf st (v2! 1.0)))
         (color (* color (step .25 rect)))
         (color (+ color (g-fill rect .2))))
    (v! color 1)))

;;; 032-the_fortune.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0.0))
         (sdf (poly-sdf (s~ st :yx) 8))
         (color (+ color (g-fill sdf .5)))
         (color (* color (stroke (rays-sdf st 8) .5 .2)))
         ;; inner poly hole
         (color (* color (step .27 sdf)))
         ;; inner poly border
         (color (+ color (stroke sdf .2 .05)))
         ;; outer poly border
         (color (+ color (stroke sdf .6 .1))))
    (v! color 1)))

;;; 033-vision.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0.0))
         (v1 (vesica-sdf st .5))
         (st2 (+ (s~ st :yx) (v2! .04 .0)))
         (v2 (vesica-sdf st2 .7))
         ;; side to side
         (color (+ color (stroke v2 1. .05)))
         ;; eyelid
         (color (+ color (* (g-fill v2 1.)
                            (stroke (circle-sdf st) .3 .05))))
         ;; rays as a oval (v1)
         (color (+ color (* (g-fill (rays-sdf st 50) .2)
                            (g-fill v1 1.25)
                            (step 1. v2)))))
    (v! color 1)))

;;; 034-the_lovers.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0.0))
         (color (+ color (g-fill (heart-sdf st) .5)))
         (color (- color (stroke (poly-sdf st 3) .15 .05))))
    (v! color 1)))

;;; 035-magician.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0.0))
         (st (+ .5 (* 1.1912 (- st .5))))
         (st (v! (flip (x st) (step .5 (y st)))
                 (y st)))
         (offset (v! .15 .0))
         (left  (circle-sdf (+ st offset)))
         (right (circle-sdf (- st offset)))
         (color (+ color (stroke left .4 .075)))
         (color (bridge color right .4 .075)))
    (v! color 1)))

;;; 036-the_link.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0f0))
         ;; this helps keeping things inside
         (st (+ .5 (* 1.1912 (- st .5))))
         (st (s~ st :yx))
         (st (v! (mix (- 1f0 (x st))
                      (x st)
                      (step .5 (y st)))
                 (y st)))
         (o (v2! .1 0f0))
         (s (v2! 1f0))
         (a (radians 0f0))
         (l (rect-sdf (rotate (+ st o) a) s))
         (r (rect-sdf (rotate (- st o) a) s))
         (color (+ color (stroke l .3 .1)))
         (color (bridge color r .3 .1))
         (color (+ color (g-fill (rhomb-sdf (abs (- (s~ st :yx)
                                                    (v2! .0 .5))))
                                 .1))))
    (v! color 1)))

;;; 037-holding_together.frag
(defun-g frag ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (st (+ .5 (* 1.1912 (- st .5))))
         (color (v3! 0.0))
         (st (v! (mix (- 1.0 (x st))
                      (x st)
                      (step .5 (y st)))
                 (y st)))
         (o (v2! .05 .0))
         (s (v2! 1.0))
         (a (radians 45))
         (l (rect-sdf (rotate (+ st o) a) s))
         (r (rect-sdf (rotate (- st o) (* -1 a)) s))
         (color (+ color (stroke l .145 .098)))
         (color (bridge color r .145 .098)))
    (v! color 1)))
