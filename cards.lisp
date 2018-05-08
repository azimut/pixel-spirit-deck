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
