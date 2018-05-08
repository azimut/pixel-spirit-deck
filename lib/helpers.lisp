(in-package :pixel-spirit-deck)

#|
 Copyright (c) 2017 Patricio Gonzalez Vivo ( http://www.pixelspiritdeck.com )
 All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are
 met:
 
 Redistributions of source code must retain the above copyright notice,
 this list of conditions and the following disclaimer.
 
 Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; (defun-g aastep ((threshold :float) (value :float))
;;   "Generate a step function by comparing two values"
;;   (step threshold value))

(defun-g aastep ((threshold :float) (value :float))
  "Generate a step function by comparing two values"
  (let ((afwidth (* 0.7 (length (v! (d-fdx value)
                                    (d-fdy value))))))
    (smoothstep (- threshold afwidth) (+ threshold afwidth) value)))

(defun-g stroke ((x :float) (size :float) (w :float))
  (let ((d (- (aastep size (+ x (* w .5)))
              (aastep size (- x (* w .5))))))
    (clamp d 0.0 1.0)))

(defun-g circle-sdf ((st :vec2))
  (* 2.0 (length (- st .5))))

(defun-g g-fill ((x :float) (size :float))
  "fills, takes 2 floats"
  (- 1.0 (aastep size x)))

(defun-g rect-sdf ((st :vec2) (s :vec2))
  "rectangle sdf, takes 2 vec2"
  (let ((st (- (* st 2.0) 1.0)))
    (max (abs (/ (x st) (x s)))
         (abs (/ (y st) (y s))))))

;;; 011

(defun-g cross-sdf ((st :vec2) (s :float))
  (let ((size (v! .25 s)))
    (min (rect-sdf st size)
         (rect-sdf st (s~ size :yx)))))

;;; 012

(defun-g flip ((v :float) (pct :float))
  (mix v (- 1.0 v) pct))

;;; 014

(defun-g vesica-sdf ((st :vec2) (w :float))
  (let ((offset (v2! (* .5 w) 0.0)))
    (max (circle-sdf (- st offset))
         (circle-sdf (+ st offset)))))

;;; 015

(defun-g tri-sdf ((st :vec2))
  (let ((st (* 2.0 (- (* 2.0 st) 1.0))))
    (max (+ (* (abs (x st)) 0.866025)
            (* (y st) .5))
         (* 0.5 (* -1.0 (y st))))))

;; 017

(defun-g rhomb-sdf ((st :vec2))
  (max (tri-sdf st)
       (tri-sdf (v2! (x st)
                     (- 1 (y st))))))

;;; 019

(defun-g rotate ((st :vec2) (a :float))
  (let ((st (* (- st .5)
               (mat2 (cos a) (* -1 (sin a))
                     (sin a) (cos a)))))
    (+ st .5)))

;;; 026

(defun-g poly-sdf ((st :vec2) (V :int))
  (let* ((st (- (* 2.0 st) 1.0))
         (a  (+ 3.1415 (atan (x st) (y st))))
         (r  (length st))
         (v  (/ 6.283185 (float V))))
    (* r (cos (- (* v (floor (+ .5 (/ a v)))) a)))))

;;; 027

(defun-g hex-sdf ((st :vec2))
  (let ((st (abs (- (* 2.0 st) 1.0))))
    (max (abs (y st))
         (+ (* (x st) 0.866025)
            (* (y st) 0.5)))))

;;; 028

(defun-g star-sdf ((st :vec2) (V :int) (s :float))
  (let* ((st  (- (* 4.0 st) 2.0))
         (a   (/ (atan (y st) (x st))
                 6.283185))
         (seg (* a (float V)))
         (a   (+ (/ (+ (floor seg) .5)
                    (float V))
                 (mix s (* -1 s)
                      (step .5 (fract seg)))))
         (a   (* a 6.283185)))
    (abs (dot (v2! (cos a) (sin a)) st))))

;;; 030

(defun-g rays-sdf ((st :vec2) (N :int))
  (let ((st (- st .5)))
    (fract (* N (/ (atan (y st) (x st))
               6.2831)))))

;;; 034

(defun-g heart-sdf ((st :vec2))
  (let* ((st (- st (v2! .5 .8)))
         (r  (* 5.0(length st)))
         (st (normalize st)))
    (- r (+ 1.26 (- (/ (* (y st) (pow (abs (x st)) .67))
                       (+ 1.5 (y st)))
                    (* 2 (y st)))))))

;;; 035

(defun-g bridge ((c :vec3) (d :float) (s :float) (w :float))
  (let ((c (* c (- 1f0 (stroke d s (* 2f0 w))))))
    (+ c (stroke d s w))))
