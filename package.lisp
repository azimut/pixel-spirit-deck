(uiop:define-package pixel-spirit-deck
    (:use #:cl #:cepl #:rtg-math #:vari #:nineveh
          :livesupport)
  (:export #:defcard ;; macro to define fragment "frag" shader
           ;; misc
           #:aastep
           #:bridge
           #:flip
           #:g-fill
           #:rotate
           #:scale
           #:stroke
           ;; sdf's
           #:circle-sdf
           #:cross-sdf
           #:flower-sdf
           #:heart-sdf
           #:hex-sdf
           #:poly-sdf
           #:rays-sdf
           #:rect-sdf
           #:rhomb-sdf
           #:spiral-sdf
           #:star-sdf
           #:tri-sdf
           #:vesica-sdf))
