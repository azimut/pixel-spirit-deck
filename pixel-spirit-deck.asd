(asdf:defsystem "pixel-spirit-deck"
  :description "cepl/varjo translation of PixelSpiritDeck"
  :author "azimut <azimut.github@protonmail.com>"
  :license "BSD-3"
  :serial t
  :depends-on (
               #:swank
               #:cepl.sdl2
               #:livesupport
               #:skitter
               #:cepl.skitter.sdl2
               #:rtg-math
               #:rtg-math.vari
               #:dendrite
               #:nineveh)
  :components ((:file "package")))
