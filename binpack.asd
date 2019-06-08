(asdf:defsystem :binpack
  :description "Rectangle packer for sprite/texture atlases"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>, Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :depends-on (alexandria)
  :serial t
  :components ((:file "packages")
               (:file "binpack")))

