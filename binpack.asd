(asdf:defsystem :binpack
  :description "Rectangle packer for sprite/texture atlases"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>, Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :depends-on (alexandria)
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op binpack-test)))
  :components ((:file "packages")
               (:file "common")
               (:file "maxrects")
               (:file "binpack")))


(asdf:defsystem :binpack/2
  :description "Rectangle packer for sprite/texture atlases (new API)"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>, Pavel Korolev <dev@borodust.org>, Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :depends-on (alexandria)
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op binpack-test/2)))
  :components ((:file "packages")
               (:file "common")
               (:file "maxrects")
               (:file "chazelle")
               (:file "binpack2")))

