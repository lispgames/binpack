(defsystem #:binpack-test/common
  :depends-on (#:binpack #:parachute)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :binpack-common))
  :components ((:file "test-common")
               (:file "common-test")))

(defsystem #:binpack-test
  :depends-on (#:binpack #:parachute)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :binpack-test))
  :components ((:file "test-common")
               (:file "test1")))

(defsystem #:binpack-test/2
  :depends-on (#:binpack/2 #:parachute)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :binpack-test/2))
  :components ((:file "test-common")
               (:file "test1")))
