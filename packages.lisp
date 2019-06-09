(defpackage binpack
  (:use :cl)
  (:export #:pack
           #:packing-failed
           #:auto-pack
           #:rect
           #:id
           #:x
           #:y
           #:w
           #:h
           #:with-rect
           #:rect-initargs
           #:expand-and-retry
           #:expand-and-continue))
