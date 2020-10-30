(defpackage binpack/common
  (:use :cl)
  (:export #:rect
           #:id
           #:w
           #:h
           #:x
           #:y
           #:page
           #:with-rect
           #:point-in-rect
           #:rect-initargs
           #:packing-failed
           #:pack-state
           #:state
           #:algorithm
           #:width
           #:height
           #:shaping
           #:page-policy
           #:sort-rects/longest-side-desc
           #:sort-rects/area-desc
           #:sort-rects/width-desc
           #:sort-rects/height-desc
           #:sort-rects/w+h-desc
           #:sort-rects/perimeter-desc
           #:sort-rects/aspect*area-desc
           #:total-pixels))

(defpackage binpack
  (:use :cl :binpack/common)
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
           #:expand-and-continue
           #:start-pack
           #:pack-1
           #:reset-pack))


(defpackage binpack/2
  (:use :cl :binpack/common)
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
           #:expand-and-continue
           #:start-pack
           #:pack-1
           #:reset-pack))
