(in-package #:binpack/2)


;;; packing shape control. By default, most of the algorithms tend to
;;; pack along entire left and bottom edges, leaving upper right empty
;;; if given an oversided initial bin size. Shaping classes allow
;;; picking an initial size and weighting various expansion options to
;;; favor packings that fill the final extents efficiently, while
;;; maintaining constraints like "multiple of X" or "power of 2"

(defclass shaping ()
  ;; initial width/height of packing area. Square root of total size
  ;; of rects to pack is reasonable initial estimate if known.
  ((w :initform 16 :accessor w :initarg :w)
   (h :initform 16 :accessor h :initarg :h)
   ;; if set, try to maintain more square shape
   (square :initform t :accessor square :initarg :square)))

;; expand active packing area to powers of 2
(defclass shaping-po2 (shaping)
  ())

(defmethod initialize-instance :after ((o shaping-po2) &key)
  (setf (w o) (ceilingp2 (w o)))
  (setf (h o) (ceilingp2 (h o))))

(defmethod shaping-penalty ((s shaping-po2) p)
  (let ((w (w s))
        (h (h s)))
    (setf (penalty-multiplier p)
          (let ((x (+ (x p) (w p)))
                (y (+ (y p) (h p))))
            (cond
              ;; fits, no penalty
              ((and (<= x w)
                    (<= y h))
               0)
              ;; otherwise calculate penalty = # of pixels expanded
              (t
               (let* ((a1 (* w h))
                      (w2 (max w (ceilingp2 x)))
                      (h2 (max h (ceilingp2 y)))
                      (a2 (* w2 h2))
                      (sp (if (square s)
                              (/ (max w2 h2)
                                 (min w2 h2))
                              1)))
                 (if (= a1 a2)
                     (break "??")
                     (* sp (+ 1 (/ (- a2 a1) a1)))))))))))

(defmethod shaping-add ((s shaping-po2) p)
  (let* ((x (+ (x p) (w p)))
         (y (+ (y p) (h p)))
         (w2 (max (w s) (ceilingp2 x)))
         (h2 (max (h s) (ceilingp2 y))))
    (when (or (/= w2 (w s))
              (/= h2 (h s)))
      (format t "expand from ~sx~s to ~sx~s~%"
              (w s) (h s) w2 h2))
    (setf (w s) w2
          (h s) h2)))


;; expand active packing area by multiples of DX,DY
(defclass shaping-quantized (shaping)
  ((dx :initform 4 :initarg :dx :accessor dx)
   (dy :initform 4 :initarg :dy :accessor dy)))

(defun ceilingn (x n)
  (* n (ceiling x n)))

(defmethod initialize-instance :after ((o shaping-quantized) &key)
  (assert (>= (dx o) 1))
  (assert (>= (dy o) 1))
  (setf (w o) (ceilingn (w o) (dx o)))
  (setf (h o) (ceilingn (h o) (dy o))))

(defmethod shaping-penalty ((s shaping-quantized) p)
  (let ((w (w s))
        (h (h s)))
    (setf (penalty-multiplier p)
          (let ((x (+ (x p) (w p)))
                (y (+ (y p) (h p))))
            (cond
              ;; fits, no penalty
              ((and (<= x w)
                    (<= y h))
               0)
              ;; otherwise calculate penalty = # of pixels expanded
              (t
               (let* ((a1 (* w h))
                      (w2 (max w (ceilingn x (dx s))))
                      (h2 (max h (ceilingn y (dy s))))
                      (a2 (* w2 h2))
                      (sp (if (square s)
                              (/ (max w2 h2)
                                 (min w2 h2))
                              1)))
                 (if (= a1 a2)
                     (break "??")
                     (* sp (+ 1 (/ (- a2 a1) a1)))))))))))

(defmethod shaping-add ((s shaping-quantized) p)
  (let* ((x (+ (x p) (w p)))
         (y (+ (y p) (h p)))
         (w2 (max (w s) (ceilingn x (dx s))))
         (h2 (max (h s) (ceilingn y (dy s)))))
    (setf (w s) w2
          (h s) h2)))

;; not useful, just implemented as a test: pack in a circular area
(defclass shaping-circle (shaping)
  ())

(defmethod shaping-penalty ((s shaping-circle) p)
  (setf (penalty-multiplier p)
        (let ((x (+ (x p) (w p)))
              (y (+ (y p) (h p))))
          (+ (expt x 2) (expt y 2)))))

(defmethod shaping-add ((s shaping-circle) p))


;; expand active packing area by cells of a DXxDY grid, indended for
;; sparse textturing where for expanding a 1024 long vertical edge
;; along X by 256, then only using the first 256 Y coords along that
;; edge might only need 1 more page of texture memory, but expanding
;; by 16 and using the entire 1024 Y range would require adding 4
;; 256x256 pages
(defclass shaping-sparse (shaping)
  ((mask :accessor mask :initform (make-array '(256 256)
                                              :element-type 'bit
                                              :initial-element 0))
   (dx :initform 256 :initarg :dx :accessor dx)
   (dy :initform 256 :initarg :dy :accessor dy)))

(defmethod shaping-penalty ((s shaping-sparse) p)
  (let ((pm 0))
    (loop for x from (floor (x p) (dx s))
          upto (ceiling(+ (x p) (w p)) (dx s))
          do (loop for y from (floor (y p) (dy s))
                   upto (ceiling (+ (y p) (h p)) (dy s))
                   do (incf pm
                            (if (and (array-in-bounds-p (mask s) x y)
                                     (= 1 (aref (mask s) x y)))
                                0
                                1))))
    (setf (penalty-multiplier p)
          pm)))

(defmethod shaping-add ((s shaping-sparse) p)
  (loop for x from (floor (x p) (dx s))
        upto (ceiling (+ (x p) (w p)) (dx s))
        do (loop for y from (floor (y p) (dy s))
                 upto (ceiling (+ (y p) (h p)) (dy s))
                 do (unless (array-in-bounds-p (mask s) x y)
                      (setf (mask s)
                            (adjust-array (mask s)
                                          (list
                                           (max (1+ x)
                                                (array-dimension (mask s) 0))
                                           (max (1+ y)
                                                (array-dimension (mask s) 1))))))
                    (setf (aref (mask s) x y) 1))))

;;; incremental/online API
;;
;; call START-PACK to get a packing state, then pass that and a rect
;; to PACK-1 to add that rect to packing, get back same RECT with X,Y,PAGE
;; set, or NIL if it couldn't fit in requested max width/height.

(defun add-page (state)
  (when (and (plusp (fill-pointer (state state)))
             (not (page-policy state)))
    (error "adding page to non-paged pack-state?"))
  (ecase (algorithm state)
    (:chazelle (vector-push-extend (init-hole (width state) (height state))
                                   (state state))))
  state)

(defun start-pack (max-width max-height
                   &key (packer :good)
                     (growth-policy)
                     (page-policy))
  "start packing with maximum size MAX-WIDTH by MAX-HEIGHT. PACKER
should be :GOOD for default high-quality packer, :FAST for default
high-speed packer (currently same as :GOOD) or name of a specific
algorithm. GROWTH-POLICY controls how packer expands when trying to
automatically determine best bin size, and should be one of the
SHAPING-* classes above. if PAGE-POLICY is set, PACK-1 will allocate
new pages when rect doesn't fit, and place things in existing pages
according to PAGE-POLICY. :FIRST-FIT will place rect on first page it
fits in. :BEST-FIT will check all pages for best fit, :LAST-PAGE will
only look at last page, so is faster when there are many pages, but
will usually leave more unused space on earlier pages."
  (ecase packer
    ;; good quality and reasonable speed
    ((:good :fast :chazelle)
     (add-page
      (make-instance 'pack-state
                     :state (make-array 0
                                        :fill-pointer 0
                                        ::adjustable t
                                        :initial-element
                                        nil)
                     :shaping growth-policy
                     :page-policy page-policy
                     :algorithm :chazelle
                     :width max-width
                     :height max-height)))
    ;; todo: update maxrects to use new SHAPING api and similar
    ;; autosizing and add it back in
    #++
    (:maxrects
     )
    ;; todo: implement a "fast" packer
    ;; (skyline, https://blackpawn.com/texts/lightmaps/default.html ,
    ;; or https://github.com/TeamHypersomnia/rectpack2D/ )?
    #++
    (:fast
     )))

(defun reset-pack (state width height)
  (setf (slot-value state 'width) width)
  (setf (slot-value state 'height) height)
  (fill (state state) nil)
  (setf (fill-pointer (state state)) 0)
  (add-page state))

;; internal functions for multipage packing
(defun pack-1/bf (rect state)
  ;; "best fit", always looks at all pages, so slowest but usually
  ;; best packing
  (break "todo best fit ~s ~s" rect state)
)
(defun pack-1/@ (rect state &optional (page (1- (fill-pointer (state state)))))
  ;; "pack on last (or specified) page". fastest since it only look at
  ;; one page, but wastes more space on earlier pages (might not be
  ;; too wasteful if rects are sorted smaller first though?)
  (ecase (algorithm state)
    (:chazelle
     (when (aref (state state) page)
       (multiple-value-bind (nh x y)
           (place (aref (state state) page) (w rect) (h rect)
                  :shaping (shaping state))
         (setf (aref (state state) page) nh)
         (if (and x y)
             (setf (page rect) page
                   (x rect) x
                   (y rect) y)
             (setf (page rect) nil
                   (x rect) nil
                   (y rect) nil))))))
  rect)

(defun pack-1/ff (rect state)
  ;; "first fit", stops as soon as it finds a page that fits, so
  ;; probably a bit faster than best-fit, but doesn't pack as well
  (loop for i below (fill-pointer (state state))
        do (pack-1/@ rect state i)
        until (page rect))
  rect)


;;
(defun pack-1 (rect state)
  ;; make sure rect is initialized so we can tell if it didn't pack
  (setf (x rect) nil
        (y rect) nil
        (page rect) nil)
  ;; can't fit, don't even try packing it or adding pages
  (when (or (> (w rect) (width state))
            (> (h rect) (height state)))
    (break "no fit")
    (return-from pack-1 rect))
  (ecase (page-policy state)
    ((nil)
     #++(break "nil")
     (pack-1/@ rect state 0))
    (:last-page
     (break "lp")
     (let ((p (pack-1/@ rect state)))
       (when (not (page p))
         (add-page state)
         (setf p (pack-1/@ rect state)))
       p))
    (:first-fit
     (pack-1/ff rect state)
     (unless (page rect)
       (add-page state)
       (pack-1/@ rect state))
     rect)
    (:best-fit
     (pack-1/bf rect state)))
  #++(format t "packed ~s,~s -> ~s,~s@~s~%" (w rect) (h rect) (x rect) (y rect) (page rect))
  rect)


;;; batch/offline API
;;
;; pass list of RECTs to PACK or AUTO-PACK, with options to control
;; auto-sizing etc, get back a list of RECTS, with X,Y,PAGE set for
;; all that were successfully packed. AUTO-PACK calls PACK a few times
;; with different settings to try to get a better packing, and returns
;; the best.

(defun pack (rects max-width max-height
             &key (packer :good)
               (growth-policy)
               (sort 'sort-rects/w+h-desc)
               (fill-policy :stop)
               (page-policy nil))
  "Pack RECTS into page(s) up to MAX-WIDTH x MAX-HEIGHT, using
algorithm PACKER (:good or :fast or specific algorithm).

 Use SHAPING-* instance in GROWTH-POLICY to try to pack into
particular regions first if specified.

If SORT is specified, it is a function to use to sort (a copy of)
RECTS before packing.

FILL-POLICY should be :STOP or :SKIP. If stop, packing stops at first
rect that doesn't fit. If :SKIP, it will try to pack all rectangles,
skipping any that don't fit.

If PAGE-POLICY is non-NIL, it should be :FIRST-FIT, :BEST-FIT, or
:LAST-PAGE to control how rects are packed into multiple
pages.  (Might still hit :STOP or :SKIP from FILL-POLICY if a rect
doesn't fit in MAX-WIDTH x MAX-HEIGHT, even with multiple pages)

Returns (sorted copy of) RECTS, with X,Y,PAGE set for all elements
that were packed, and cleared to NIL for any elements that failed to
pack.

Returns # of pages allocated as 2nd value, and NIL or # of rects that failed to pack as 3rd value
"
  (let ((state (start-pack max-width max-height
                           :packer packer
                           :growth-policy growth-policy
                           :page-policy page-policy))
        (unpacked 0))
    (when sort
      (setf rects (funcall sort (copy-list rects))))
    (loop for (rect . rest) on rects
          for p = (pack-1 rect state)
          unless (page p)
            do (ecase fill-policy
                 ;; stop packing
                 (:stop
                  ;; clear X,Y,PAGE for rest of rects, just to be
                  ;; consistent
                  #++(break "stop")
                  (incf unpacked)
                  (loop for r in rest
                        do (incf unpacked)
                           (setf (x r) nil (y r) nil (page r) nil))
                  (loop-finish))
                 ;; try packing remaining rects
                 (:skip
                     #++(break "skip")
                     (incf unpacked))))
    (values rects
            (fill-pointer (state state))
            (if (zerop unpacked) nil unpacked))))


(defun calculate-page-bounds (rects pages)
  (let ((mx (make-array pages :element-type 'fixnum :initial-element 0))
        (my (make-array pages :element-type 'fixnum :initial-element 0))
        (a 0)
        (u 0))
   (loop for r in rects
         for p = (page r)
         for x = (+ (x r) (w r))
         for y = (+ (y r) (h r))
         when (page r)
           do (setf (aref mx p) (max (aref mx p) x))
              (setf (aref my p) (max (aref my p) y))
         else do (incf u))
    (let ((m (loop for p below pages
                   collect (list (aref mx p) (aref my p))
                   do (incf a (* (aref mx p) (aref my p))))))
      (list* a u m))))


(defun copy-rects (rects)
  (loop for r in rects
        collect (apply #'make-instance (class-of r)
                       (rect-initargs r))))

(defun auto-pack (rects max-width max-height
                  &key (packer :good)
                    (growth-policy)
                    (sorts '(sort-rects/w+h-desc
                             sort-rects/longest-side-desc
                             sort-rects/perimeter-desc
                             sort-rects/area-desc
                             sort-rects/aspect*area-desc))
                    (multipage t))
  (let ((best nil)
        (best-area 0)
        (best-count 0))
   (loop for s in sorts
         for (pack pages)
           = (multiple-value-list
              (pack (copy-rects rects) max-width max-height
                    :sort s
                    :packer packer
                    :growth-policy growth-policy
                    :fill-policy :skip
                    :page-policy (if multipage :best-fit nil)))
         for (a u m) = (calculate-page-bounds pack pages)
         when (or (not best)
                  (< m best-count)
                  (and (<= m best-count)
                       (< a best-area)))
           do (setf best pack
                    best-count m
                    best-area a))
    best))


#++
(ql:quickload 'binpack)
