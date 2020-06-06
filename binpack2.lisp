(in-package #:binpack)
#++(ql:quickload 'binpack)

;; based on Chazelle's "Bottom-Left Bin-Packing Heuristic: An
;; Efficient Implementation" (
;; https://www.cs.princeton.edu/~chazelle/pubs/blbinpacking.pdf ),
;; with heuristics from http://clb.demon.fi/files/RectangleBinPack.pdf
;; and with options for controlling shape of packing

;;; goals for shaping heuristics:
;;
;; for a given set of rects, we want them efficiently packed into some
;; rectangle, but might not care about the actual size/shape of the
;; rectangle beyond some much larger constraints like maximum image or
;; texture sizes. Just packing into that maximum might end up with a
;; wide/flat packing, or one that mostly uses 2 edges and leaves the
;; middle empty.
;;
;; instead, we give options for biasing the packing while maintaining
;; the "bottom left" constraint, and some modifiers for common
;; constraints like "multiple of X" widths etc.
;;
;;; uses cases:
;;   fixed width or height, pack into minimum for other dimension
;;
;;   pack into minimal square, limited to 4096 or whatever in either
;;   dimension
;;
;;   same as above, but with width limited to multiples of 4 or 8
;;
;;   pack into minimal rectangle with power-of-2 dimensions?
;;

;;; biasing heuristics:
;; given a starting width/height (estimated from total size of rects
;; if possible, or specified manually), and quantization (multiples of
;; 8 etc) for each axis, we add a penalty to the original packing
;; heuristic for any packing which would extend the current
;; (quantized) width or height, and further penalize those that would
;; push the aspect ratio further from that of the starting dimensions.
;;
;; or possibly bias based on amount of area added to total size when a
;; packing extends past current extents? possibly scaled by starting
;; aspect ratio if maintaining that is desired? added area without
;; aspect ratio would probably be best for power-of-2 packings

;;; optimization options
;; (ideally try to make them interruptable, for example add a "stop
;;  and return best result so far" restart, so user can let it run as
;;  long as they want, or interrupt it early)
;;
;; retry pack with fixed sizes a few sizes smaller than final pack?
;; (size affects packing, so can't binary search or stop when it
;; fails, have to do linear search and try a few past first failure in
;; case they get luckier)
;;
;; pack, then remove anything above or to right of a gap, then repack
;; those (possibly with different heuristics/sorting/etc)
;;
;; retry pack a few times with some randomization of sorting,
;; heuristic, and packing choices?
;;
;; variant of full retry: add some restart points partway to allow
;; backtracking, with some estimate of quality of partial pack for
;; doing a search (max dimensions, gaps/density, ?)

;;; other features to add:
;;
;; multipage packing?
;;
;; pack-from-center? (divide into quadrants and pack all 4 at once
;; from origin (possibly with options for overlapping axis?))
;;
;; mode that reuses the search to try to pack multiple rects of same
;; width at once?

(defclass dll ()
  ;; doubly linked list links
  ((next :accessor dll-next)
   (prev :accessor dll-prev)))

(defmethod initialize-instance :after ((o dll) &key &allow-other-keys)
  (unless (slot-boundp o 'next)
    (setf (dll-next o) o))
  (unless (slot-boundp o 'prev)
    (setf (dll-prev o) o)))
(defun insert-after (prev new)
  (cond
    (prev
     (setf (dll-prev new) prev)
     (setf (dll-next new) (dll-next prev))
     (setf (dll-prev (dll-next prev)) new)
     (setf (dll-next prev) new))
    (t
     (setf (dll-prev new) new)
     (setf (dll-next new) new)))
  new)

(defun delete-node (node)
  ;; returns NEXT, or NIL if last node was removed
  (cond
    ((not (dll-next node))
     nil)
    ((eql (dll-next node) node)
     (setf (dll-next node) nil
           (dll-prev node) nil)
     nil)
    (t
     (let ((next (dll-next node)))
       (setf (dll-prev next) (dll-prev node)
             (dll-next (dll-prev node)) next)
       (setf (dll-next node) nil
             (dll-prev node) nil)
       next))))

(defmacro %do-dll ((node dll endp iterator) &body body)
  (alexandria:with-gensyms (start)
    (alexandria:once-only (dll)
      `(loop with ,start = ,dll
             for ,node = ,dll then (,iterator ,node)
             for ,endp = (eql ,start (,iterator ,node))
             repeat 100
             do (progn ,@body)
             until (eql ,start (,iterator ,node))))))

(defmacro do-dll/next ((node dll &optional endp) &body body)
  `(%do-dll (,node ,dll ,endp dll-next) ,@body))

(defmacro do-dll/prev ((node dll &optional endp) &body body)
  `(%do-dll (,node ,dll ,endp dll-prev) ,@body))

(defun dll-length (dll)
  (let ((a 0))
    (do-dll/next (n dll end)
      (incf a))
    a))

(defclass deq-entry (dll)
  ((v :reader deq-v :initarg :v)))

(defclass deq ()
  ((front :accessor %deq-front :initform nil)
   (back :accessor %deq-back :initform nil)))

(defun make-deq ()
  (make-instance 'deq))
(defun deq-empty-p (deq)
  (null (%deq-front deq)))
(defun empty-deq (deq)
  (setf (%deq-front deq) nil)
  (setf (%deq-back deq) nil))

(defun top1 (deq)
  (when (%deq-front deq)
    (deq-v (%deq-front deq))))

(defun top2 (deq)
  (when (%deq-back deq)
    (deq-v (%deq-back deq))))

(defun pop1 (deq)
  (when (%deq-front deq)
    (setf (%deq-front deq) (delete-node (%deq-front deq)))
    (unless (%deq-front deq)
      (setf (%deq-back deq) nil))))

(defun pop2 (deq)
  (when (%deq-front deq)
    (let ((b2 (delete-node (%deq-back deq))))
      (setf (%deq-back deq) (when b2 (dll-prev (%deq-front deq)))))
    (unless (%deq-back deq)
      (setf (%deq-front deq) nil))))

(defun push1 (n deq)
  (let ((node (make-instance 'deq-entry :v n)))
    (if (deq-empty-p deq)
        (setf (%deq-front deq) node
              (%deq-back deq) node)
        (setf (%deq-front deq)
              (insert-after (%deq-back deq) node)))))

(defun push2 (n deq)
  (let ((node (make-instance 'deq-entry :v n)))
    (if (deq-empty-p deq)
        (setf (%deq-front deq) node
              (%deq-back deq) node)
        (progn
          (insert-after (%deq-back deq) node)
          (setf (%deq-back deq) (dll-prev (%deq-front deq)))))))
;;
(defclass hole-vertex (dll)
  ((x :accessor hv-x :initform 0 :initarg :x)
   (y :accessor hv-y :initform 0 :initarg :y)
   ;; QN and QW links for top of left-notch
   (n :accessor hv-n :initform nil :initarg :qn)
   (w :accessor hv-w :initform nil :initarg :qw)
   ;; link back from QN,QW to top of left-notch
   (q :accessor hv-q :initform nil :initarg :q)
   ;; if this is part of a vertical edge, what part it is (otherwise,
   ;; should be a QN point on a horizontal edge)
   (classify :accessor hv-classify :initform nil)))

(defmacro %do-hole-vertices ((node dll dx dy endp iterator) &body body)
  (alexandria:with-gensyms (prev n)
    `(let ((,prev nil))
       (%do-dll (,node ,dll ,endp ,iterator)
         (let ((,dx (if ,prev (- (hv-x ,node) (hv-x ,prev))))
               (,dy (if ,prev (- (hv-y ,node) (hv-y ,prev)))))
           (setf ,prev ,node)
           (flet ((jump (,n)
                    (setf ,node (,(ecase iterator
                                    (dll-prev 'dll-next)
                                    (dll-next 'dll-prev))
                                 ,n))))
             (declare (ignorable #'jump))
             ,@body))))))

(defmacro do-hole-vertices/next ((node dll dx dy &optional endp) &body body)
  `(%do-hole-vertices (,node ,dll ,dx ,dy ,endp dll-next) ,@body))

(defmacro do-hole-vertices/prev ((node dll dx dy &optional endp) &body body)
  `(%do-hole-vertices (,node ,dll ,dx ,dy ,endp dll-prev) ,@body))

(defun up-dir (v)
  (cond
    ((< (hv-y v) (hv-y (dll-prev v)))
     'dll-prev)
    ((< (hv-y v) (hv-y (dll-next v)))
     'dll-next)
    (t nil)))

(defun down-dir (v)
  (cond
    ((> (hv-y v) (hv-y (dll-prev v)))
     'dll-prev)
    ((> (hv-y v) (hv-y (dll-next v)))
     'dll-next)
    (t nil)))


(defun extend-edge-up (v dir)
  ;; find local maximum of Y coord along this edge
  (unless dir
    (break "tried to extend horizontal sub-edge up?")
    (return-from extend-edge-up v))
  (loop for n = (funcall dir v)
        while (and (= (hv-x v) (hv-x n))
                   (< (hv-y v) (hv-y n)))
        do (setf v n))
  v)

(defun extend-edge-down (v dir)
  ;; find local minimum of Y coord along this edge
  (unless dir
    (break "tried to extend horizontal sub-edge up?")
    (return-from extend-edge-down v))
  (loop for n = (funcall dir v)
        while (and (= (hv-x v) (hv-x n))
                   (> (hv-y v) (hv-y n)))
        do (setf v n))
  v)

(setf *print-circle* t)
(defun classify-vertical-edge (top bottom)
  (let ((up (up-dir bottom))
        (down (down-dir top))
        (rising (eql (dll-prev top) bottom))
        (falling (eql (dll-next top) bottom)))
    (when (eql (dll-prev top) (dll-prev bottom))
      (return-from classify-vertical-edge :degenerate))
    (unless (and up down)
      (return-from classify-vertical-edge nil))
    (setf top (extend-edge-up top up))
    (setf bottom (extend-edge-down bottom down))
    (let ((dxt (- (hv-x (funcall up top)) (hv-x top)))
          (dxb (- (hv-x (funcall down bottom)) (hv-x bottom))))
      (cond
        ((and rising (plusp dxt) (plusp dxb))
         :leftmost)
        ((and rising (minusp dxt) (minusp dxb))
         :left-notch)
        ((and falling (minusp dxt) (plusp dxb))
         :falling-edge)

        ;; shouldn't happen in bottom-left packing
        ((and falling (plusp dxt) (plusp dxb))
         :right-notch)

        ;; these aren't interesting, but including for debugging for now
        ((and rising (minusp dxt) (plusp dxb))
         :step-down)
        ((and rising (plusp dxt) (minusp dxb))
         :step-up)
        ((and falling (plusp dxt) (minusp dxb))
         :rising-edge)
        ((and falling (minusp dxt) (minusp dxb))
         :rightmost)

        (t (list :dxt dxt :dxb dxb :rising rising :falling falling))))))


(defun set-qnw (v)
  (assert (equalp (hv-classify v) '(:left-notch :top)))
  ;; walk NEXT until we find a horizontal edge that crosses this X value
  (let ((x1 (hv-x v))
        (y1 (hv-y v))
        (v2a nil))
    (block h
      (do-dll/next (v2 (dll-next v))
        (let* ((v3 (dll-next v2))
               (x2 (hv-x v2))
               (x3 (hv-x v3)))
          (cond
            ((= x2 x1)
             (assert (/= (hv-y v) (hv-y v2)))
             (assert (not (hv-q v2)))
             ;; directly under a vertex, just link to it
             (setf (hv-n v) v2)
             (setf (hv-q v2) v)
             (setf v2a v2)
             (return-from h nil))
            ((< x2 x1 x3)
             (assert (= (hv-y v2) (hv-y v3)))
             ;; under an edge, split it and link to new vertex
             (setf (hv-n v) (make-hole-vertex x1 (hv-y v2)))
             (setf (hv-q (hv-n v)) v)
             (setf (hv-classify (hv-n v)) (list :qn :top))
             (insert-after v2 (hv-n v))
             (setf v2a v2)
             (return-from h nil))
            ((eql v2 v)
             (error "couldn't find edge above left notch?"))))))

    (let ((left nil))
      (flet ((add (n)
               (if left
                   (when (< (hv-x n) (hv-x left))
                     (setf left n))
                   (setf left n))))
        (do-dll/next (v2 v2a)
          (let* ((v3 (dll-next v2))
                 (y2 (hv-y v2))
                 (y3 (hv-y v3)))
            (cond
              ((and (> (hv-x v2) (hv-x v))
                    (= y2 y1))
               (add v2))
              ((and (> (hv-x v2) (hv-x v))
                    (< y3 y1 y2))
               (add v2))
              ((eql v2 v)
               (unless left
                 (error "couldn't find edge right of left notch?"))))))
        (when left
          (let* ((v2 left)
                 (v3 (dll-next v2))
                 (y2 (hv-y v2))
                 (y3 (hv-y (dll-next v2))))
            (cond
              ((= y2 y1)
               (add v2)
               (assert (/= (hv-x v) (hv-x v2)))
               ;; directly left of a vertex, just link to it
               (setf (hv-w v) v2)
               (assert (not (hv-q v2)))
               (setf (hv-q v2) v)
               (setf v2a v2)
               (return-from set-qnw t))
              ((< y3 y1 y2)
               (assert (= (hv-x v2) (hv-x v3)))
               ;; left of an edge, split it and link to new vertex
               (setf (hv-w v) (make-hole-vertex (hv-x v2) y1))
               (setf (hv-q (hv-w v)) v)
               (setf (hv-classify (hv-n v)) (list :qw :right))
               (insert-after v2 (hv-w v))
               (setf v2a v2)
               (return-from set-qnw nil))
              (t
               (error "broken?"))))))))
  nil)

(defun classify-vertex (v)
  (when (and (eql v (dll-prev v))
             (eql v (dll-next v)))
    (return-from classify-vertex :degenerate))
  (when (or (eql v (dll-prev v))
            (eql v (dll-next v)))
    (error "half-degenerate vertex?"))
  (let* ((prev (dll-prev v))
         (next (dll-next v))
         (x (hv-x v))
         (y (hv-y v))
         (px (hv-x prev))
         (py (hv-y prev))
         (nx (hv-x next))
         (ny (hv-y next))
         (dx1 (- px x))
         (dy1 (- py y))
         (dx2 (- nx x))
         (dy2 (- ny y)))
    ;; make sure we have only flat lines
    (assert (and (or (zerop dx1) (zerop dy1))
                 (or (zerop dx2) (zerop dy2))))
    (let* ((v2 (if (zerop dx1) prev next))
           (top (if (> y (hv-y v2)) v v2))
           (bottom (if (> y (hv-y v2)) v2 v))
           (%edge (classify-vertical-edge top bottom))
           (edge (when %edge (list %edge (if (eql top v) :top :bottom)))))
      (assert (not (and (hv-classify v) edge)))
      (setf (hv-classify v) (or edge (hv-classify v)))
      (when (equalp edge '(:left-notch :top))
        (set-qnw v))
      edge)))

(defstruct (point (:constructor make-point (x y))
                  (:conc-name nil))
  (x 0.0)
  (y 0.0))

;; used to indicate a point in D that is above a gap caused by
;; skipping a subhole. Depending on the height of the placement and Y
;; value of the support from C, we might be able to place something
;; against the edge below this point or might not
(defstruct (point-gap (:include point)
                      (:constructor make-point-gap (x y gap
                                                      &optional (gap-y 0)))
                      (:conc-name nil))
  (gap nil)
  (gap-y 0))

;; used to indicate a point in C that can't support a placement at the
;; exact X value, but can if strictly greater (or less depending on
;; which side)
(defstruct (point-open (:include point)
                       (:constructor make-point-open (x y))
                       (:conc-name nil)))

;; used to indicate a point in C that starts a span at the bottom of a
;; vertical edge, so is supported to the left without needing support
;; from D
(defstruct (point-bottom-left  (:include point)
                               (:constructor make-point-bottom-left (x y))
                               (:conc-name nil)))

(defclass f-edge ()
  ;; f-edge contains the vertical edges of the top or bottom edge of F
  ;; data structure, in the form of vectors of lower and upper
  ;; points. Gap stores amount skipped in corresponding edge by a
  ;; Q->QN link if any
  ((d :accessor f-d :initarg :d)
   (h :accessor f-h :initarg :h)
   (gap :accessor f-gap :initarg :gap :initform nil)
   (gap-y :accessor f-gap-y :initarg :gap-y :initform nil)))

;; subhole
(defclass subhole (dll)
  ;; link to containing hole
  ((hole :accessor sh-hole :initarg :hole)
   ;; f-edge struct for top,bottom edges of subhole
   (top :accessor sh-top :initarg :top)
   (bottom :accessor sh-bottom :initarg :bottom)
   (start :reader sh-start :initarg :start)
   ;; link to end of top/bottom, = Q vertex if any, otherwise lower right vertex
   (end :accessor sh-end :initarg :end)

   ;; to be calculated in constructor

   ;; true if subhole has a "falling corner"
   (falling-corner-p :accessor sh-falling-corner-p :initarg :falling)
   ;; for fast rejection of things that definitely won't fit
   (max-width :accessor sh-max-width :initarg :width)
   (max-height :accessor sh-max-height :initarg :height)))

(defclass hole (dll)
  (;; link to dll of hole-vertex
   (vertices :accessor h-vertices :initform nil :initarg :vertices)

   ;; calculated in constructor

   ;; link to dll of subholes for this hole
   (subholes :accessor h-subholes :initform nil)
   ;; for fast rejection of things that definitely won't fit
   (max-width :accessor ht-max-width :initform 0)
   (max-height :accessor ht-max-height :initform 0)))

(defmacro with-minmax ((min-var max-var minmax-fun) &body body)
  (alexandria:with-gensyms (v)
    `(let ((,min-var nil)
           (,max-var nil))
       (declare (ignorable ,min-var ,max-var))
       (flet ((,minmax-fun (,v)
                (setf ,min-var (min (or ,min-var ,v) ,v))
                (setf ,max-var (max (or ,max-var ,v) ,v))))
         ,@body))))
(defun make-hole-vertex (x y &optional prev)
  (let ((a (make-instance 'hole-vertex :x x :y y)))
    (insert-after prev a)
    a))


(defun make-subhole (hole start prev)
  (let* ((top start)
         (bottom (dll-prev start))
         (top-x nil)
         (top-y nil)
         (top-gap nil)
         (top-gap-y nil)
         (bottom-x nil)
         (bottom-y nil)
         (end nil)
         (falling 0))
    (assert (not (eql top bottom)))
    (assert (= (hv-x top) (hv-x bottom)))
    (assert (> (hv-y top) (hv-y bottom)))
    (with-minmax (x1 x2 mmx)
      (with-minmax (y1 y2 mmy)
        (let ((hit-q nil))
          (do-hole-vertices/next (v top dx dy endp)
            ;; to build a subhole top, we walk clockwise from start
            ;; (following hv-n) collecting vertical edges until we see
            ;; either a line moving left (= end of top), or a hv-q
            ;; after a normal vertex.

            ;; once we see the hv-q, we continue until we see a
            ;; leftwards edge, only collecting edges that go below the
            ;; y value of the hv-q (should be 1 or 2 depending on
            ;; whether we have a falling edge in hole and where it is)
            (when (and dx (minusp dx))
              (unless end
                (setf end (hv-x (dll-prev v))))
              (return nil))
            (when (and (not hit-q)
                       dy
                       (zerop dy)
                       (hv-q v))
              (setf end (hv-x v))
              (setf hit-q v))
            (cond
              ;; first point
              ((and (not dy) (not hit-q))
               (push (hv-x v) top-x)
               (push (hv-y (dll-prev v)) top-y)
               (push (hv-x v) top-x)
               (push (hv-y v) top-y)

               (push nil top-gap)
               (push nil top-gap-y))
              ;; upwards vertical edges
              ((and dy
                    (plusp dy)
                    (not (hv-n v)))
               (unless hit-q
                 (push (hv-x v) top-x)
                 (push (hv-y v) top-y)
                 (if (hv-q v)
                     (progn
                       (push dy top-gap)
                       (push (- (hv-y v) dy) top-gap-y))
                     (progn
                       (push nil top-gap)
                       (push nil top-gap-y)))))
              ;; downwards edges
              ((and dy
                    (minusp dy)
                    (not (hv-q v)))
               (when (or (not hit-q)
                         (<= (hv-y v)
                             (hv-y hit-q)))
                 (incf falling)
                 (push (hv-x v) top-x)
                 (push (hv-y v) top-y)
                 (push nil top-gap)
                 (push nil top-gap-y))))
            ;; follow hv-n
            (when (and dx (zerop dx) (hv-n v))
              (jump (hv-n v)))))

        (let ((hit-q nil))
          (do-hole-vertices/prev (v bottom dx dy)
            ;; to build subhole bottom, we walk CCW from bottom
            ;; (following only prev) collecting vertical edges,
            ;; stopping at leftwards edge or first hv-w

            ;; once we see hv-q, we skip to hv-w and continue
            ;; (following hv-w) until we see a leftwards edge,
            ;; collecting only the points whose y value is > y value of
            ;; hv-n (can be any # >= 1)

            (when (and dx (minusp dx))
              (return nil))
            (when (and (not hit-q)
                       dy
                       (zerop dy)
                       (hv-w v))
              (setf hit-q v))
            (cond
              ;; first point
              ((and (not dy) (not hit-q))
               (push (hv-x v) bottom-x)
               (push (hv-y (dll-next v)) bottom-y)
               (push (hv-y v) bottom-y)
               (push (hv-x v) bottom-x))
              ;; upwards vertical edges
              ((and dy
                    (plusp dy)
                    (not (hv-q v)))
               (when (or (not hit-q)
                         (>= (hv-y v)
                             (hv-y hit-q)))
                 (push (hv-x v) bottom-x)
                 (push (hv-y v) bottom-y)))
              ;; downwards edges
              ((and dy
                    (not (zerop dy))
                    (not (hv-n v))
                    (not (hv-q v)))
               (unless hit-q
                 (push (hv-x v) bottom-x)
                 (push (hv-y v) bottom-y))))
            ;; follow hv-w
            (when (hv-w v)
              (jump (hv-w v)))))

        ;; once we have top/bottom, make sure the ends match properly
        ;; (may need to trim or extend ends depending on how previous
        ;; loops terminated, not sure yet)
        (when (or top-y bottom-y)

          (when (< (car top-y)
                   (car bottom-y))
            (let ((p (position (car bottom-x) top-x)))
               (if p
                  (setf top-x (subseq top-x p)
                        top-y (subseq top-y p)
                        top-gap (subseq top-gap p)
                        top-gap-y (subseq top-gap-y p))
                  (break "?"))))
          (assert (= (length top-x) (length top-y)))
          (assert (= (length bottom-x) (length bottom-y)))
          (let ((ey1 (min (car bottom-y) (cadr bottom-y)))
                (ey2 (max (car top-y) (cadr top-y))))
            (setf (car top-y) ey1)
            (setf (car bottom-y) ey2)))
        (setf falling
              (and (cddr top-y)
                   (< (cadr top-y) (caddr top-y))))

        (flet ((make-dh (xx yy &optional making-c)
                 (loop for y0 = nil then y
                       for x in xx
                       for y in yy
                       for falling = (and y0 (< y y0))
                       do (mmx x) (mmy y)
                        when y0
                         collect (if (and falling making-c)
                                     (make-point-bottom-left
                                      x (alexandria:clamp (min y0 y) y1 y2))
                                     (make-point
                                      x (alexandria:clamp (min y0 y) y1 y2)))
                          into d
                         and collect (make-point x (alexandria:clamp
                                                    (max y0 y) y1 y2))
                               into h

                       finally (return (list :d (coerce d 'vector)
                                             :h (coerce h 'vector))))))
          (let ((s (make-instance 'subhole
                                  :Falling falling
                                  :hole hole
                                  :top (apply
                                        #'make-instance
                                        'f-edge
                                        :gap (coerce (nreverse top-gap)
                                                     'vector)
                                        :gap-y (coerce (nreverse top-gap-y)
                                                       'vector)
                                        (make-dh (nreverse top-x)
                                                 (nreverse top-y)))
                                  :bottom (apply
                                           #'make-instance
                                           'f-edge
                                           (make-dh (nreverse bottom-x)
                                                    (nreverse bottom-y)
                                                    t))
                                  :end end
                                  :start bottom
                                  :width (- x2 x1)
                                  :height (- y2 y1))))
            (insert-after prev s)
            s))))))

(defun make-subholes (hole)
  (with-minmax (x1 x2 mmx)
    (with-minmax (y1 y2 mmy)
      (let ((edges))
        (do-dll/next (n (h-vertices hole))
          (mmx (hv-x n))
          (mmy (hv-y n))
          (when (equalp '(:leftmost :top) (classify-vertex n))
            (push n edges)))

        (setf (ht-max-width hole) (- x2 x1))
        (setf (ht-max-height hole) (- y2 y1))

        ;; we build subholes in 2nd pass since we need to wait until
        ;; classify-vertex builds the QN/QW links for all nodes
        (loop for e in edges
              for h = (make-subhole hole e nil)
                then (make-subhole hole e h)
              finally (return h))))))

(defun make-hole (v &optional prev)
  (let ((a (make-instance 'hole :vertices v)))
    (insert-after prev a)
    (setf (h-subholes a) (make-subholes a))
    a))

;; for now we only directly create the first open hole, others are
;; clipped from that
(defun init-hole (w h)
  (let* ((a (make-hole-vertex 0 0))
         (b (make-hole-vertex 0 h a))
         (c (make-hole-vertex w h b))
         (d (make-hole-vertex w 0 c)))
    (make-hole d)))

;; mostly for debugging/testing
(defun %make-hole-from-points (points)
  ;; points should be (x1 y1 x2 y2 ...), clockwise around hole
  (make-hole
   (loop for (x y) on points by 'cddr
         for v = (make-hole-vertex x y nil)
           then (make-hole-vertex x y v)
         finally (return v))))

(defun vp= (vertex point)
  (unless (and (= (hv-x vertex) (x point))
               (= (hv-y vertex) (y point)))
    (break "~s ~s" vertex point))
  (and (= (hv-x vertex) (x point))
       (= (hv-y vertex) (y point))))


(defun check-subhole (sh)
  (assert (equalp '(:leftmost :bottom) (hv-classify (sh-start sh))))
  (assert (vp= (sh-start sh) (aref (f-d (sh-bottom sh)) 0)))
  (assert (vp= (sh-start sh) (aref (f-d (sh-top sh)) 0)))
  (assert (vp= (dll-next (sh-start sh)) (aref (f-h (sh-bottom sh)) 0)))
  (assert (vp= (dll-next (sh-start sh)) (aref (f-h (sh-top sh)) 0)))

  (assert (sh-end sh))
  (let ((x1 most-positive-fixnum)
        (x2 most-negative-fixnum)
        (y1 most-positive-fixnum)
        (y2 most-negative-fixnum))
    ;; components of top/bottom edges must have same length
    (assert (= (length (f-d (sh-top sh)))
               (length (f-h (sh-top sh)))
               (length (f-gap (sh-top sh)))
               (length (f-gap-y (sh-top sh)))))
    (assert (= (length (f-d (sh-bottom sh)))
               (length (f-h (sh-bottom sh)))))

    ;; validate top/bottom, calculate bounds
    (let ((top (sh-top sh)))
      (loop with px = most-negative-fixnum
            with py = (y (aref (f-d top) 0))
            with l = (length (f-d top))
            for d across (f-d top)
            for h across (f-h top)
            for g across (f-gap top)
            for gy across (f-gap-y top)
            for i from 0
            ;; all segments are vertical
            do (assert (= (x d) (x h)))
               ;; f-d is always lower y value than f-h
               (assert (> (y h) (y d)))
               ;; all edges line up with previous, and go up except
               ;; last 1 or 2 which go down
               (assert (if (or
                            ;; last 2 edges of falling-corner-p hole go down
                            (and (sh-falling-corner-p sh)
                                 (>= i (- l 2)))
                            ;; otherwise only last edge goes down
                            (>= i (1- l)))
                           (= (y h) (shiftf py (y d)))
                           (= (y d) (shiftf py (y h)))))
               ;; all edges are right of previous edges
               (assert (> (x d) px))
               (setf px (x d))
               ;; gap should be less than distance between d and h if set
               (when g
                 (assert (< g (- (y h) (y d))))
                 (assert gy)
                 (assert (<= (y d) gy (y h)))
                 (assert (<= (+ gy g) (y h))))
               ;; update bounds
               (setf x1 (min x1 (x d) (x h)))
               (setf x2 (max x2 (x d) (x h)))
               (setf y1 (min y1 (y d) (y h)))
               (setf y2 (max y2 (y d) (y h)))))
    (let ((bottom (sh-bottom sh)))
      ;; gap should never be set
      (every 'null (f-gap bottom))
      (every 'null (f-gap-y bottom))
      (loop with px = most-negative-fixnum
            for d0 = nil then d
            for h0 = nil then h
            for d across (f-d bottom)
            for h across (f-h bottom)
            for i from 0
            ;; all edges are vertical
            do (assert (= (x d) (x h)))
               ;; f-d is always lower y value than f-h
               (assert (> (y h) (y d)))
               ;; all edges line up with previous
               (when (or d0 h0)
                 (assert (or (= (y h) (y h0))
                             (= (y h) (y d0))
                             (= (y d) (y h0))
                             (= (y d) (y d0)))))
               ;; all edges are right of previous edges
               (assert (> (x d) px))
               (setf px (x d))
               ;; update bounds
               (setf x1 (min x1 (x d) (x h)))
               (setf x2 (max x2 (x d) (x h)))
               (setf y1 (min y1 (y d) (y h)))
               (setf y2 (max y2 (y d) (y h)))))

    ;; check bounding box, make sure end is inside bounds
    (assert (= (sh-max-width sh) (- x2 x1)))
    (assert (= (sh-max-height sh) (- y2 y1)))
    (assert (<= x1 (sh-end sh) x2))))

(defun -x (p d)
  (make-point (- (x p) d) (y p)))
(defun +x (p d)
  (make-point (+ (x p) d) (y p)))
(defun -y (p d)
  (make-point (x p) (- (y p) d)))
(defun +y (p d)
  (make-point (x p) (+ (y p) d)))

(defun <=x (p1 p2)
  (<= (x p1) (x p2)))
(defun <=y (p1 p2)
  (<= (y p1) (y p2)))
(defun >y (p1 p2)
  (> (y p1) (y p2)))
(defun <y (p1 p2)
  (< (y p1) (y p2)))

(defun make-c (fb l end)
  ;;generate C given FB and width L of b1b2 (procedure BOTTOM etc)
  (flet ((h (x)
           (aref (f-h fb) x))
         (d (x)
           (aref (f-d fb) x)))
    (let* ((q (make-deq))
           (b1 (-x (d 0) l))
           (b2 (d 0))
           (c (list b1))
           (m (length (f-h fb)))
           (support (if (= (y b1) (y (d 1)))
                        (d 1)
                        (h 1))))
      (labels ((setup (start end)
                 (let ((q (make-deq)))
                   (loop
                     for i from (- end 1) downto start
                      when (or (deq-empty-p q)
                              (<=y (first (top1 q))
                                   (d i)))
                       do (let* ((p1 (if (and (= (1+ i) end)
                                              (not (point-bottom-left-p (d end))))
                                         (d (1+ i))
                                         (h (1+ i))))
                                 (p0 (make-point (x (d i))
                                                 (y p1))))
                            (push1 (list p0 p1)
                                   q)))
                   q))
               (mergeq (q1)
                 (when (not (deq-empty-p q1))
                   (destructuring-bind (sl sr) (top1 q1)
                     (assert (= (y sl) (y sr)))
                     (loop while (not (deq-empty-p q))
                           for (ql qr) = (top2 q)
                           do (assert (= (y ql) (y qr)))
                           while (<y ql sl)
                           do (pop2 q))))
                 (loop for x = (top1 q1)
                       until (deq-empty-p q1)
                       do (push2 x q)
                          (pop1 q1)))
               (slide (start)
                  (loop named outer
                       for hit = nil
                       for i = start
                       repeat 10
                       while (< start m)
                       do ;; slide on support
                          (loop ;; ready to fall
                                when (>= (x (h i))
                                         (+ (x support) l))
                                  do (setf hit :drop)
                                  and return nil

                                ;; hit a new support
                                when (and (= (y (h i)) (y support))
                                          (/= (x (h i))
                                              (x support)))
                                  do (setf support (h i))

                                     ;; hit hidi
                                when (> (y (h i)) (y b2))
                                  do (setf hit :raise)
                                  and return nil

                                do (incf i)

                                when (>= i m)
                                  return nil)
                          (ecase hit
                            ((nil)
                             ;; i = m, done
                             (assert (>= i (1- m)))
                             (pop c)
                             (loop-finish))
                            (:raise
                             (let ((u (make-point (- (x (h i)) l)
                                                  (y b1))))
                               (push u c)
                               (setf b1 (make-point-open (- (x (h i)) l)
                                                         (y (h i)))
                                     b2 (h i))

                               (when (= i (1- m))
                                 (loop-finish))
                               (push b1 c)

                               (empty-deq q)
                               (setf support (make-point (x (d (1+ i)))
                                                         (y b1)))
                               (setf start (1+ i))))
                            (:drop
                             ;; ready to fall
                             (setf b1 support)
                             (setf b2 (+x b1 l))
                             (push (make-point-open (x b1) (y b1)) c)
                             (let ((q1 (setup start i)))
                               (mergeq q1)
                               ;; q is list of lists of horizontal segments
                               (destructuring-bind (lk rk) (top1 q)
                                 (pop1 q)
                                 (let ((dy (- (y b1) (y lk))))
                                   (setf b1 (-y b1 dy)
                                         b2 (-y b2 dy))
                                   (push (make-point-bottom-left (x b1) (y b1))
                                         c)
                                   (setf start i)
                                   (setf support rk))))))))
               (trim ()
                 (assert (evenp (length c)))
                 (loop for (p1 p2) on c by #'cddr
                       ;; only keep spans that are in proper order
                       when (and (>= (x p2) (x p1))
                                 ;; and extend past beginning of subhole
                                 (>= (x p2) (x (h 0)))
                                 ;; and start before end of subhole
                                 (< (x p1) end))
                         collect (if (< (x p1) (x (h 0)))
                                     ;; clamp start of span to start of
                                     ;; subhole (should only affect
                                     ;; first valid span)
                                     (make-point-bottom-left (x (h 0)) (y p1))
                                     p1)
                         and collect p2)))
        (slide 1)
        (setf c (nreverse c))
        (coerce (trim)'vector)))))

(defun make-d (ft l end falling)
  (declare (ignorable end falling))
  ;; generate D given FT and width L of b3b4 (procedure TOP)
  (let ((p (1- (length (f-d ft)))))
    (flet ((h (x)
             (aref (f-h ft) x))
           (d (x)
             (aref (f-d ft) x))
           (g (x)
             (aref (f-gap ft) x))
           (gy (x)
             (aref (f-gap-y ft) x)))
      (assert (= (length (f-d ft)) (length (f-h ft))))
      (unless falling
        (return-from make-d
          ;; this goes a bit past what is needed, since it doesn't check
          ;; right edge, but placement will be limited by lower edge
          (let ((a (list* (h 0)
                          (loop for i from 1 upto p
                                unless (= i p)
                                  collect (d i)
                                collect (make-point-gap (x (h i))
                                                        (y (h i))
                                                        (g i)
                                                        (gy i))))))
            (coerce a 'vector))))
      (let ((d (list (h 0)))
            (p (1- (length (f-h ft)))))
        (when (> (+ (x (d 0)) l)
                 (x (d p)))
          (return-from make-d #()))

        (loop with p-1 = (1- p)
              with dp-1 = (d p-1)
              with dp = (d p)
              with hp = (h p)
              for n from 1 below p
              for dn = (d n)
              for hn = (h n)
              for gap = (g n)
              if (= n p-1)
                ;; if we reached p-1, previous span exactly fits in
                ;; gap, so need to make it shorter, and add span under
                ;; falling edge
                do (let* ((u (make-point (- (x dp-1) l) (y dn)))
                          (v (make-point-open (x u) (y dp-1)))
                          (prev (car d)))
                     (assert (= (x u) (x (-x hn l))))

                     (cond
                       ;; possibly give up on current span
                       ((> (+ (x prev) l) (x dn))
                        (pop d))
                       ;; or complete it
                       (t
                        (let ((p (make-point (- (x dn) l)
                                             (y prev))))
                          (push p d))))

                     (push v d)
                     (push (h p) d)
                     (loop-finish))
              if (<= (+ (x dn) l)
                     (if (>= (y dp-1) (y dn))
                         (x dp)
                         (x dp-1)))
                ;; next span fits, finish previous span and start next
                do (push dn d)
                 (if gap
                     (push (make-point-gap (x hn) (y hn) gap (gy n)) d)
                     (push hn d))

              else
                if (> (+ (x dn) l)
                      (x dp))
                  ;; hit end, no more spans fit
                  do (cond
                       ;; possibly give up on current span
                       ((> (+ (x (car d)) l) (x dp))
                        (pop d))
                       ;; or complete it
                       (t
                        (let ((p (make-point (- (x dp) l)
                                             (y (car d)))))
                          (push p d))))
                  and return nil
              else
                if (< (y dp-1) (y dn))
                  ;; fig 12a: we've hit falling edge, and it is lower
                  ;; than current span
                  do (let* ((u (make-point (max (x dn)
                                                (- (x dp-1) l))
                                           (y dn)))
                            (v (make-point-open (x u) (y dp-1))))
                       (assert (>= (x u) (x dn)))
                       (push u d)
                       (push v d)
                       (push (h p) d)
                       (loop-finish))
              else
                if (= (y dp-1) (y dn))
                  ;; we span the gap between this span and falling edge
                  do (let ((u (make-point (x dp) (y dn))))
                       (push u d)
                       (loop-finish))
              if (> (+ (x dn) l) (x dp-1))
                ;; fig 12b: falling edge is above this span, but blocks
                ;; any intervening spans. find vertical edge left of
                ;; falling edge and add last span, if it fits
                do (cond
                     ((> (+ (x (car d)) l)
                         (if (>= (y dp-1) (y (car d)))
                             (x dp)
                             (x dp-1)))
                      (pop d))
                     (t
                      (push (d (1+ n)) d)))
                   ;; find vertical edge left of falling edge
                   (loop while (and (< n p-1)
                                    (< (y (d (1+ n)))
                                       (y dp-1)))
                         do (incf n))
                   (setf dn (d n))
                   (setf hn (h n))
                 (when (<= (+ (x dn) l)
                           (x dp))
                   (let ((u (if (>= (x dn) (- (x dp-1) l))
                                (if (g n)
                                    (let* ((g1 (gy n))
                                           (g2 (+ g1 gap))
                                           (fy (y dp-1)))
                                      (make-point-gap (x dn) fy
                                                      (if (< fy g1)
                                                          nil
                                                          (- (min g2 fy)
                                                             g1))
                                                      (gy n)))
                                    (make-point (x dn) (y dp-1)))
                                (make-point-open (- (x dp-1) l)
                                                 (y dp-1)))))
                     (push u d)
                     (push hp d)))
                 (loop-finish))
        (let ((v (coerce (nreverse d) 'vector)))
          (unless (zerop (mod (length v) 2))
            (break "~s?~s" (length v) v))
          v)))))


(defun placing (h c d)
  (declare (type vector c d))
  
  (let ((e)
        (m (1- (floor (length c) 2)))
        (p (1- (floor (length d) 2))))
    (when (or (< m 0) (< p 0))
      ;; C or D is empty, nothing fits
      (return-from placing nil))
    (labels ((l (x) (aref c (* x 2)))
             (r (x) (aref c (1+ (* x 2))))
             ;; ll=l', rr=r'
             (ll (x) (aref d  (* x 2)))
             (rr (x) (aref d (1+ (* x 2))))
             ;; return true if H fits between l' and l at specified indices
             (ok (j i)
               (>= (- (y (ll j)) (y (l i)))
                   h))
             (falling (j i x)
               (declare (ignore j))
               ;; true if we are exactly at a point-open point on C
               (or (and (point-open-p (l i))
                        (= x (x (l i))))
                   (and (point-open-p (r i))
                        (= x (x (r i))))))
             (sliding (j i x)
               ;; true if placement doesn't have any support to left
               (cond
                 ;; placed at bottom-left point on C, has support
                 ((and (= x (x (l i)))
                       (point-bottom-left-p (l i)))
                  nil)
                 ;; at left edge of span at top, no gap. Need to
                 ;; make sure placement extends past bottom of top
                 ;; edge
                 ((and (= x (x (ll j)))
                       (or (not (point-gap-p (ll j)))
                           (not (gap (ll j))))
                       ;; fixme: set falling edge as 'open' or store
                       ;; Y value for all edges in D?
                       (not (point-open-p (ll j)))
                       (> (+ (y (l i)) h)
                          ;; use height of previous edge, should be
                          ;; right with way D is generated? maybe
                          ;; store in gap-y instead?
                          (y (rr (max 0 (1- j))))))
                  nil)
                 ;; at left edge of span at top, and there is a gap,
                 ;; check gap for support
                 ((and (= x (x (ll j)))
                       (typep (ll j) 'point-gap)
                       (gap (ll j)))
                  (let* ((p (ll j))
                         (gy1 (gap-y p))
                         (gy2 (+ gy1 (gap p)))
                         (y1 (y (l i)))
                         (y2 (+ y1 h)))
                    (or (and (< gy1 y1)
                             (<=  y2 gy2))
                        (< y2 gy1))))
                 (t
                  ;; otherwise, assume no support? (shouldn't happen?)
                  t))))
      (declare (ignorable #'r #'rr))
      (loop
        with min-x = (x (l 0))
        with placed = nil
        with i = 0 ;; index into C (bottom)
        with j = 0 ;; index into D (top)
        repeat 100
        do (setf min-x (max min-x
                            (x (l i))
                            (x (ll j))))

           ;; if we have a valid placement, add it
        do (let* ((x (max min-x
                          (x (l i))
                          (x (ll j))))
                  (p (make-point x (y (l i)))))
             (when (and (ok j i)
                        (not (falling j i x))
                        (not (sliding j i x)))
               (push p e)
               ;; can't place anything else on this bottom span
               (setf placed t)))
        when (> i m)
          return nil
        do ;; advance top or bottom to next candidate
           (cond
             ((or placed
                  (and (< (x (r i))
                          (x (rr j)))
                       (< i m))
                  (< (x (r i)) (x (ll j))))
              (incf i))
             ((and (= (x (rr j))
                      (x (r i)))
                   (or (< i m)
                       (< j p)))
              ;; we need to be careful when segments end at same
              ;; point, because both segments are valid at that point.
              (cond
                ;; if either index hits end, increment other one
                ((= j p)
                 (incf i))
                ((= i m)
                 (incf j))
                ;; increment i if it is falling
                ((< (y (l (1+ i)))
                    (y (r i)))
                 (incf i))
                ;; increment j if it is rising
                ((< (y (rr j))
                    (y (ll (1+ j))))
                 (incf j))
                ;; otherwise, increment whichever of i,j rises less
                ((< (- (y (l (1+ i))) (y (r i)))
                    (- (y (ll (1+ j))) (y (rr j))))
                 (incf i))
                (t
                 (incf j))))
             (t
              (incf j)
              (when (and (<= j p)
                         (< (x (r i)) (x (ll j))))
                (incf i))))
           (setf placed nil)
        while (and (<= i m) (<= j p))))
    (nreverse e)))

;; mostly for debugging/testing, so not optimized
(defun valid-placement-p (hole w h x y &key dump)
  ;; determine if it X,Y is a valid bottom-left-stable placement for a
  ;; rectangle of dimensions WxH:

  ;; doesn't cross any edges of hole

  ;; doesn't completely contain any edges of hole

  ;; at least one edge of hole coincides with each of bottom and left
  ;; edges of placement (but specifically not including the case where
  ;; only corners touch. hole edge and side must overlap by a non-zero
  ;; amount)
  (let ((hit-left nil)
        (hit-bottom nil))
    (labels ((check-v (x1 y1 y2)
               ;; see if a vertical hole edge crosses or is entirely
               ;; contained in placement
               (let ((y1 (min y1 y2))
                     (y2 (max y1 y2)))
                 (when (and (< x x1 (+ x w))
                            (or (< y1 y y2)
                                (< y1 (+ y h) y2)
                                (<= y y1 y2 (+ y h))))
                   (return-from valid-placement-p nil)))

               ;; see if upwards vertical edge overlaps left edge of placement
               (when (and (< y1 y2)
                          (= x x1)
                          (or (< y1 y y2)
                              (< y1 (+ y h) y2)
                              (and (<= y y1)
                                   (<= y2 (+ y h)))))
                 (setf hit-left t)))
             (check-h (x1 x2 y1)
               ;; see if horizontal hole edge crosses placement
               (let ((x1 (min x1 x2))
                     (x2 (max x1 x2)))
                 (when (and (< y y1 (+ y h))
                            (or (< x1 x x2)
                                (< x1 (+ x w) x2)
                                (<= x x1 x2 (+ x w))))
                   (return-from valid-placement-p nil)))
               ;; see if leftwards horizontal edge overlaps bottom
               ;; edge of placement
               (when (and (< x2 x1)
                          (= y y1)
                          (or (< x2 x x1)
                              (< x2 (+ x w) x1)
                              (and (<= x x2)
                                   (<= x1 (+ x w)))))
                 (setf hit-bottom t))))
      (do-hole-vertices/next (v (h-vertices hole) dx dy end)
        (unless (or dx dy)
          (setf dx (- (hv-x v) (hv-x (dll-prev v))))
          (setf dy (- (hv-y v) (hv-y (dll-prev v)))))

        (cond
          ((and (zerop dx) (zerop dy))
           (error "invalid hole? 0-length segment at (~s,~s?)" (hv-x v) (hv-y v)))
          ((and (not (zerop dx)) (not (zerop dy)))
           (error "invalid hole? diagonal segment at (~s,~s), dxy=(~s,~s)?"
                  (hv-x v) (hv-y v) dx dy))
          ((zerop dx)
           (check-v (hv-x v)
                    (hv-y (dll-prev v)) (hv-y v)))
          ((zerop dy)
           (check-h (hv-x (dll-prev v)) (hv-x v)
                    (hv-y v)))
          (t (break "??"))))
      (and hit-bottom hit-left))))

(defun find-all-placements/sh (subhole w h)
  (let ((r nil))
    (do-dll/next (n subhole)
      (when (and (<= w (sh-max-width n))
                 (<= h (sh-max-height n)))
        (let ((d (make-d (sh-top n) w (sh-end n) (sh-falling-corner-p n))))
          (when (plusp (length d))
            (let ((c (make-c (sh-bottom n) w (sh-end n))))
              (setf r (append (placing h c d)
                              r)))))))
    r))

(defun find-all-placements (hole w h)
  (let ((r nil))
    (do-dll/next (n hole)
      (when (and (<= w (ht-max-width n))
                 (<= h (ht-max-height n)))
        (setf r (append (find-all-placements/sh (h-subholes n) w h)
                        r))))
    r))
#++
(ql:quickload '(binpack parachute))
