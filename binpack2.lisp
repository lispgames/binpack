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

(defun insert-before (next new)
  (cond
    (next
     (setf (dll-next new) next)
     (setf (dll-prev new) (dll-prev next))
     (setf (dll-next (dll-prev next)) new)
     (setf (dll-prev next) new))
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
   (classify :accessor hv-classify :initform nil)
   ;; if this vertex is the bottom of a leftmost edge of a subhole, or
   ;; 'end' vertex for a subhole, link to the subhole
   (sh :accessor hv-sh :initform nil)))

(defmacro %do-hole-vertices ((node dll dx dy endp iterator) &body body)
  (alexandria:with-gensyms (prev n)
    `(let ((,prev nil))
       (%do-dll (,node ,dll ,endp ,iterator)
         (let (,@(when dx `((,dx (if ,prev (- (hv-x ,node) (hv-x ,prev))))))
               ,@(when dy `((,dy (if ,prev (- (hv-y ,node) (hv-y ,prev)))))))
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
      #++(assert (not (and (hv-classify v) edge)))
      (setf (hv-classify v) (or edge (hv-classify v)))
      (when (equalp edge '(:left-notch :top))
        (set-qnw v))
      edge)))

(defclass point ()
  ((x :initform 0.0 :initarg :x :accessor x)
   (y :initform 0.0 :initarg :y :accessor y)
   (ref :initform nil :initarg :Ref :accessor ref)))

(defun make-point (x y &key ref)
  (make-instance 'point :x x :y y :ref ref))

(defun point-p (x)
  (typep x 'point))

;; used to indicate a point in D that is above a gap caused by
;; skipping a subhole. Depending on the height of the placement and Y
;; value of the support from C, we might be able to place something
;; against the edge below this point or might not
(defclass point-gap (point)
  ((gap :initform nil :accessor gap :initarg :gap)
   (gap-y :initform 0 :accessor gap-y :initarg :gap-y)))

(defun make-point-gap (x y gap &optional (gap-y 0))
  (make-instance 'point-gap :x x :y y :gap gap :gap-y gap-y))

(defun point-gap-p (x)
  (typep x 'point-gap))

;; used to indicate a point in C that can't support a placement at the
;; exact X value, but can if strictly greater (or less depending on
;; which side)
(defclass point-open (point)
  ())

(defun make-point-open (x y &key ref)
  (make-instance 'point-open :x x :y y :ref ref))

(defun point-open-p (x)
  (typep x 'point-open))

;; used to indicate a point in C that starts a span at the bottom of a
;; vertical edge, so is supported to the left without needing support
;; from D
(defclass point-bottom-left  (point)
  ())

(defun make-point-bottom-left (x y &key ref)
  (make-instance 'point-bottom-left :x x :y y :ref ref))

(defun point-bottom-left-p (x)
  (typep x 'point-bottom-left))

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
   ;; link to lower vertex of leftmost edge for the subhole
   (start :accessor sh-start :initarg :start)
   ;; link to end of top/bottom, = Q vertex if any, otherwise lower right vertex
   (endv :accessor sh-endv :initform nil)
   ;; x coord of right edge of hole
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

(defun update-subhole (subhole)
  (let* ((start (dll-next (sh-start subhole)))
         (top start)
         (bottom (dll-prev start))
         (top-x nil)
         (top-y nil)
         (top-gap nil)
         (top-gap-y nil)
         (bottom-x nil)
         (bottom-y nil)
         (ref nil)
         (end nil)
         (falling 0)
         (q nil))
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
              (setf hit-q v)
              (setf q v))
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
        (format t "q1 = ~s~%" (and q (list (hv-x q) (hv-y q))))
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
              (format t "q2 ~s -> ~s~%"
                      (and q (list (hv-x q) (hv-y q)))
                      (and v (list (hv-x v) (hv-y v))))
              (setf q v)
              (setf hit-q v))
            (cond
              ;; first point
              ((and (not dy) (not hit-q))
               (push v ref)
               (push (hv-x v) bottom-x)
               (push (hv-y (dll-next v)) bottom-y)
               (push v ref)
               (push (hv-x v) bottom-x)
               (push (hv-y v) bottom-y))
              ;; upwards vertical edges
              ((and dy
                    (plusp dy)
                    (not (hv-q v)))
               (when (or (not hit-q)
                         (>= (hv-y v)
                             (hv-y hit-q)))
                 (format t "q3 ~s ->"
                         (and q (list (hv-x q) (hv-y q))))
                 (unless q(setf q v))
                 ;; we are at top of right edge, follow it to bottom corner
                 (loop while (= (hv-x q) (hv-x (dll-next q)))
                       do (setf q (dll-next q)))
                 (format t "~s~%" (and q (list (hv-x q) (hv-y q))))
                 (push v ref)
                 (push (hv-x v) bottom-x)
                 (push (hv-y v) bottom-y)))
              ;; downwards edges
              ((and dy
                    (not (zerop dy))
                    (not (hv-n v))
                    (not (hv-q v)))
               (unless hit-q
                 (push v ref)
                 (push (hv-x v) bottom-x)
                 (push (hv-y v) bottom-y))))
            ;; follow hv-w
            (when (hv-w v)
              (jump (hv-w v)))))
        (format t "q4 ~s~%" (and q (list (hv-x q) (hv-y q))))

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
                  (progn
                    (format t "top-x = ~s~%top-y = ~s~%bot-x = ~s~%bot-y = ~s~%"
                            top-x top-y bottom-x bottom-y)
                    (break "?")))))
          (assert (= (length top-x) (length top-y)))
          (assert (= (length bottom-x) (length bottom-y)))
          (let ((ey1 (min (car bottom-y) (cadr bottom-y)))
                (ey2 (max (car top-y) (cadr top-y))))
            (setf (car top-y) ey1)
            (setf (car bottom-y) ey2)))
        (setf falling
              (and (cddr top-y)
                   (< (cadr top-y) (caddr top-y))))

        (flet ((make-dh (xx yy &optional making-c refs)
                 (loop for y0 = nil then y
                       for x in xx
                       for y in yy
                       for ref = (pop refs)
                       for falling = (and y0 (< y y0))
                       do (mmx x) (mmy y)
                       when y0
                         collect (if (and falling making-c)
                                     (make-point-bottom-left
                                      x (alexandria:clamp (min y0 y) y1 y2)
                                      :ref ref)
                                     (make-point
                                      x (alexandria:clamp (min y0 y) y1 y2)
                                      :ref ref))
                           into d
                           and collect (make-point x (alexandria:clamp
                                                      (max y0 y) y1 y2)
                                                   :ref ref)
                                 into h

                       finally (return (list :d (coerce d 'vector)
                                             :h (coerce h 'vector))))))
          (setf (sh-falling-corner-p subhole) falling)
          (setf (sh-top subhole) (apply
                               #'make-instance
                               'f-edge
                               :gap (coerce (nreverse top-gap)
                                            'vector)
                               :gap-y (coerce (nreverse top-gap-y)
                                              'vector)
                               (make-dh (nreverse top-x)
                                        (nreverse top-y))))
          (setf (sh-bottom subhole) (apply
                                  #'make-instance
                                  'f-edge
                                  (make-dh (nreverse bottom-x)
                                           (nreverse bottom-y)
                                           t
                                           (nreverse ref)))
)
          (setf (sh-end subhole) end)
          (setf (sh-endv subhole) q)
          (setf (hv-sh q) subhole)
          (setf (sh-max-width subhole) (- x2 x1))
          (setf (sh-max-height subhole) (- y2 y1))))))
)

(defun make-subhole (hole start prev &key (update t))
  (let ((s (make-instance 'subhole
                          :Falling nil
                          :hole hole
                          :top nil
                          :bottom nil
                          :end nil
                          :start (dll-prev start)
                          :width 0
                          :height 0)))
    (setf (hv-sh (sh-start s)) s)
    (when update
      (update-subhole s))
    (insert-after prev s)
            s)
)

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
  ;(assert (equalp '(:leftmost :bottom) (hv-classify (sh-start sh))))
  (assert (eql (hv-sh (sh-start sh)) sh))
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
                                                 (y p1)
                                                 :ref (or (ref (d i))
                                                          (ref p1)))))
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
                                                  (y b1)
                                                  :ref (or (ref (h i))
                                                           (ref b1)))))
                               (push u c)
                               (setf b1 (make-point-open (- (x (h i)) l)
                                                         (y (h i))
                                                         :ref (ref (h i)))
                                     b2 (h i))

                               (when (= i (1- m))
                                 (loop-finish))
                               (push b1 c)

                               (empty-deq q)
                               (setf support
                                     (make-point (x (d (1+ i)))
                                                 (y b1)
                                                 :ref (or (ref (d (1+ i)))
                                                          (ref b1))))
                               (setf start (1+ i))))
                            (:drop
                             ;; ready to fall
                             (setf b1 support)
                             (setf b2 (+x b1 l))
                             (push (make-point-open (x b1) (y b1) :ref (ref b1))
                                   c)
                             (let ((q1 (setup start i)))
                               (mergeq q1)
                               ;; q is list of lists of horizontal segments
                               (destructuring-bind (lk rk) (top1 q)
                                 (pop1 q)
                                 (let ((dy (- (y b1) (y lk))))
                                   (setf b1 (-y b1 dy)
                                         b2 (-y b2 dy))
                                   (push (make-point-bottom-left
                                          (x b1) (y b1) :ref (ref b1))
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
                                     (make-point-bottom-left
                                      (x (h 0)) (y p1) :ref (or (ref (h 0))
                                                                (ref p1)))
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


;; this might hold large data live, so change-class to rect before
;; passing back to user?
(defclass placement (rect)
  ;; internal stuff to speed up incremental updates, etc
  ((hole-point :reader hole-point :initarg :point)
   (hole :Reader hole :initarg :hole)))

(defun placing (w h c d hole)
  (declare (type vector c d))
  (let ((e)
        (m (1- (floor (length c) 2)))
        (p (1- (floor (length d) 2))))
    (when (or (< m 0) (< p 0))
      ;; C or D is empty, nothing fits
      (return-from placing nil))
    (labels ((l (x) (aref c (* x 2)))
             (r (x) (aref c (1+ (* x 2))))
             (ref-point (x)
               (or (ref (aref c (* x 2)))
                   (ref (aref c (1+ (* x 2))))))
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
        do (setf min-x (max min-x
                            (x (l i))
                            (x (ll j))))

           ;; if we have a valid placement, add it
        do (let* ((x (max min-x
                          (x (l i))
                          (x (ll j)))))
             (when (and (ok j i)
                        (not (falling j i x))
                        (not (sliding j i x)))
               (push (make-instance 'placement
                                    :x x :y (y (l i))
                                    :w w :h h :point (ref-point i)
                                    :hole hole)
                     e)
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
(defun valid-placement-p (hole w h x y)
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

(defun find-all-placements/sh (subhole w h hole)
  (let ((r nil))
    (do-dll/next (n subhole)
      (when (and (<= w (sh-max-width n))
                 (<= h (sh-max-height n)))
        (let ((d (make-d (sh-top n) w (sh-end n) (sh-falling-corner-p n))))
          (when (plusp (length d))
            (let ((c (make-c (sh-bottom n) w (sh-end n))))
              (setf r (append (placing w h c d hole)
                              r)))))))
    r))

(defun find-all-placements (hole w h)
  (let ((r nil))
    (do-dll/next (n hole)
      (when (and (<= w (ht-max-width n))
                 (<= h (ht-max-height n)))
        (setf r (append (find-all-placements/sh (h-subholes n) w h n)
                        r))))
    r))

(defclass placement-span (dll)
  ((x1 :initarg :x1 :accessor x1)
   (y1 :initarg :y1 :accessor y1)
   (x2 :initarg :x2 :accessor x2)
   (y2 :initarg :y2 :accessor y2)
   ;; list of keywords indicating how span touches quad (or that it is
   ;; a gap between contacts). see intersect-hole-with-quad for values
   (classify :initarg :classify :accessor classify)
))

;; some methods so we don't need to check types as often
(defmethod a ((p placement-span)) nil)
(defmethod b ((p placement-span)) nil)
(defmethod start ((p placement-span)) nil)
(defmethod end ((p placement-span)) nil)
(defmethod overlap-length ((p placement-span)) 0)

(defclass overlap-span (placement-span)
  ;; properties of intersection of a hole and a target placement quad,
  ;; stored as doubly linked list in CW order around the target quad

  ;; endpoints of an edge of a hole that overlaps an edge of target
  ;; placement, in CW order (segment between points might be part of
  ;; edge of quad, or extend past either or both ends)
  ((a :initarg :a :reader a)
   (b :initarg :b :reader b)
   ;; length of part that actually overlaps
   (overlap-length :initarg :length :reader overlap-length)
   ;; fraction of distance (0-1) from A to B of start and end of
   ;; overlap. (0,1) = entire span touches edge. Might be 0,0 or 1,1
   ;; (with length 0) if just corner touches corner of quad and that
   ;; ends up needed for the update code)
   (start :initarg :start :Reader start)
   (end :initarg :end :Reader end)))

(defun point-on-rect (x y r)
  (with-rect (nil x1 y1 w h) r
    (let ((x2 (+ x1 w))
          (y2 (+ y1 h)))
      (or (and (or (= x x1) (= x x2))
               (<= y1 y y2))
          (and (or (= y y1) (= y y2))
               (<= x1 x x2))))))

(defun p (v)
  (when v (list (hv-x v) (hv-y v))))

(defun overlap-p (span v1)
  (let* ((v2 (dll-next v1))
         (x1 (x1 span))
         (y1 (y1 span))
         (x2 (x2 span))
         (y2 (y2 span))
         (xmin (min x1 x2))
         (xmax (max x1 x2))
         (ymin (min y1 y2))
         (ymax (max y1 y2)))
    (format t "  check span ~s,~s - ~s,~s | edge ~s - ~s~%"
            x1 y1 x2 y2 (p v1) (p v2))
    (let ((r
            (cond
              ;; edge and span start at same point, and are co-linear
              ((or (and (= x1 x2 (hv-x v1) (hv-x v2))
                        (= (hv-y v1) y1))
                   (and (= y1 y2 (hv-y v1) (hv-y v2))
                        (= (hv-x v1) x1)))
               :exact-start)
              ;; edge starts somewhere in span, and is co-linear
              ((or (and (= x1 x2 (hv-x v1) (hv-x v2))
                        (< ymin (hv-y v1) ymax))
                   (and (= y1 y2 (hv-y v1) (hv-y v2))
                        (< xmin (hv-x v1) xmax)))
               :start)
              ;; edge ends at span
              ((or (and (= x1 x2 (hv-x v2))
                        (<= ymin (hv-y v1) ymax))
                   (and (= y1 y2 (hv-y v2))
                        (<= xmin (hv-x v1) xmax)))
               :to)
              ;; edge ends somewhere in span, and is co-linear
              ((or (and (= x1 x2 (hv-x v1) (hv-x v2))
                        (<= ymin (hv-y v2) ymax))
                   (and (= y1 y2 (hv-y v1) (hv-y v2))
                        (<= xmin (hv-x v2) xmax)))
               :end)
              ;; doesn't intersect
              (t
               nil))))
      (format t "   = ~s~%" r)
      r)))

(defun calculate-overlap (span v1)
  (let* ((v2 (dll-next v1))
         (x1 (x1 span))
         (y1 (y1 span))
         (x2 (x2 span))
         (y2 (y2 span))
         (x3 (hv-x v1))
         (y3 (hv-y v1))
         (x4 (hv-x v2))
         (y4 (hv-y v2))
         (den (- (* (- x1 x2)
                    (- y3 y4))
                 (* (- y1 y2)
                    (- x3 x4)))))

    (format t "  calculate span ~s,~s - ~s,~s | edge ~s - ~s~%"
            x1 y1 x2 y2 (p v1) (p v2))
    (labels ((d (y y1 y2)
               (/ (- y y1)
                  (- y1 y2)))
             (parallel (x1 y1 x2 y2 x3 y3 x4 y4)
               ;; written for vertical lines (x1=x2=x3=x4), called with
               ;; x,y swapped for horizontal
               (when (= x1)
                (let ((symin (min y1 y2))
                      (symax (max y1 y2))
                      (vymin (min y3 y4))
                      (vymax (max y3 y4)))
                  (let ((start (= y1 y3)) ;; both start at same point
                        (end (= y2 y4))   ;; both end at same point
                        ;; edge starts in span
                        (mid-start (< symin y3 symax))
                        ;; edge ends in span
                        (mid-end (< symin y4 symax))
                        ;; span starts inside edge
                        (overlap-start (< vymin y1 vymax))
                        ;; span ends inside edge
                        (overlap-end (< vymin y2 vymax)))
                    (cond
                      ;; edge and span coincide exactly
                      ((and start end)
                       (values :exact 0.0 1.0 x1 y1 x2 y2))
                      ;; edge and span start at same point, edge ends first
                      ((and start mid-end)
                       (values :start-middle 0 (d y2 y3 y4)
                               x1 y1 x3 y3))
                      ;; edge and span start at same point, span ends first
                      ((and start overlap-end)
                       (values :after-exact 0 (d y2 y3 y4)
                               x1 y1 x2 y2))
                      ;; edge starts in middle of span, both end at same point
                      ((and mid-start end)
                       (values :middle-end (d y1 y3 y4) 1
                               x3 y3 x2 y2))
                      ;; edge starts before span, both end at same point
                      ((and overlap-start end)
                       (values :before-exact (d y1 y3 y4) 1
                               x1 y1 x2 y2))
                      ;; edge extends past edge on both sides
                      ((and overlap-start overlap-end)
                       (values :spans
                               (d y1 y3 y4)
                               (d y2 y3 y4)
                               x1 y1 x2 y2))

                      ;; edge starts before span, stops in middle
                      ((and overlap-start mid-end)
                       (values :before
                               (d y1 y3 y4)
                               (d y2 y3 y4)
                               x1 y1 x2 y2))
                      ;; edge starts in middle of span, ends after end
                      ((and mid-start overlap-end)
                       (values :after
                               (d y1 y3 y4)
                               (d y2 y3 y4)
                               x1 y1 x2 y2))
                      (start
                       (values :from 0 0 x3 y3 x3 y3))
                      (mid-start
                       (break "shouldn't happen?"))
                      (end
                       (values :to-exact 1 1 x4 y4 x4 y4))
                      (mid-end
                       (values :to 1 1 x4 y4 x4 y4))
                      (t
                       (break "start=~s, end=~s~%mid-start=~s, mid-end=~s~%~
                              overlap-start=~s, overlap-end=~s,"
                              start end mid-start mid-end overlap-start overlap-end))))))))
      (cond
        ((and (zerop den) (= x1 x2 x3 x4))
         ;;lines are parallel and vertical
         (parallel x1 y1 x2 y2 x3 y3 x4 y4))
        ((and (zerop den) (= y1 y2 y3 y4))
         ;;lines are parallel and horizontal (reuse code from vertical,
         ;;with axes swapped
         (multiple-value-bind (type d1 d2 x1 y1 x2 y2)
             (parallel y1 x1 y2 x2 y3 x3 y4 x4)
           (values type d1 d2 y1 x1 y2 x2))
         )
        ((zerop den)
         ;; lines are parallel and not on same line
         nil
         )
        ((= x1 x2)
         ;; span is horizontal, check ends of line for intersection
         (cond
           ((and (= x3 x1) (<= y1 y3 y2))
            ;; edge starts on segment
            (values :to 0.0 0.0 x3 y3 x3 y3)
            )
           ((and (= x4 x1) (<= y1 y4 y2))
            ;; edge ends on segment
            (values :to 1.0 1.0 x4 y4 x4 y4)
            )))
        ((= y1 y2)
         (cond
           ((and (= y3 y1) (<= x1 x3 x2))
            ;; edge starts on segment
            (values :to 0.0 0.0 x3 y3 x3 y3)
            )
           ((and (= y4 y1) (<= x1 x4 x2))
            ;; edge ends on segment
            (values :to 1.0 1.0 x4 y4 x4 y4)
            ))

         )
        (t
         (break "?"))

      

        ))
    )
  )


(defun intersect-hole-with-quad (hole placement)
  ;; return spans of HOLE that overlap edges of quad defined by
  ;; x,y,w,h, along with coordinates and length of overlap.
  ;; also return total length of intersection as 2nd value, since
  ;; it's easy to calculate, for use in placement heuristics

  (let* ((len 0)
         (px1 (x placement))
         (py1 (y placement))
         (px2 (+ px1 (w placement)))
         (py2 (+ py1 (h placement)))
         (v (hole-point placement))
         (bottom (make-instance 'placement-span :x1 px2 :y1 py1 :x2 px1 :y2 py1
                                           :classify '(:gap :bottom)))
         (r bottom))
    ;; we build intersection in CW order, so edges are in same
    ;; direction as edges from hole
    (insert-before
     r (make-instance 'placement-span :x1 px1 :y1 py1 :x2 px1 :y2 py2
                                      :classify '(:gap :left)))
    (insert-before
     r (make-instance 'placement-span :x1 px1 :y1 py2 :x2 px2 :y2 py2
                                      :classify '(:gap :top)))
    (insert-before
     r (make-instance 'placement-span :x1 px2 :y1 py2 :x2 px2 :y2 py1
                                      :classify '(:gap :right)))


    ;; possibly should just pass placement if it stores hole?
    (assert (eql hole (hole placement)))

    ;; find beginning of overlap (possibly should save work here to
    ;; use in next steps, but not many edges in worst case, and don't
    ;; need to do full calculation here)
    ;;

    (let ((contact (overlap-p bottom v)))
      (ecase contact
        ;; if span and edge don't intersect, walk backwards on hole edge
        ;; until it is right of placement, then walk forwards until we get
        ;; contact with bottom of placement.
        ((nil)
         (format t "no contact @ ~s%" (p v))
         ;; move ccw around hole past placement
         (loop do (setf v (dll-prev v))
               while (<= (hv-x v) px2)
               do (format t "  -> ~s~%" (p v))
               when (eql v (hole-point placement))
                 do (break "couldn't find contact or point past placement?"))
         ;; then move CW until we find a contact with bottom, and
         ;; return it
         (loop for c = (overlap-p bottom v)
               for i from 0
               do (format t " ~s <- ~s~%" c (p v))
               until c
               do (setf v (dll-next v))
               when (> i 100) do (break "?"))
         )
        ;; hole edge starts before placement, and stops in middle of
        ;; bottom. can't continue around edge since then it would
        ;; have matched :start
        (:end

         )
        ;; hole edge starts in middle of bottom edge of placement,
        ;; back up to :to edge
        (:start
         (setf v (dll-prev v))
         (unless (eql :to (overlap-p bottom v))
           (break "got ~s instead of :to contact?"
                  (eql :to (overlap-p bottom v)))))


        ;; hole edge starts at lower right corner, so could continue
        ;; around the placement
        (:exact-start
         ;; while span and edge start at same point, move both to
         ;; prev. when hole edge extends past span, start at that
         ;; hole edge and span. if span extends past hole edge, start
         ;; at that span and previous hole edge (which should be a
         ;; :to or :from contact)
         (loop
           do ;; DO needed for loop-finish
              (setf v (dll-prev v))
              (setf r (dll-prev r))
              (let ((c (overlap-p r v)))
                (ecase c
                  (:exact-start ;; keep looping
                   (assert (not (eql r bottom))))
                  (:start
                   (setf v (dll-prev v))
                   (loop-finish))
                  ((:end :to)
                   (loop-finish))))))
        (:to
         ;;do nothing, we are at :to edge we wanted
         )))

    (format t "moved start points to ~s,~s~%" r v)
    (format t "  h edge = ~s,~s -> ~s,~s~%"
            (hv-x v) (hv-y v) (hv-x (dll-next v)) (hv-y (dll-next v)))
    (format t "  p span = ~s,~s -> ~s,~s~%"
            (x1 r) (y1 r) (x2 r) (y2 r))
    (format t "  contact ?= ~s~%" (overlap-p r v))
    (format t "  full contact = ~s~%"
            (multiple-value-list (calculate-overlap r v)))



    ;; walk vertices of hole, checking for overlaps. Follow Q->N
    ;; links, since other subholes can't extend back into this one to
    ;; touch the placement. When we go above the top edge of placement
    ;; we can skip to the falling edge of the subhole if any (need to
    ;; store that to skip to it though).

    ;; we only check current span of placement, and advance it when we
    ;; fill the span with contacts, see a perpendicular edge that
    ;; straddles it, or see a parallel span that extends past it
    (flet ((len (a b c d)
             (sqrt (+ (expt (- a c) 2)
                      (expt (- b d) 2)))))
      (loop
        with v1 = v
        do (multiple-value-bind (type d1 d2 x1 y1 x2 y2)
               (calculate-overlap r v)
             (let ((edge (second (classify r)))
                   (next (dll-next v)))
               (when type
                 (let ((l (len x1 y1 x2 y2)))
                   (incf len l)
                   (format t "test edge ~s -> ~s~%" (p v) (p next))
                   (format t "     span ~s: ~s,~s -> ~s~s~%"
                           (classify r) (x1 r) (y1 r) (x2 r) (y2 r))
                   (format t "  == ~s: ~s ~s = ~s,~s - ~s,~s~%"
                           type d1 d2 x1 y1 x2 y2)
                   (ecase type
                     ((:exact :before-exact :after-exact :spans)
                      ;; entire span is covered by edge, replace it completely
                      (change-class r 'overlap-span
                                    :start d1
                                    :end d2
                                    :length l
                                    :a v
                                    :b next
                                    :classify (list type edge)
                                    :x1 x1 :y1 y1
                                    :x2 x2 :y2 y2))
                     (:from
                      (let ((n (make-instance
                                'overlap-span
                                :start d1
                                :end d2
                                :length l
                                :a v
                                :b next
                                :classify (list :from edge)
                                :x1 x1 :y1 y1
                                :x2 x2 :y2 y2)))
                        ;; if both start at same point and diverge, we
                        ;; need a contact at the corner, then continue
                        ;; checking current span (shouldn't be able to
                        ;; diverge in middle since we should have
                        ;; already seen a contact along span and split
                        ;; it
                        (insert-before r n)))
                     (:to
                      (let ((n (make-instance
                                'overlap-span
                                :start d1
                                :end d2
                                :length l
                                :a v
                                :b next
                                :classify (list :to edge)
                                :x1 x1 :y1 y1
                                :x2 x2 :y2 y2))
                            ;; split the span into 2, with contact in
                            ;; the middle, so we can match remaining
                            ;; span against next edge
                            (np (make-instance
                                 'placement-span
                                 :classify (classify r)
                                 :x1 x1 :y1 y1
                                 :x2 (x2 r) :y2 (y2 r))))
                        (setf (x2 r) x1
                              (y2 r) y1)
                        (insert-after r n)
                        (insert-after n np)
                        (setf r np))))))
               (ecase edge
                 (:bottom
                  (format t "advance from bottom? ~s ~s ~s~%"
                          (<= (hv-x next) px2)
                          (> (hv-y v) py2)
                          (> (hv-y next) py2))
                  (when (or (<= (hv-x next) px2)
                            (> (hv-y v) py2)
                            (> (hv-y next) py2))
                    (setf r (dll-next r)))
                  (setf v next))
                 (:left
                  (format t "advance from left? ~s | ~s ~s ~s~%"
                          (> (hv-y next) py2)
                          (>= (hv-y next) py2)
                          (> (hv-x v) px2)
                          (> (hv-x next) px2))
                  (if (> (hv-y next) py2)
                      ;; todo: can skip V directly to falling edge if hv-y2
                      ;; is strictly greater than py2, since it won't go
                      ;; lower until then (can touch top or right edge of
                      ;; placement)
                      (setf v next)
                      (setf v next)
                      )
                  
                  (when (or (> (hv-y next) py2)
                            (> (hv-x v) px2)
                            (> (hv-x next) px2))
                    (setf r (dll-next r))))
                 (:top
                  ;; todo
                  (if (> (hv-y next) py2)
                      ;; todo: skip V directly to falling edge
                      (setf v next)
                      (setf v next))
                  (when (or (> (hv-x v) px2)
                            (> (hv-x next) px2))
                    (setf r (dll-next r))))
                 (:right
                  (if (> (hv-y next) py2)
                      (setf v next)
                      (setf v next))
                  )

                 )))
        until (eql v v1)))

    #++(break "r ~s~%" r)
    (values r len)))

(let ((h (init-hole 8 8)))
  (intersect-hole-with-quad h
                            (make-instance 'placement
                                           :x 0 :y 0 :w 3 :h 4
                                           :point (h-vertices h)
                                           :hole h)))

(defun remove-quad-from-hole (hole placement
                              &key (overlap (intersect-hole-with-quad
                                             hole placement)))
  (assert (typep overlap 'placement-span))
  (format t "remove quad: ~s,~s ~sx~s~%"
          (x placement) (y placement)
          (w placement) (h placement))

  (let ((i 0))
    (do-dll/next (o overlap)
      (format t "~s: ~s~%" i o)
      (format t " ~s,~s - ~s,~s~%"
              (x1 o) (y1 o) (x2 o) (y2 o))
      (when (typep o 'overlap-span)
        (format t "  ~s , ~s @ ~s, ~s~%" (a o) (b o) (start o) (end o))
        (format t "  ~s,~s  ~s,~s~%"
                (hv-x (a o)) (hv-y (a o))
                (hv-x (b o)) (hv-y (b o))))
      (incf i)))

  ;; find first non-gap span to simplify later logic
  (let ((start nil))
    (do-dll/next (v overlap)
      (when (and (typep v 'overlap-span) (not start))
        (setf start v)
        (return nil)))
    (assert start)
    (format t "move overlap from ~s to ~s~%" overlap start)
    (setf overlap start))
  (let (;; links that should be broken
        (to-disconnect (make-hash-table))
        ;; links that should be preserved or created, in specified
        ;; directions
        (connect-key<-value (make-hash-table))
        (connect-key->value (make-hash-table))
        ;; nodes that need updated in keys (disconnected or connected)
        ;; (hash table to avoid duplicates efficiently)
        (worklist (make-hash-table))
        ;; subholes possibly affected by this placement (not
        ;; exhaustive list, just those reachable from following
        ;; vertices stopping at Q verts, from disconnected links)
        (affected-subholes (make-hash-table))
        (affected-q (make-hash-table))
        ;; vertex under new or moved left notch (= possible new subhole)
        (left-notch (make-hash-table))
        ;; number of new gaps indicates # of holes in result: 0 =
        ;; completely filled hole. 1 = filled part of hole without
        ;; creating any new holes (may have created or removed
        ;; subholes), N>1 = created N-1 holes
        (new-gaps nil)

        ;; vertices to be moves (or possibly combined with another)
        (to-move (make-hash-table))
        ;; any holes created by the placement (mostly for debugging
        ;; currently)
        (new-holes nil)
        (first-point nil)
        (move-q (make-hash-table)))
    ;; walk through contacts deciding what needs done to vertices
    (labels ((disconnect (n dir)
               (format t "disconnect ~s ~s (= ~s~%~%" (p n) dir
                       (if (eql :next dir)
                           (p (dll-next n))
                           (p (dll-prev n))))
               (unless n (break "disconnect ~s ~s?" n dir))

               (setf (gethash n worklist) t)

               (if (and (gethash n to-disconnect)
                        (not (eql dir (gethash n to-disconnect))))
                   (setf (gethash n to-disconnect) t)
                   (setf (gethash n to-disconnect) dir)))
             (mark-affected (start)
               (format t "mark affected from ~s~%" (p start))
               (do-hole-vertices/next (v start dx nil)
                 (format t "  check ~s~%" (p v))
                 (when (hv-sh v)
                   (format t "  affected subhole ~s (~s)~%" (hv-sh v)
                           (p (sh-start (hv-sh v))))
                   (setf (gethash (hv-sh v) affected-subholes) t)
                   (return nil))
                 (when (and dx (plusp dx))
                   (return nil))
                 (when (hv-w v)
                   (if (< (hv-y v) (+ (y placement) (h placement)))
                       (progn
                         (format t "  affected q ~s~%" v)
                         (break "check this")
                         (setf (gethash v affected-q) t))
                       (return nil)))))
             (remove-node (n)
               ;; if vertex was start or end of a subhole, we need
               ;; to update it
               (let ((sh (hv-sh n)))
                 (format t "@@@ remove node ~s: ~s~%" (p n) sh)
                 (when sh
                   (format t "  == ~s ~s~%" (p (sh-start sh))
                           (p (sh-endv sh))))
                 (when sh
                   (cond
                     ((eql n (sh-start sh))
                      ;; save it for update later, since we need to
                      ;; search for new start, and might have
                      ;; deleted subhole completely
                      (format t "  update subhole~%")
                      (setf (gethash sh affected-subholes) t))
                     ((and (eql n (sh-endv sh)))
                      (let ((p (dll-prev n)))
                        (when (hv-w n)
                          (setf (gethash n move-q) (list sh p)))
                        (format t "adjust end ~s -> ~s~%~s -> ~s~%"
                                (sh-endv sh) p
                                (p (sh-endv sh)) (p p))
                        (setf (sh-endv sh) p)
                        (setf (hv-sh p) sh))))))
               (when (eql n (h-vertices hole))
                 (setf (h-vertices hole) (dll-prev n)))
               (delete-node n))
             (add-or-update-left-notch (p)
               (format t "possible new or updated left notch at ~s~%" (p p))
               (setf (gethash p left-notch) t))
             (p (w) (when w (list (hv-x w) (hv-y w))))
             (print-hole (h)
               (do-hole-vertices/next (v (h-vertices h) dx dy)
                 (format t "   ~s (~s / ~s) n:~s w:~s q:~s~%" (p v) dx dy
                         (when (hv-n v) (p (hv-n v)))
                         (when (hv-w v) (p (hv-w v)))
                         (when (hv-q v) (p (hv-q v)))))
               (format t "  subholes:~%")
               (do-dll/next (sh (h-subholes h))
                 (format t "   ~s: start ~s, end ~s~%"
                         sh (p (sh-start sh)) (p (sh-endv sh))))
               ))
      (format t "~%~%remove ~s from hole:~%" (list (x placement)
                                                   (y placement)
                                                   (+ (x placement)
                                                      (w placement))
                                                   (+ (x placement)
                                                      (h placement))))
      (do-hole-vertices/next (v (h-vertices hole) dx dy)
        (format t "  ~s (~s / ~s)~%" (p v) dx dy))
      (format t "subholes:~%")
      (do-dll/next (sh (h-subholes hole))
        (format t "  ~s: start ~s, end ~s~%"
                sh (p (sh-start sh)) (p (sh-endv sh))))
      (let ((index -1)
            (prev nil)
            (first nil)
            (in-gap nil))
        (do-dll/next (i overlap end)
          (format t "### ~s ~s (prev ~s~%~%"i end prev)
          (incf index)
          (let ((a (a i))
                (b (b i))
                (start (start i))
                (end (end i))
                (classify (car (classify i)))
                (edge (cadr (classify i))))
            (format t "~s: ~s ~s~% ~s .. ~s~% ~a~%"
                    index classify edge
                    start end
                    (list a b)
                    )
            (flet ((todo (x)
                     (break "~s: ~s ~s~% ~s .. ~s~% ~a ~a"
                            x classify edge
                            start end
                            a b)
                     )
                   (start-or-continue-gap (a)
                     (format t "start or continue gap ~s (was ~s)~%"
                             (p a) (mapcar #'p in-gap))
                     (push a in-gap))
                   (end-gap (a)
                     (format t "end gap at ~s (from ~s)~%"
                             (p a) (mapcar #'p in-gap))
                     (when in-gap
                       (assert a))
                     (when in-gap
                       (push (list* a in-gap) new-gaps))
                     (setf in-gap nil)))
              (case classify
                (:before-exact
                 (let ((p (make-hole-vertex (x1 i) (y1 i))))
                   (format t "add point1 ~s @ ~s - ~s~%"
                           (p p) (p a) (p b))
                   (unless first
                     (setf first p))
                   (insert-after a p)
                   (setf prev nil)
                   (disconnect p :next)
                   (disconnect (dll-next p) :prev)
                   (unless first-point
                     (setf first-point p))
                   (end-gap a)))
                (:after-exact
                 (let ((p (make-hole-vertex (x2 i) (y2 i))))
                   (format t "add point2 ~s @ ~s, ~s~%"
                           (p p) (p a) (p b))
                   (unless first
                     (setf first p))
                   (insert-after a p)
                   (mark-affected b)
                   (setf prev p)
                   (disconnect a :next)
                   (disconnect p :prev)
                   (end-gap a)))
                (:exact
                 ;(disconnect a :prev)
                 (disconnect a :next)
                 (disconnect b :prev)
                 ;(disconnect b :next)
                 (end-gap nil))
                (:gap
                 (assert prev)
                 (cond
                   ((and first
                         (= (x2 i) (hv-x first))
                         (= (y2 i) (hv-y first)))
                    (format t "gap = first @ ~s~%" (p first))
                    (format t "close final gap @ ~s (from ~s)~%"
                            (p prev) (p first))
                    (push (list first prev ) new-gaps))
                   (t
                    (let ((p (make-hole-vertex (x2 i) (y2 i))))
                      (format t "  add gap ~s @ ~s~%"
                              (p p) (p prev))
                      (assert (= (hv-x prev) (x1 i)))
                      (assert (= (hv-y prev) (y1 i)))
                      (unless first
                        (setf first p))
                      ;; spans are clockwise around outside of (filled)
                      ;; quad, so we add them reversed into hole, so
                      ;; they will be clockwise around empty hole
                                        ;(connect p prev)
                      (insert-before prev p)
                      (setf prev p)
                      ;; todo: add new spans to a worklist for
                      ;; checking for new left notches, or blocked
                      ;; Q-QW links
                      )))
                 (start-or-continue-gap prev))
                (:to
                 (format t "to: ~s,~s @ ~s,~s ~s,~s~%"
                         (p a) (p b) (x1 i) (y1 i) (x2 i) (y2 i))
                 (mark-affected b)
                 (unless (and (= (hv-x b) (x1 i))
                              (= (hv-y b) (y1 i)))
                   #++(when (hv-n b)
                        (break "handle q vert"))
                   (format t "move 'to' vertex to  ~s,~s~%" (x1 i) (y1 i))
                   (assert (not (gethash b to-move)))
                   (setf (gethash b to-move) (list (x1 i) (y1 i))))
                 (setf prev b)
                 (end-gap b))
                (:away
                 (mark-affected a)

                 (unless (and (= (hv-x a) (x2 i))
                              (= (hv-y a) (y2 i)))
                   (format t "move 'away' vertex to  ~s,~s~%" (x2 i) (y2 i))
                   #++(when (hv-n a)
                        (break "handle q vert"))
                   (assert (not (gethash a to-move)))
                   (setf (gethash a to-move) (list (x2 i) (y2 i)))
                   )
                 (setf prev a)
                 (end-gap nil))
                (:start-middle
                 (disconnect a :next)
                 (disconnect b :prev)
                 (mark-affected b)
                 (setf prev b)
                 (end-gap a))
                (:middle-end
                 (disconnect a :next)
                 (disconnect b :prev)
                 (unless first
                   (setf first a))
                 (mark-affected b)
                 (setf prev nil)
                 ;; if a leftwards edge starts in the middle of a
                 ;; placement rect, we either closed a hole (if there
                 ;; is another contact to the right of this point),
                 ;; moved a left notch (if point A was a Q node), or
                 ;; created a left notch
                 (when (and (not in-gap)
                            (= (hv-y a) (hv-y b))
                            (> (hv-x a) (hv-x b)))
                   (add-or-update-left-notch a))
                 (end-gap a))
                (:middle
                 (mark-affected b)
                 (let* ((p1 (make-hole-vertex (x1 i) (y1 i) a))
                        (p2 (make-hole-vertex (x2 i) (y2 i) p1)))
                   (format t "  add middle points ~s, ~s~%" (p p1) (p p2))
                   (disconnect p1 :next)
                   (disconnect p2 :prev)
                   (setf prev p2)
                   (end-gap p1))
)


                (otherwise
                 (todo "todo"))
                ))))

)

      ;; perform updates

      (let ((updated-subholes nil)
            (remove-subholes nil))
        (loop for work in (alexandria:hash-table-keys worklist)
              for d = (gethash work to-disconnect)
              for from = (gethash work connect-key<-value)
              for to = (gethash work connect-key->value)
              do (format t "disconnect: ~s @ ~s (~s ~s)~%"
                         (p work) d (p from) (p to))
                 (format t "  @@ ~s, ~s~%"
                         (p (dll-prev work)) (p (dll-next work)))
              when (eql d t)
                do (remove-node work))

        (when (plusp (hash-table-count to-move))
          (loop for i being the hash-keys of to-move using (hash-value (tx ty))
                do (cond
                     ((or (and (= tx (hv-x (dll-prev i)))
                               (= ty (hv-y (dll-prev i))))
                          (and (= tx (hv-x (dll-next i)))
                               (= ty (hv-y (dll-next i)))))
                      (format t "remove duplicate node @ ~s (~s) (~s ~s)~%"
                              (p i)
                              (list tx ty)
                              (p (dll-prev i))
                              (p (dll-next i)))
                      (delete-node i))
                     (T
                      (format t "move node from ~s to ~s~%"
                              (p i) (list tx ty))
                      (setf (hv-x i) tx
                            (hv-y i) ty)))))


        (when (> (length new-gaps) 1)
          (format t "vvv before new-gaps:~%")
          (loop for h in (list* hole new-holes)
                for i from 0
                do (format t "hole ~s/~s ~s:~%" i (length new-holes) h)
                   (print-hole h)))

        (let ((l (length new-gaps)))
          (format t "new gaps ~s~%" l)
          (loop for g in new-gaps
                for i from 1
                do (format t " ~s/~s = ~s~%" i l (mapcar #'p g)))
          (cond
            ((zerop l)
             (break "no gaps?")
             (format t "filled hole ~s~%" hole)
             (return-from remove-quad-from-hole nil))
            ((> l 1)
             (let ((update-next (make-hash-table))
                   (update-prev (make-hash-table))
                   (modified-holes))
              (flet ((reconnect (from to)
                       ;; store changes so we can try to keep DLLs valid
                       (setf (gethash from update-next) to)
                       (setf (gethash to update-prev) from)))
                (loop for gap in new-gaps
                      for verts = gap
                      for h = hole then (make-instance 'hole)
                      ;; add hole to dll if new
                      do (unless (eql h hole)
                           (insert-before hole h))
                         (push h modified-holes)
                         (setf (h-vertices h) (first verts))
                         ;; connect
                         (loop for (from to) on verts
                               while to
                               do (reconnect from to))))


               (loop for from being the hash-keys of update-next
                       using (hash-value to)
                     ;; previous NEXT
                     for old-next = (dll-next from)
                     ;; NEXT should have a new value for prev
                     for old = (gethash old-next update-prev)
                     do (format t "  update link: ~s -> ~s~%" (p from) (p to))
                        (format t "  was ~s -> ~s~%" (p from) (p old-next))
                        (format t "  prev = ~s~%" (when old (p old)))
                        (assert old)
                        (assert old-next)
                        (rotatef (dll-prev old-next)
                                 (dll-prev to))
                        (rotatef (dll-next from)
                                 (dll-next old)))

               ;; for now just rebuild all holes we changed. not sure
               ;; if we can reliably update subholes in place or not?
               ;; would have to at least walk enough to find which
               ;; hole it is in now
               (loop for a in modified-holes
                     do (setf (h-subholes a) (make-subholes a)))
               (format t "^^^ after new-gaps:~%")
               (loop for h in modified-holes
                     for i from 1
                     do (format t "hole ~s/~s ~s:~%"
                                i (length modified-holes) h)
                        (format t " ~s <-> ~s~%"
                                (dll-prev h) (dll-next h))
                        (print-hole h))
               (return-from remove-quad-from-hole nil))

             )
            ((= l 1)
             (format t "no new holes")))
)

        (when (plusp (hash-table-count affected-subholes))
          ;; for each subhole that needs updated, start from it's
          ;; "end" point, and search CW until we find either a
          ;; rightward edge after a leftwards edge (in which case we
          ;; just passed the leftmost edge), or a Q vertex, in which
          ;; case the subhole has been removed and that Q can be
          ;; removed (and subhole it was in gets the END vertex from
          ;; this one).  While searching, we need to mark the
          ;; beginning of the first leftwards edge as new Q point
          (format t "affected-subholes: ~s~%"
                  (hash-table-count affected-subholes))
          (loop with leftp = nil
                with rightp = nil
                with q = nil
                for sh in (alexandria:hash-table-keys affected-subholes)
                for hole = (sh-hole sh)
                for end = (sh-endv sh)
                do (format t " check subhole ~s ~s - ~s~%"
                           sh (p (sh-start sh)) (p (sh-endv sh)))
                   (do-hole-vertices/next (v end dx dy endp)
                        (format t "  check ~s dx:~s dy:~s~%" (p v) dx dy)
                        (cond
                          ((and (hv-n v)
                                (not (eql sh (hv-sh v)))
                                )
                           ;; found Q vertex from another subhole, remove
                           ;; this subhole, and update the END link
                           (do-dll/next (sh2 (h-subholes hole) endp)
                             ;; todo: make some sort of backref or index
                             ;; to find subhole for a q vertex
                             (when (eql (sh-endv sh2) v)
                               (setf (sh-endv sh2) (or q end))
                               (setf (sh-end sh2) (hv-x (or q end)))
                               (when q
                                 (when (hv-n end)
                                   (setf (gethash end move-q) (list sh2 end)))
                                 (setf (gethash v move-q) (list sh2 q)))
                               (return nil))
                             (format t "   remove subhole ~s (~s - ~s)~%"
                                     sh (p (sh-start sh))
                                     (p (sh-endv sh)))
                             #++
                             (when endp
                               (break "couldn't find subhole for q vertex?")))
                           (setf (sh-end sh) nil)
                           (push sh remove-subholes)
                           (return nil))

                          ((and dx (minusp dx))
                           ;; found Q or end vertex for this subhole.
                           ;;
                           (unless q
                             (setf q (dll-prev v)))
                           (setf leftp t))
                          ((and dx (plusp dx) (not leftp))
                           ;; looking for new Q vertex
                           (setf rightp t)
                           (assert (not q)))
                          ((and dy (plusp dy) (not leftp))
                           ;; found right edge or falling edge, subhole
                           ;; has been removed or separated
                           (setf (sh-end sh) nil)
                           (push sh remove-subholes)
                           (return nil))
                          ((and dx (plusp dx))
                           (assert q)
                           ;; found leftmost edge (must have gone left at
                           ;; least once, and up in between, so prev
                           ;; twice should be OK
                           (format t "    move start of subhole ~s from ~s to ~s~%      @  ~s <- ~s~%"
                                   sh (p (sh-start sh))
                                   (p (dll-prev (dll-prev v)))
                                   (p (dll-prev v)) (p v)
                                   )
                           (format t "      q = ~s (~s), leftp = ~s, rightp = ~s~%"
                                   (p q) (if (hv-n end) t nil) leftp rightp)
                           (setf (sh-start sh) (dll-prev (dll-prev v)))
                           (setf (hv-sh (sh-start sh)) sh)
                           ;; update or create Q vertex
                           (when (or (hv-n end) rightp)
                             (unless (eql q end)
                               (setf (gethash end move-q) (list sh q))
                               (setf (sh-endv sh) q)
                               (setf (sh-end sh) (hv-x q))))
                           (push sh updated-subholes)
                           (return nil))
                          )
                        )
                ))


        (when (plusp (hash-table-count left-notch))
          #++
          (break "check for new/updated left notch~%~s~%~s"
                 (mapcar #'p (alexandria:hash-table-keys left-notch))
                 left-notch)
          (loop for ln being the hash-keys of left-notch
                for leftp = nil
                for rightp = nil
                for q = nil
                do (do-hole-vertices/next (v ln dx dy)
                     (cond
                       ((and dy (minusp dy))
                        ;; no new subhole
                        (return nil))
                       ((and dx (minusp dx))
                        (unless q
                          (setf q (dll-prev v)))
                        (setf leftp t))
                       ((and dx (plusp dx) leftp)
                        ;; just passed leftmost edge

                        (let ((sh (make-subhole hole
                                                (dll-prev v)
                                                (h-subholes hole)
                                                :update nil
                                                )))
                          (assert q)
                          (assert (not (gethash q move-q)))
                          (setf (gethash q move-q) (list sh q))
                          (push sh updated-subholes))
                        (return nil))))))

        (format t "remove-subholes ~s~%" remove-subholes)
        (format t "move-q ~s~%" (mapcar #'p(alexandria:hash-table-keys move-q)))


        (when remove-subholes
          (loop for sh in remove-subholes
                for start = (sh-start sh)
                do (format t "remove ~s @ ~s~%" sh (p start))
                   (assert (and (not (dll-next start))
                                (not (dll-prev start))))
                   (when (eql sh (h-subholes hole))
                     (setf (h-subholes hole)
                           (dll-next sh)))
                   (delete-node sh)

)
)
        ;; todo: update Q links for holes that are still live, if they
        ;; overlap placement
        (when (plusp (hash-table-count move-q))
          #++(break "move ~s" (alexandria:hash-table-alist move-q))
          (loop for q being the hash-keys of move-q using (hash-value (sh to))
                for n = (hv-n q)
                for w = (hv-w q)
                do (format t "move q ~s -> ~s: ~s ~s~%"
                           (p q) (p to) (p n) (p w))
                   (when n
                        ;; remove old N links
                        (setf (hv-n q) nil)
                        (setf (hv-q n) nil)
                        ;; remove N node completely if possible
                        (when (= (hv-y (dll-prev n))
                                 (hv-y n)
                                 (hv-y (dll-next n)))
                          (delete-node n)))
                   (when w
                     ;; remove old W links
                     (setf (hv-w q) nil)
                     (setf (hv-q w) nil)
                     ;; remove W node completely if possible
                     (when (= (hv-x (dll-prev w))
                              (hv-x w)
                              (hv-x (dll-next w)))
                       (delete-node w)))
                   ;; move the point
                   (setf q to)
                   ;; find new N and W points
                   (when (hv-classify q)
                     (assert (equalp (hv-classify q) '(:left-notch :top))))
                   (let ((e (classify-vertical-edge q (dll-prev q))))
                     (when e
                       (assert (eql e :left-notch))
                       (setf (hv-classify q) '(:left-notch :top))
                       (set-qnw q)))




)
)


        (format t "^^^ after uipdates:~%")
        (loop for h in (list* hole new-holes)
              for i from 0
              do (format t "hole ~s/~s ~s:~%" i (length new-holes) h)
                 (print-hole h))

        (format t "updated = ~s~%" updated-subholes)

        ;; rebuild top/bottom data structures for any modified
        ;; subholes that weren't removed
        (when updated-subholes
          (map nil 'update-subhole updated-subholes)
          )))

    )



  )

(init-hole 256 256)
#++
(ql:quickload '(binpack parachute))

#++
(let* ((h (%make-hole-from-points binpack-test::*hole-fig6b*))
      (p (time (find-all-placements h 1 1))))
  (time
   (mapcar
    (lambda (p) (multiple-value-list (intersect-hole-with-quad h p)))
    p
    )))
#++
(let* ((h (binpack2-vis::hole binpack2-vis::*w*))
       (p (time (find-all-placements
                 h
                 (* 16 (binpack2-vis::pwx binpack2-vis::*w*))
                 (* 16 (binpack2-vis::pwy binpack2-vis::*w*))))))
  (format t "--------------~%")
  (binpack2-vis::redraw-hole binpack2-vis::*w*)
  (format t "--------------~%")
  (time
   (mapcar
    (lambda (p) (remove-quad-from-hole h p ))
    p
    )))

(defun print-hole (h)
  (let ((l nil))
    (do-dll/next (v (h-vertices h))
      (push (hv-x v) l)
      (push (hv-y v) l))
    (format t "    ~s~%" (nreverse l)))
)
