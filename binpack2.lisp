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


(defun fff (&rest r) (declare (ignore r)))

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
   ;; Q<->QN and Q<->QW links. Links from Q to N/W if Q is also set,
   ;; otherwise from N/W back to Q
   (n :accessor hv-n :initform nil :initarg :qn)
   (w :accessor hv-w :initform nil :initarg :qw)
   ;; true if node is a Q vertex (top of left notch), NIL otherwise
   (q :accessor hv-q :initform nil :initarg :q)
   ;; what type of vertical edge this vertex is corner of
   ;; (:left-notch, :leftmost, :rightmost, :falling, etc) or :QW or
   ;; :QN for QW or QN vertices in middle of span
   (classify :accessor hv-classify :initform nil)
   ;; where this vertex is on a vertical edge, :top,:middle,:bottom,
   ;; or :middle if it is QN vertex in middle of horizontal span
   (corner :accessor hv-corner :initform nil)
   ;; if this vertex is the bottom of a leftmost edge of a subhole, or
   ;; 'end' vertex for a subhole, link to the subhole
   (sh :accessor hv-sh :initform nil)
   ;; if vertex is falling corner, link to hole
   (hole :accessor hv-hole :initform nil)))

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

(defun up-down-dir (v)
  (cond
    ((or (< (hv-y v) (hv-y (dll-prev v)))
         (> (hv-y v) (hv-y (dll-next v))))
     (values 'dll-prev 'dll-next :falling))
    ((or (< (hv-y v) (hv-y (dll-next v)))
         (> (hv-y v) (hv-y (dll-prev v))))
     (values 'dll-next 'dll-prev :rising))
    (t nil)))


(defun extend-edge-up (v dir)
  ;; find local maximum of Y coord along this edge
  (unless dir
    (return-from extend-edge-up v))
  (loop for n = (funcall dir v)
        while (and (= (hv-x v) (hv-x n))
                   (< (hv-y v) (hv-y n)))
        do (setf v n))
  v)
(defun extend-edge-right (v dir)
  ;; find local maximum of x coord along this edge
  (unless dir
    (return-from extend-edge-right v))
  (loop for n = (funcall dir v)
        while (and (= (hv-y v) (hv-y n))
                   (< (hv-x v) (hv-x n)))
        do (setf v n))
  v)

(defun extend-edge-down (v dir)
  ;; find local minimum of Y coord along this edge
  (unless dir
    (return-from extend-edge-down v))
  (loop for n = (funcall dir v)
        while (and (= (hv-x v) (hv-x n))
                   (> (hv-y v) (hv-y n)))
        do (setf v n))
  v)

(defun classify-vertical-edge (v)
  (multiple-value-bind (up down dir) (up-down-dir v)
    (let* ((top (extend-edge-up v up))
           (bottom (extend-edge-down v down))
           (rising (eql dir :rising))
           (falling (eql dir :falling))
           (corner (cond ((eql top v) :top)
                         ((eql bottom v) :bottom)
                         (t :middle))))
      (unless (and up down)
        ;; N vertex or bug?
        (assert (eql (hv-classify v) :qn))
        (assert (eql (hv-corner v) :middle))
        (return-from classify-vertical-edge
          (values (hv-classify v) (hv-corner v))))
      (when (eql top bottom)
        (break "degen")
        (return-from classify-vertical-edge
          (values :degenerate :middle)))

      (let ((dxt (- (hv-x (funcall up top)) (hv-x top)))
            (dxb (- (hv-x (funcall down bottom)) (hv-x bottom))))
        (values
         (cond
           ((and (not (eql v top))
                 (not (eql v bottom)))
            (assert (eql (hv-corner v) :middle))
            (hv-classify v))
           ((and rising (plusp dxt) (plusp dxb))
            ;; cw: -x +y +x
            :leftmost)
           ((and rising (minusp dxt) (minusp dxb))
            ;; cw +x +y -x
            :left-notch)
           ((and falling (minusp dxt) (plusp dxb))
            ;; cw: +x -y +x
            :falling-edge)

           ;; shouldn't happen in bottom-left packing
           ((and falling (plusp dxt) (plusp dxb))
            ;; cw: -x -y +x
            :right-notch)

           ;; these aren't interesting, but including for debugging for now
           ((and rising (minusp dxt) (plusp dxb))
            ;; CW: -x +y -x
            :step-up)
           ((and rising (plusp dxt) (minusp dxb))
            ;; cw: +x +y +x
            :rising-edge)
           ((and falling (plusp dxt) (minusp dxb))
            ;; cw: -x -y -x
            :step-down)
           ((and falling (minusp dxt) (minusp dxb))
            ;; cw: +x -y -x
            :rightmost)
           ((and (not falling) (not rising) (eql (hv-corner v) :middle))
            (hv-classify v))
           (t (error "~s"(list :dxt dxt :dxb dxb :rising rising :falling falling))))
         corner)))))


(defun set-qnw (v)
  ;; build Q->N and Q->W links for a hole that doesn't have any old
  ;; Q->N links from before this left-notch was added
  (assert (eql (hv-classify v) :left-notch))
  (assert (eql (hv-corner v) :top))
  (setf (hv-q v) t)
  ;; walk NEXT until we find an X value right of Q
  (let ((x1 (hv-x v))
        (y1 (hv-y v)))
    (block h
      (do-dll/next (v2 (dll-next v))
        (let* ((v3 (dll-next v2))
               (x2 (hv-x v2))
               (x3 (hv-x v3)))
          (cond
            ((and (= x1 x2)
                  ;; not top or middle of a falling edge
                  (<= (hv-y v2)
                      (hv-y (dll-next v2))))
             ;; directly under a vertex, just link to it
             (assert (not (hv-n v2)))
             ;; can't be a :top or :middle corner
             (assert (eql (hv-corner v2) :bottom))
             (setf (hv-n v) v2)
             (setf (hv-n v2) v)
             (return-from h nil))
            ((< x2 x1 x3)
             (assert (= (hv-y v2) (hv-y v3)))
             ;; under an edge, split it and link to new vertex
             (let ((n (make-hole-vertex x1 (hv-y v2) v2)))
               (setf (hv-n v) n)
               (setf (hv-n n) v)
               (setf (hv-classify n) :qn)
               (setf (hv-corner n) :middle)
               (return-from h nil)))
            ((and (hv-n v2) (hv-q v2)
                  (< x2 x1))
             ;; follow Q->N links, but make sure next iteration sees the
             ;; N vertex so we can notice if Q is between that and next vertex
             (setf v2 (dll-prev (hv-n v2))))
            ((or (> x2 x1) (eql v2 v))
             (error "couldn't find edge above left notch?"))))))

    ;; walk PREV until we find a Y value above Q
    (assert (eql (hv-classify (dll-prev v)) :left-notch))
    (assert (eql (hv-corner (dll-prev v)) :bottom))
    (let ((rightmost nil)
          (count 0))
      (do-dll/prev (v2 (dll-prev v))
        (let* ((v3 (dll-next v2))
               (y2 (hv-y v2))
               (y3 (hv-y (dll-next v2))))
          (when (or (eql (hv-classify v2) :rightmost)
                    (eql (hv-classify v2) :falling-edge))
            (setf rightmost t))
          (incf count)
          (when (> count 1000)
            (error "looped?"))
          (cond
            ((and (= y2 y1)
                  ;; not top of rightmost edge next to falling edge
                  (<= (hv-x v2)
                      (hv-x (dll-prev v2))))
             (assert (/= (hv-x v) (hv-x v2)))
             (assert (not (hv-w v2)))
             ;; directly left of another vertex (upper left of lower
             ;; notch or step down) use it directly
             (setf (hv-w v) v2)
             (setf (hv-w v2) v)
             (return-from set-qnw t))

            ((< y3 y1 y2)
             (assert (= (hv-x v2) (hv-x v3)))
             ;; left of an edge, split it and link to new vertex
             (let ((w (make-hole-vertex (hv-x v2) y1 v2)))
               (setf (hv-w v) w)
               (setf (hv-w w) v)
               (setf (hv-classify w) :qw)
               (setf (hv-corner w) :middle)
               (return-from set-qnw nil)))
            ;; follow QN->Q links if we haven't seen rightmost edge yet
            ((and (hv-n v2)
                  (not (hv-q v2))
                  (< y2 y1)
                  (not rightmost))
             (setf v2 (dll-next (hv-n v2))))
            ;; and Q->QW links
            ((and (hv-w v2)
                  (hv-q v2))
             (assert (< y2 y1))
             (setf v2 (dll-next (hv-w v2))))
            ((or (> y2 y1) (eql v2 v))
             (error "couldn't find edge right of Q node?")))))))
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
    (multiple-value-bind (edge corner)
        (classify-vertical-edge v)
      (when (eql corner :middle)
        (assert (member (hv-classify v) '(:qn :qw)))
        (assert (member edge '(:qn :qw :rightmost))))
      (setf (hv-classify v) (or edge (hv-classify v)))
      (setf (hv-corner v) corner)
      (when (and (eql edge :left-notch)
                 (eql corner :top))
        (set-qnw v))
      edge)))

(defclass point ()
  ((x :initform 0.0 :initarg :x :accessor x)
   (y :initform 0.0 :initarg :y :accessor y)
   (ref :initform nil :initarg :ref :accessor ref)))

(defun make-point (x y &key ref)
  (make-instance 'point :x x :y y :ref ref))

(defun point-p (x)
  (typep x 'point))

;; used to indicate a point in D that is above a gap or gaps caused by
;; skipping subholes. Depending on the height of the placement and Y
;; value of the support from C, we might be able to place something
;; against the edge below this point or might not
(defclass point-gap (point)
  ;; list of y,h for each gap on the vertical edge ending at this
  ;; point
  ((gaps :initform nil :accessor gaps :initarg :gaps)))

(defun make-point-gap (x y gaps)
  (make-instance 'point-gap :x x :y y :gaps gaps))

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
  ;; points. Gap stores amount skipped in corresponding edge by
  ;; Q->QN links if any
  ((d :accessor f-d :initarg :d)
   (h :accessor f-h :initarg :h)
   (gaps :accessor f-gaps :initarg :gaps :initform nil)))

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

   ;; link to corner if subhole has a "falling corner"
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
   ;; link to corner if hole has a "falling corner"
   (falling-corner-p :accessor h-falling-corner-p :initarg :falling
                     :initform nil)
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

(defun pxy (tx ty)
  (loop for x = (pop tx)
        for y = (pop ty)
        while (or x y)
        collect (list x y)))

(defun update-subhole (subhole)
  (let* ((start (dll-next (sh-start subhole)))
         (top start)
         (bottom (dll-prev start))
         (top-x nil)
         (top-y nil)
         (top-gaps nil)
         (bottom-x nil)
         (bottom-y nil)
         (ref nil)
         (end nil)
         (falling nil)
         (x-trim nil)
         (q nil))
    (assert (not (eql top bottom)))
    (assert (= (hv-x top) (hv-x bottom)))
    (assert (> (hv-y top) (hv-y bottom)))
    (with-minmax (x1 x2 mmx)
      (with-minmax (y1 y2 mmy)
        ;; we build bottom of subhole first, since a lower notch
        ;; could block the subhole completely, and we don't want
        ;; to include any falling corner in that case.
        (let ((hit-q nil)
              (prev-dy nil)
              (once nil)
              (has-rightmost nil))
          (flet ((add (x y v &optional force)
                   (unless (and (not force)
                                (eql y (car bottom-y)))
                     (push v ref)
                     (push x bottom-x)
                     (push y bottom-y))))
            (do-hole-vertices/prev (v bottom dx dy)
              ;; to build subhole bottom, we walk CCW from bottom
              ;; (following only prev) collecting vertical edges,
              ;; stopping at leftwards edge or first hv-w

              ;; once we see hv-q, we skip to hv-w and continue
              ;; (following hv-w) until we see a leftwards edge,
              ;; collecting only the points whose y value is > y value of
              ;; hv-n (can be any # >= 1)
              (when (and dy (not (zerop dy)))
                (setf prev-dy dy))
              (when (and (not hit-q)
                         (hv-q v))
                (setf q v)
                (setf hit-q v))
              (cond
                ;; first point
                ((and (not dy) (not hit-q))
                 (add (hv-x v) (hv-y (dll-next v)) v t)
                 (add (hv-x v) (hv-y v) v))
                ;; leftwards edge, just passed right edge or left notch,
                ;; depending on direction of previous vertical edge
                ((and dx (minusp dx))
                 (when (and (not hit-q) (not once))
                   (setf has-rightmost t)
                   (unless q
                     (setf q (dll-next v))
                     ;; we are at top of right edge, follow it to bottom corner
                     (loop while (= (hv-x q) (hv-x (dll-next q)))
                           do (setf q (dll-next q))))
                   (when (and prev-dy (minusp prev-dy))
                     (assert (hv-w q))))
                 ;; we go 1 extra edge in case there is a falling edge
                 (when once
                   (return nil))
                 (unless (= (hv-y (dll-prev v))
                            (hv-y v))
                   (setf once t)))
                ((member (hv-classify v)
                         '(:leftmost :rising-edge))
                 (return nil))
                ;; upwards vertical edges
                ((and dy
                      (plusp dy)
                      ;; ignore extra points in the middle of vertical edge
                      (not (and (hv-w v)
                                (eql (hv-x (dll-prev v))
                                     (hv-x v))))
                      ;; if we have seen a Q vertex for this subhole,
                      ;; only include edges that extend above the Q
                      ;; vertex, since ones below that can't support a
                      ;; placement in this subhole
                      (or (not hit-q)
                          (> (hv-y v) (hv-y q)))
                      ;; ignore any edges left of last added edge,
                      ;; which should be from some other subhole below
                      ;; this one
                      (> (hv-x v) (car bottom-x)))
                 (add (hv-x v) (hv-y v) v)
                 ;; if edge extends past the QN vertex for this subhole
                 ;; (if any), stop here and tell the top pass to stop
                 ;; at this X coordinate
                 (when (and hit-q
                            (>= (hv-y v) (hv-y (hv-n q))))
                   (assert (not x-trim))
                   (setf x-trim (hv-x v))
                   (return nil)))
                ;; downwards edges
                ((and dy
                      (minusp dy)
                      (not hit-q)
                      (not (hv-n v))
                      (not (hv-q v))
                      (not once))
                 (unless hit-q
                   (add (hv-x v) (hv-y v) v))))
              ;; follow q->qw
              (when (and (hv-w v) (hv-q v))
                (assert (dll-next (hv-w v)))
                (jump (hv-w v))))))
        (let ((hit-q nil))
          (flet ((add (x y &optional gap keep)
                   (unless (and (not keep)
                                (eql y (car top-y)))
                     (push x top-x)
                     (push y top-y)
                     (push gap top-gaps))))


            (do-hole-vertices/next (v top dx dy endp)
              ;; to build a subhole top, we walk clockwise from start
              ;; collecting vertical edges until we see either a line
              ;; moving left (= end of top), or a QN vertex on a
              ;; horizontal edge (which should be the N vertex for this
              ;; subhole's Q node, but can't check that directly since
              ;; it hasn't been linked yet)

              ;; While walking, we need to follow Q->QN links, grouping
              ;; any set of edges with same X coord into 1 edge with a
              ;; 'gap' for every Q->QN link on that edge,

              ;; once we see the QN, we continue until we see a
              ;; leftwards edge, only collecting edges that go below the
              ;; y value of the hv-q (should be 1 or 2 depending on
              ;; whether we have a falling edge in hole and where it is)
              (when (and dx (minusp dx))
                (unless end
                  (setf end (hv-x (dll-prev v))))
                (return nil))

              ;; if 'bottom' pass stopped early, trim top to same X coords
              (when (and x-trim
                         (>= (hv-x v) x-trim))
                ;; x-trim must be right of a Q->QN link, so if we
                ;; haven't seen that yet something is wrong
                (assert hit-q)
                (add x-trim (hv-y q) nil t)
                (return nil))
              ;; find an N vertex
              (when (and (not hit-q)
                         (hv-n v)
                         (not (hv-q v)))
                (setf end (hv-x v))
                (setf hit-q v))
              (cond
                ;; first point
                ((and (not dy) (not hit-q))
                 (push (hv-x v) top-x)
                 (push (hv-y (dll-prev v)) top-y)
                 (add (hv-x v) (hv-y v)))
                ;; upwards vertical edges
                ((and dy
                      ;; we get dx,dy set when jumping past gaps
                      (zerop dx)
                      (plusp dy))
                 (let ((gap nil)
                       ;; bottom of vertical edge
                       (v1 (dll-prev v)))
                   (loop while (and (hv-n v)
                                    (hv-q v))
                         for n = (hv-n v)
                         do (push (list (hv-y v)
                                        (- (hv-y (hv-n v))
                                           (hv-y v)))
                                  gap)
                            (setf v (hv-n v))
                         while (= (hv-x (dll-next v)) (hv-x v1))
                         do (setf v (dll-next v)))
                   (unless hit-q
                     (add (hv-x v) (hv-y v) (nreverse gap)))
                   (when gap
                     (jump (dll-next v)))))
                ;; downwards edges
                ((and dy
                      (minusp dy)
                      ;; skip QW vertices in middle of edge
                      (not (and (hv-w v)
                                (not (hv-q v))
                                (= (hv-x v)
                                   (hv-x (dll-next v))))))
                 (let ((up (extend-edge-up v #'dll-prev)))
                   (cond
                     ((or (not hit-q)
                          (and (<= (hv-y v)
                                   (hv-y hit-q))
                               (> (hv-y up)
                                  (hv-y (hv-n hit-q)))))
                      (when (and (or (not hit-q)
                                     (eql hit-q v)
                                     (< (hv-y v)
                                        (hv-y hit-q)))
                                 (>= (hv-x (dll-next v))
                                     (hv-x v)))
                        (assert (not falling))
                        (setf falling v))
                      (when (or (not hit-q)
                                (<= (hv-y v) (hv-y hit-q)))
                        (let ((rm (eql (hv-classify v) :rightmost)))
                          (add (hv-x v)
                               (if hit-q
                                   (if rm
                                       (min (hv-y (hv-n hit-q))
                                            (hv-y v))
                                       (max (hv-y (hv-n hit-q))
                                            (hv-y v)))
                                   (hv-y v))
                               nil rm))))
                     ;; if we see a falling edge after end, add 1 or 2
                     ;; more segments to close the top of subhole
                     ((and hit-q
                           (or (< (hv-y v)
                                  (hv-y (hv-n hit-q)))
                               (eql (hv-classify v) :rightmost)))
                      (when (> (hv-y (dll-prev v))
                               (hv-y (hv-n hit-q)))
                        (add (hv-x v)
                             (min (hv-y v)
                                  (hv-y (hv-n hit-q)))
                             nil)
                        (loop-finish)))))))
              ;; follow q->n (only see them here if we already stopped
              ;; collecting upwards edges)
              (when (and (hv-q v) (hv-n v))
                (assert (dll-next (hv-n v)))
                (jump (hv-n v))))))

        ;; once we have top/bottom, make sure the ends match properly
        ;; (may need to trim or extend ends depending on how previous
        ;; loops terminated, not sure yet)
        (when (or top-y bottom-y)
          (when (or (<= (length top-x) 2)
                    (<= (length top-y) 2))
            (error "short top? ~s~%" (pxy top-x top-y)))
          (when (or (<= (length bottom-x) 2)
                    (<= (length bottom-y) 2))
            (error "short bottom? ~s ~s~%"
                   bottom-x bottom-y))

          (when (/= (car top-x)
                    (car bottom-x))
            (let ((end (min (car top-x)
                            (car bottom-x))))
              (cond
                ((= end (car bottom-x))
                 (loop for (x next) = top-x
                       when (= x end)
                         return nil
                       when (< next end x)
                         do (setf (car top-x) end)
                            (loop-finish)
                       else
                         do (pop top-x)
                            (pop top-y)
                            (pop top-gaps)))
                ((= end (car top-x))
                 ;; trim bottom (not sure this can happen?)
                 ))))
          (assert (= (length top-x) (length top-y)))
          (assert (= (length bottom-x) (length bottom-y))))

        (flet ((make-dh (xx yy &optional making-c refs)
                 (loop for y0 = nil then y
                       for x0 = nil then x
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
          (if (and falling
                   (> (hv-y falling)
                      (hv-y q)))
              (setf (sh-falling-corner-p subhole) falling)
              (setf (sh-falling-corner-p subhole) nil))
          (setf (sh-top subhole) (apply
                                  #'make-instance
                                  'f-edge
                                  :gaps (coerce (nreverse top-gaps)
                                                'vector)
                                  (make-dh (nreverse top-x)
                                           (nreverse top-y))))
          (setf (sh-bottom subhole) (apply
                                     #'make-instance
                                     'f-edge
                                     (make-dh (nreverse bottom-x)
                                              (nreverse bottom-y)
                                              t
                                              (nreverse ref))))
          (setf (sh-end subhole) (hv-x q))
          (setf (sh-endv subhole) q)
          (assert (not (hv-sh q)))
          (setf (hv-sh q) subhole)
          (setf (sh-max-width subhole) (- x2 x1))
          (setf (sh-max-height subhole) (- y2 y1)))))))

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
    s))

(defun clean-subholes (hole)
  ;; to simplify things, sometimes we want to just rebuild subholes
  ;; from scratch, so remove any extra vertices in the middle of an
  ;; edge (old N,W verts, previous corner directly under edge of
  ;; placement, etc), and clean up stale subhole backlinks
  (flet ((r (v)
           (when (eql v (h-vertices hole))
             (setf (h-vertices hole) (dll-prev v)))
           (delete-node v)))
    (let ((c nil))
      (do-dll/next (n (h-vertices hole))
        (setf (hv-sh n) nil)
        (cond
          ((and (= (hv-x (dll-prev n))
                   (hv-x n))
                (= (hv-y (dll-prev n))
                   (hv-y n)))
           ;; repeated point, probably from touching corners,
           ;; was removed by other test so keep it this time
           (error "shouldn't happen"))
          ((and (or (not (hv-n n))
                    (not (hv-w n)))
                (or (= (hv-x (dll-prev n))
                       (hv-x n)
                       (hv-x (dll-next n)))
                    (= (hv-y (dll-prev n))
                       (hv-y n)
                       (hv-y (dll-next n)))))
           (push n c))))
      ;; remove separately so we don't confuse iteration
      (map nil #'r c))))

(defun make-subholes (hole &key clean)
  (when clean
    (clean-subholes hole))

  (with-minmax (x1 x2 mmx)
    (with-minmax (y1 y2 mmy)
      (let ((edges (make-hash-table)))
        ;; find edges and bounds, build Q links, classify vertices
        (do-dll/next (n (h-vertices hole))
          (mmx (hv-x n))
          (mmy (hv-y n))
          ;; run for side effects
          (classify-vertex n)
          (when (and (eql (hv-classify n) :leftmost)
                     (eql (hv-corner n) :top))
            (setf (gethash n edges) n)))

        (setf (ht-max-width hole) (- x2 x1))
        (setf (ht-max-height hole) (- y2 y1))

        (when (and clean (h-falling-corner-p hole))
          (setf (hv-hole (h-falling-corner-p hole)) nil)
          (setf (h-falling-corner-p hole) nil))

        ;; we build subholes in 2nd pass since we need to wait until
        ;; classify-vertex builds the QN/QW links for all nodes
        (loop for e being the hash-keys of edges
              for h = (make-subhole hole e nil)
                then (make-subhole hole e h)
              for f = (sh-falling-corner-p h)
              when f
                do (if (h-falling-corner-p hole)
                       (assert (eql f (h-falling-corner-p hole)))
                       (progn
                         (when (hv-hole f)
                           (if clean
                               (setf (h-falling-corner-p (hv-hole f))
                                     nil)
                               (assert (eql hole (hv-hole f)))))
                         (setf (h-falling-corner-p hole)
                               (sh-falling-corner-p h)
                               (hv-hole f) hole)))
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
  (fff " check subhole ~s: @ ~s~%" sh (p (sh-start sh)))
  (fff "    fc = ~s ~a~%" (p (sh-falling-corner-p sh))
    (when (sh-falling-corner-p sh)
      (list (p (dll-prev (sh-falling-corner-p sh)))
            (p (dll-next (sh-falling-corner-p sh))))))
  (fff "    top = :~%")
  (loop for d across (f-d (sh-top sh))
        for h across (f-h (sh-top sh))
        for g across (f-gaps (sh-top sh))
        do (fff "     ~s -> ~s @ ~s~%"
             (pp d) (pp h) g))
  (fff "    bottom = :~%")
  (loop for d across (f-d (sh-bottom sh))
        for h across (f-h (sh-bottom sh))
        do (fff "     ~s -> ~s~%"
             (pp d) (pp h)))

  ;;(assert (equalp '(:leftmost :bottom) (hv-classify (sh-start sh))))
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
               (length (f-gaps (sh-top sh)))))
    (assert (= (length (f-d (sh-bottom sh)))
               (length (f-h (sh-bottom sh)))))

    ;; validate top/bottom, calculate bounds
    (let ((top (sh-top sh)))
      (loop with px = most-negative-fixnum
            with py = (y (aref (f-d top) 0))
            with l = (length (f-d top))
            for d across (f-d top)
            for h across (f-h top)
            for g across (f-gaps top)
            for i from 0
            ;; all segments are vertical
            do (assert (= (x d) (x h)))
               ;; f-d is always lower y value than f-h
               (assert (> (y h) (y d)))
               ;; all edges line up with previous, and go up except
               ;; last 1 or 2 which go down (if we have more than 1)
               (when (> l 1)
                 (assert (if (or
                              ;; last 2 edges of falling-corner-p hole go down
                              (and (sh-falling-corner-p sh)
                                   (>= i (- l 2)))
                              ;; otherwise only last edge goes down
                              (>= i (1- l)))
                             (= (y h) (shiftf py (y d)))
                             (= (y d) (shiftf py (y h))))))
               ;; all edges are right of previous edges
               (assert (> (x d) px))
               (setf px (x d))
               (when g
                 (loop for ((y gh) (y2)) on g
                       do ;; gaps can at most reach top of edge
                          (assert (<= (+ y gh) (y h)))
                          ;; and must start above bottom
                          (assert (> y (y d)))
                       while y2
                       do ;; gaps must be in increasing y order
                          (assert (< y y2))
                          ;; gaps can't touch
                          (assert (< (+ y gh) y2))))
               ;; update bounds
               (setf x1 (min x1 (x d) (x h)))
               (setf x2 (max x2 (x d) (x h)))
               (setf y1 (min y1 (y d) (y h)))
               (setf y2 (max y2 (y d) (y h)))))
    (let ((bottom (sh-bottom sh)))
      ;; gap should never be set
      (every 'null (f-gaps bottom))
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
    (assert (= (sh-max-height sh) (- y2 y1)))))

(defun check-hole (h)
  (let ((points (make-hash-table :test 'equalp))
        (subholes1 (make-hash-table))
        (subholes2 (make-hash-table)))
    (do-dll/next (v (h-vertices h))
      ;; valid linked list
      (assert (eql (dll-prev (dll-next v)) v))
      (assert (eql (dll-next (dll-prev v)) v))
      ;; no duplicated points
      (let ((xy (list (hv-x v) (hv-y v))))
        7(assert (not (gethash xy points)))
        (setf (gethash xy points) v))
      ;; vertices should be classified ;; todo: verify correct
      (assert (and (hv-classify v) (hv-corner v)))
      (assert (not (eql (hv-corner v) :right-notch)))
      ;; check Q vertices
      (when (hv-q v)
        ;; top of left notch
        (assert (eql (hv-classify v) :left-notch))
        (assert (eql (hv-corner v) :top))
        ;; link to both N and W
        (assert (hv-n v))
        (assert (hv-w v))
        ;; link back to subhole
        (assert (hv-sh v))
        ;; subhole is in hole
        (assert (eql (sh-hole (hv-sh v)) h)))
      ;; QN,QW links are bidirectional, and are to or from a Q node
      (when (hv-n v)
        (assert (or (hv-q v) (hv-q (hv-n v))))
        (assert (eql (hv-n (hv-n v)) v)))
      (when (hv-w v)
        (assert (or (hv-q v) (hv-q (hv-w v))))
        (assert (eql (hv-w (hv-w v)) v)))
      (when (hv-sh v)
        ;; must be either start (bottom of leftmost edge) or end (Q or
        ;; bottom of rightmost edge)
        (cond
          ((eql (hv-classify v) :leftmost)
           (assert (eql (hv-corner v) :bottom)))
          ((eql (hv-classify v) :rightmost)
           (assert (eql (hv-corner v) :bottom)))
          ;; might also be bottom of a falling edge, if entire subhole is
          ;; blocked by falling edge
          ((eql (hv-classify v) :falling-edge)
           (assert (eql (hv-corner v) :bottom)))
          ((hv-q v)
           (assert (eql (hv-classify v) :left-notch))
           (assert (eql (hv-corner v) :top)))
          (t
           (error "got hv-sh link on ~s ~s point?"
                  (hv-classify v) (hv-corner v))))
        ;; count vertices linking to each subhole
        (incf (gethash (hv-sh v) subholes1 0) 1)))
    (assert (h-subholes h))
    (do-dll/next (sh (h-subholes h))
      (assert (eql (dll-prev (dll-next sh)) sh))
      (assert (eql (dll-next (dll-prev sh)) sh))
      (assert (eql (sh-hole sh) h))
      ;; not looping incorrectly
      (assert (not (gethash sh subholes2)))
      (setf (gethash sh subholes2) t)
      (check-subhole sh))
    (loop for k being the hash-keys of subholes1 using (hash-value v)
          ;; exactly 2 vertices link to each subhole (start,end)
          do (assert (= v 2))
             ;; and they link to subhole in this hole
             (assert (gethash k subholes2)))
    (loop for k being the hash-keys of subholes2
          do ;; every subhole in hole was subhole for some vertices
             (assert (gethash k subholes1)))))

(defun -x (p d)
  (make-point (- (x p) d) (y p) :ref (ref p)))
(defun +x (p d)
  (make-point (+ (x p) d) (y p) :ref (ref p)))
(defun -y (p d)
  (make-point (x p) (- (y p) d) :ref (ref p)))
(defun +y (p d)
  (make-point (x p) (+ (y p) d) :ref (ref p)))

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
                     for i from (- end 1)
                     downto start
                     for p0 = (if (point-bottom-left-p (d i))
                                  (d i)
                                  (h i))
                     when (or (deq-empty-p q)
                              (<y (first (top1 q))
                                  p0))
                       do (let* ((p1 (if (and (= (1+ i) end)
                                              (not (point-bottom-left-p
                                                    (d end))))
                                         (d (1+ i))
                                         (h (1+ i))))
                                 (p0 (make-point (x p0)
                                                 (y p0)
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
                           while (<=y ql sl)
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
                          (loop
                            ;; hit a new support (rising edge
                            ;; strictly between support and support+l)
                            when (and (< (x support)
                                         (x (h i))
                                         (+ (x support) l))
                                      ;; at same height
                                      (= (y (h i)) (y support))
                                      ;; with a falling edge after it
                                      (< (1+ i) m)
                                      (= (y (h i)) (y (h (1+ i))))
                                      (point-bottom-left-p (d (1+ i))))
                              do (setf support (h (1+ i)))
                                 (setf start i)
                                 ;; ready to fall
                            when (and
                                  (< (y (d i))
                                     (y support))
                                  (>= (x (h i))
                                      (+ (x support) l)))
                              do (setf hit :drop)
                              and return nil

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
                             (cond
                               ((< (x (car c))
                                   (- (x (h (1- m))) l))
                                (let* ((i (1- m))
                                       (u (make-point (- (x (h i)) l)
                                                      (y b1))))
                                  (push u c)))
                               (t
                                (pop c)))
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
                                 (cond
                                   ((and (= (y lk) (y b1))
                                         (< (x lk)
                                            (+ (x b1) l)))
                                    ;; spanned gap, just continue
                                    ;; previous placement
                                    (setf support rk)
                                    (setf start i)
                                    (pop c))
                                   (t
                                    (let ((dy (- (y b1) (y lk))))
                                      (setf b1 (-y b1 dy)
                                            b2 (-y b2 dy))
                                      (push (make-point-bottom-left
                                             (x b1) (y b1) :ref (ref b1))
                                            c)
                                      (setf start i)
                                      (setf support rk))))))))))
               (trim ()
                 (assert (evenp (length c)))
                 (loop for (p1 p2) on c by #'cddr
                       when (> (x p2) end)
                         do (setf (x p2) end)
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
                         and collect (if (= (x p2) end)
                                         ;; if point is at end of subhole,
                                         ;; it can't support a placement
                                         (change-class p2 'point-open)
                                         p2))))
        (slide 1)
        (setf c (nreverse c))
        (let ((c (coerce (trim) 'vector)))
          c)))))

(defun make-d (ft l end falling)
  (declare (ignorable end falling))
  ;; generate D given FT and width L of t1t2 (procedure TOP)
  (let ((p (1- (length (f-d ft)))))
    (flet ((h (x)
             (aref (f-h ft) x))
           (d (x)
             (aref (f-d ft) x))
           (g (x)
             (aref (f-gaps ft) x)))
      (assert (= (length (f-d ft)) (length (f-h ft))))
      ;; doesn't fit at all
      (when (> (+ (x (d 0)) l)
               (x (d p)))
        (return-from make-d #()))

      ;; no falling edge, just return the horizontal edges of FT
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
                                                        (g i))))))
            (coerce a 'vector))))

      ;; falling edge, walk FT and add places where L fits
      (let* ((d nil)
             (p0 0)
             (x0 (x (h 0)))
             (y0 (y (h 0)))
             (p-1 (1- p))
             (dp-1 (d p-1))
             (xp-1 (x (d p-1)))
             (yp-1 (y (d p-1)))
             (dp (d p))
             (xp (x dp))
             (fe-left (when (and (< (y (d 0)) yp-1 (y (h 0))))
                        0)))
        (flet ((make-point* (n &key open (y (y (h n))) xmax)
                 (cond
                   ((and open fe-left (<= (- xp-1 l)
                                          (x (h fe-left))))
                    (if (g fe-left)
                        ;; todo: trim gaps to height of falling edge?
                        (make-point-gap (x (h fe-left)) y (g fe-left))
                        (make-point (x (h fe-left)) y)))
                   ((and open)
                    (make-point-open (- xp-1 l) y))
                   ((g n)
                    (make-point-gap (if xmax
                                        (min xmax (x (h n)))
                                        (x (h n)))
                                    y (g n)))
                   (t
                    (make-point (if xmax
                                    (min xmax (x (h n)))
                                    (x (h n)))
                                y)))))
          ;; L exactly fits in width of subhole, add 0-length
          ;; placement under lower of first edge and falling edge
          (cond
            ((= (+ (x (d 0)) l)
                (x dp))
             (let ((y (min (y (h 0)) yp-1)))
               (setf d (vector (make-point* 0 :y y)
                               (make-point* p :y y)))))
            ;; handle case where there are no rising edges so the loop
            ;; doesn't need to care about it
            ((= p-1 1)
             (cond
               ;; fits before falling edge, add 2 spans
               ((<= (+ x0 l) xp-1)
                (setf d (vector (make-point* 0)
                                (make-point (- xp-1 l) y0)
                                (make-point* xp-1 :y yp-1 :open t)
                                (make-point-open (- (x (h p)) 0) yp-1))))
               ;; spans gap, add 1 span
               ((and (> (+ x0 l) xp-1)
                     (= y0 yp-1))
                (setf d (vector (make-point* 0)
                                (make-point (- (x (h p)) 0) yp-1))))
               ;; doesn't fit, add 1 span under falling edge
               (t
                (setf d (vector (make-point* 0 :y yp-1)
                                (make-point (- (x (h p)) 0) yp-1))))))
            ;; general case, loop over rising edges until we reach
            ;; falling edge
            (t
             (loop for n from 1 below p
                   for gap = (g n)
                   do (when (and (< (y (d n)) yp-1 (y (h n))))
                        (assert (not fe-left))
                        (setf fe-left n))
                      (cond
                        ;; l fits exactly between this edge and
                        ;; falling edge, or it fits and next edge is
                        ;; falling edge. add span ending at this
                        ;; point, span at next point, and span below
                        ;; falling edge, and return
                        ((or (and (= (+ (x (h n)) l) xp-1)
                                  (> (y (h n)) yp-1))
                             (and (= (1+ n) p-1)
                                  (< (+ (x (h n)) l) xp-1)))
                         ;; span ending here
                         (push (make-point* p0) d)
                         (push (d n) d)
                         ;; (possibly 0-length) span between here and
                         ;; falling edge
                         (push (make-point* n) d)
                         (push (make-point (- xp-1 l) (y (h n)))
                               d)
                         ;; span below falling edge
                         (push (make-point* n :y yp-1 :open t)
                               d)
                         ;; (ends at dp since bottom edge will limit it)
                         (push (make-point* p :y yp-1) d)
                         (loop-finish))
                        ;; l fits between end of this edge and falling
                        ;; edge with space left over. add span ending
                        ;; at current point, and continue at next
                        ;; point
                        ((< (+ (x (h n)) l) xp-1)
                         (push (make-point* p0) d)
                         (push (d n) d)
                         (setf p0 n))
                        ;; L fits under falling edge, and falling edge
                        ;; is higher than next span. add span ending
                        ;; at current point and continue at next point
                        ((and (>= (+ (x (h n)) l) xp-1)
                              (<= (+ (x (h n)) l) xp)
                              (< (y (d n)) (y dp-1))
                              (<= (y (h n)) (y dp-1))
                              (/= (1+ n) p-1))
                         (push (make-point* p0) d)
                         (push (d n) d)
                         (setf p0 n))
                        ;; l spans gap between this edge and falling edge
                        ;; with same value, add span that extends to
                        ;; falling edge and return
                        ((and (> (+ (x (h n)) l) xp-1)
                              (= (y (d n)) (y dp-1)))
                         ;; span ending here
                         (push (make-point* p0) d)
                         ;; (ends at dp since bottom edge will limit it)
                         (push (make-point* p :y yp-1) d)
                         (loop-finish))
                        ;; l extends below falling edge, add span ending
                        ;; at this point, and span below falling edge if
                        ;; it fits
                        ((and (> (+ (x (h n)) l) xp-1)
                              (< (y (d n)) (y dp-1)))
                         ;; span ending here
                         (assert (<= (+ (x (h p0)) l) (x dp)))
                         (push (make-point* p0) d)
                         (if (= (+ (x (h p0)) l)
                                (x (h p)))
                             (push (make-point* p :y (y (d n)))
                                   d)
                             (push (make-point* n :y (y (d n))
                                                  :xmax (- (x dp) l))
                                   d))
                         (when (<= (+ (x (d n)) l) (x (d p)))
                           ;; span below falling edge
                           (push (make-point* n :y yp-1 :open t)
                                 d)
                           ;; (ends at dp since bottom edge will limit it)
                           (push (make-point* p :y yp-1) d))
                         (loop-finish))
                        ;; l hits left side of falling edge, add span
                        ;; ending at (- falling-edge l) if it fits, and
                        ;; add span under falling edge and return
                        ((and (> (+ (x (h n)) l) xp-1)
                              (> (y (d n))
                                 (y dp-1)))
                         ;; span that would have ended here
                         (when (<= (+ (x (d p0)) l) xp-1)
                           (push (make-point* p0) d)
                           (push (make-point (- xp-1 l) (y (h p0))) d))
                         ;; span below falling edge
                         (push (make-point* n :y yp-1 :open t)
                               d)
                         ;; (ends at dp since bottom edge will limit it)
                         (push (make-point* p :y yp-1) d)
                         (loop-finish)))))))
        (if (listp d)
            (let ((v (coerce (nreverse d) 'vector)))
              (unless (zerop (mod (length v) 2))
                (break "~s?~s" (length v) v))
              v)
            (progn
              d))))))


;; this might hold large data live, so change-class to rect before
;; passing back to user?
(defclass placement (rect)
  ;; internal stuff to speed up incremental updates, etc
  ((hole-point :reader hole-point :initarg :point)
   (hole :Reader hole :initarg :hole)
   (quick-contact :accessor quick-contact :initform nil)
   (full-contact :accessor full-contact :initform nil)
   (penalty-multiplier :accessor penalty-multiplier :initform 1)
   (overlap :accessor overlap :initform nil)))

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
               (and (>= (- (y (ll j)) (y (l i)))
                        h)))
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
                           (not (gaps (ll j))))
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
                       (gaps (ll j)))
                  (let* ((gap (gaps (ll j)))
                         (y1 (y (l i)))
                         (y2 (+ y1 h))
                         (ph (- y2 y1)))
                    (if (<= (+ (y (l i)) h)
                            (y (rr (max 0 (1- j)))))
                        ;; fits entirely below edge
                        t
                        ;; check gaps
                        (loop for (y h) in gap
                              ;; if placement fits in gap, we can't place
                              ;; here
                              when (and (<= ph h)
                                        (<= y y1 y2 (+ y h)))
                                return t
                              ;; otherwise, if gap overlaps placement, top
                              ;; or bottom is supported, so we can place
                              ;; here
                              when (< y1 y y2)
                                return nil
                              ;; otherwise, check remaining gaps, and if
                              ;; none returned T, we can place here
                              finally (return nil)))))
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
               (unless (dll-next (ref-point i))
                 (break "?~s" (ref-point i)))
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
           (loop
             while (and (< j p)
                        (<= i m)
                        (< (x (rr j))
                           (x (l i))))
             do (incf j))
           (loop
             while (and (< i m)
                        (<= j p)
                        (< (x (r i))
                           (x (ll j))))
             do (incf i))
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

(defun valid-placement-p* (i)
  (valid-placement-p (hole i) (w i) (h i) (x i) (y i)))

(defun find-all-placements/sh (subhole w h hole)
  (let ((r nil))
    (do-dll/next (n subhole)
      (when (and (<= w (sh-max-width n))
                 (<= h (sh-max-height n)))
        (let ((d (make-d (sh-top n) w (sh-end n) (sh-falling-corner-p n))))
          (when (plusp (length d))
            (let* ((c (make-c (sh-bottom n) w (sh-end n)))
                   (p (placing w h c d hole)))
              (setf r (append p r)))))))
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
   (classify :initarg :classify :accessor classify)))

;; some methods so we don't need to check types as often
(defmethod a ((p placement-span)) nil)
(defmethod b ((p placement-span)) nil)
(defmethod start ((p placement-span)) nil)
(defmethod end ((p placement-span)) nil)
(defmethod overlap-length ((p placement-span)) 0)

;; just for checking types more easily
(defclass gap-span (placement-span)
  ())

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
(defun pp (v)
  (when v
    (etypecase v
      (point-gap
       (list (x v) (y v) :g (gaps v)))
      (point-open
       (list (x v) (y v) :o))
      (point-bottom-left
       (list (x v) (y v) :b))
      (t
       (if (eql (type-of v) 'point)
           (list (x v) (y v))
           (list (x v) (y v) (type-of v)))))))

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
              ;; edge ends somewhere in span, and is co-linear
              ((or (and (= x1 x2 (hv-x v1) (hv-x v2))
                        (<= ymin (hv-y v2) ymax))
                   (and (= y1 y2 (hv-y v1) (hv-y v2))
                        (<= xmin (hv-x v2) xmax)))
               :end)
              ;; edge extends past span on both ends, and is co-linear
              ((let ((xmin (min (hv-x v1) (hv-x v2)))
                     (xmax (max (hv-x v1) (hv-x v2)))
                     (ymin (min (hv-y v1) (hv-y v2)))
                     (ymax (max (hv-y v1) (hv-y v2))))
                 (or (and (= x1 x2 (hv-x v1) (hv-x v2))
                          (or (<= ymin y1 ymax)
                              (<= ymin y2 ymax)))
                     (and (= y1 y2 (hv-y v1) (hv-y v2))
                          (or (<= xmin x1 xmax)
                              (<= xmin x2 xmax)))))
               :end)
              ;; doesn't intersect, or only intersects at start/end point
              (t
               nil))))
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
    (labels ((d (y y1 y2)
               (/ (- y y1)
                  (- y2 y1)))
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
                         ;; one starts at end of other
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
                                x1 y1 x4 y4))
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
                                x1 y1 x4 y4))
                       ;; edge starts in middle of span, ends after end
                       ((and mid-start overlap-end)
                        (values :after
                                (d y1 y3 y4)
                                (d y2 y3 y4)
                                x3 y3 x2 y2))
                       ;; edge starts and ends within span
                       ((and mid-start mid-end)
                        (values :middle 0 1 x3 y3 x4 y4))
                       (t
                        ;; disjoint segments, or only start/end intersects
                        nil)))))))
      (cond
        ((and (zerop den) (= x1 x2 x3 x4))
         ;;lines are parallel and vertical
         (parallel x1 y1 x2 y2 x3 y3 x4 y4))
        ((and (zerop den) (= y1 y2 y3 y4))
         ;;lines are parallel and horizontal (reuse code from vertical,
         ;;with axes swapped
         (multiple-value-bind (type d1 d2 x1 y1 x2 y2)
             (parallel y1 x1 y2 x2 y3 x3 y4 x4)
           (values type d1 d2 y1 x1 y2 x2)))
        ((zerop den)
         ;; lines are parallel and not on same line
         nil)
        ((or (= x1 x2) (= y1 y2))
         ;; perpendicular lines, at most endpoints intersect
         nil)
        (t
         (break "?"))))))

(defun intersect-hole-with-quad (placement)
  ;; calculates intersection of placement with hole used to create
  ;; placement, returned as a dll of placement and overlap spans, and
  ;; total length of contact
  (let* ((len 0)
         (spans nil)
         (lastx nil)
         (lasty nil)
         (last-edge nil)
         (first-edge nil)
         (px1 (x placement))
         (py1 (y placement))
         (px2 (+ px1 (w placement)))
         (py2 (+ py1 (h placement)))
         (v1 (hole-point placement))
         (hole (hole placement))
         (passed-corner nil))
    (labels ((add-span (span)
               ;; must be horizontal or vertical or 0-length
               (assert (or (= (x1 span) (x2 span))
                           (= (y1 span) (y2 span))))
               (insert-after spans span)
               (setf spans span))
             (add-gap (x y)
               (add-span (make-instance
                          'gap-span
                          :classify :gap
                          :x1 lastx :y1 lasty
                          :x2 x :y2 y)))
             (add-overlap (v type d1 d2 x1 y1 x2 y2)
               ;; add overlap of edge from (dll-prev v) to v
               (let ((from (dll-prev v))
                     (l (sqrt (+ (expt (- x2 x1) 2)
                                 (expt (- y2 y1) 2)))))
                 (incf len l)
                 (cond
                   ;; skipped some space, add a gap
                   ((and lastx
                         (or (not (= lastx x1))
                             (not (= lasty y1))))
                    (add-gap x1 y1))
                   ;; normal span immediately after a corner, remove
                   ;; the corner (probably a N/W node, and we only
                   ;; care about corners if that is the only contact
                   ;; point)
                   ((and spans
                         (eql (classify spans) :corner)
                         (= x1 (x1 spans))
                         (= y1 (y1 spans)))
                    (delete-node (shiftf spans (dll-prev spans)))
                    (unless (dll-next spans)
                      (setf spans nil))))
                 (add-span (make-instance
                            'overlap-span
                            :classify type
                            :x1 x1 :y1 y1
                            :x2 x2 :y2 y2
                            :start d1 :end d2
                            :a from :b v))
                 (setf lastx x2)
                 (setf lasty y2)))
             (add-corner (x y)
               (when (and lastx
                          (or (not (= lastx x))
                              (not (= lasty y)))
                          (not
                           (and (= x (x1 (dll-next spans)))
                                (= y (y1 (dll-next spans))))))
                 (add-gap x y)
                 (setf lastx x
                       lasty y)))
             (add-corners (edge)
               (when (and last-edge (not (= edge last-edge)))
                 (assert (<= 0 edge 3))
                 (loop with x = (vector px2 px1 px1 px2)
                       with y = (vector py1 py1 py2 py2)
                       for i = (mod (1+ last-edge) 4) then (mod (1+ i) 4)
                       do (add-corner (aref x i) (aref y i))
                       while (/= i edge)))
               (unless first-edge
                 (setf first-edge edge))
               (setf last-edge edge))
             (d (y y1 y2)
               (/ (- y y1)
                  (- y2 y1)))
             (contact (x1 y1 x2 y2 x3 y3 x4 y4)
               ;; xy1,xy2 are 'span' of placement, xy3,xy4 are 'edge'
               ;; from hole

               ;; written for vertical lines (x1=x2=x3=x4), called with
               ;; x,y swapped for horizontal
               (let ((symin (min y1 y2))
                     (symax (max y1 y2))
                     (vymin (min y3 y4))
                     (vymax (max y3 y4)))
                 (let ((start (= y1 y3)) ;; both start at same point
                       (end (= y2 y4))   ;; both end at same point
                       ;; one starts at end of other
                       (to-start (= y1 y4))
                       ;;(from-end (= y2 y3))
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
                              x1 y1 x4 y4))
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
                              x1 y1 x4 y4))
                     ;; edge starts in middle of span, ends after end
                     ((and mid-start overlap-end)
                      (values :after
                              (d y1 y3 y4)
                              (d y2 y3 y4)
                              x3 y3 x2 y2))
                     ;; edge starts and ends within span
                     ((and mid-start mid-end)
                      (values :middle 0 1 x3 y3 x4 y4))
                     (to-start
                      (values :corner 1 1 x1 y1 x1 y1))
                     (t
                      ;; disjoint segments, or only start/end intersects
                      nil))))))

      ;; search counterclockwise for a point right of placement
      (do-dll/prev (v v1)
        ;; either we went past it to the right, or followed rightmost
        ;; edge past it vertically
        (when (or (> (hv-x v) px2)
                  (and (= (hv-x v) px2)
                       (>= (hv-y v) py2)))
          (setf v1 v)
          (return nil))
        ;;todo: follow n->Q and Q->W links where possible
        )

      ;; search for intersections clockwise from there
      (do-hole-vertices/next (v v1 dx dy endp)
        (when (and dx dy) ;; ignore first point
          (let* ((prev (dll-prev v))
                 (x1 (hv-x prev))
                 (y1 (hv-y prev))
                 (x2 (hv-x v))
                 (y2 (hv-y v)))
            (when (and (<= x1 px1) (<= y1 py1))
              (setf passed-corner t))
            (cond
              ;; on same line as bottom edge
              ((= y1 y2 py1)
               (multiple-value-bind (type d1 d2 cy1 cx1 cy2 cx2)
                   (contact  py1 px2 py1 px1 y1 x1 y2 x2)
                 (when type
                   (add-corners 0)
                   (add-overlap v type d1 d2 cx1 cy1 cx2 cy2))))
              ;; on same line as left edge
              ((= x1 x2 px1)
               (multiple-value-bind (type d1 d2 cx1 cy1 cx2 cy2)
                   (contact px1 py1 px1 py2 x1 y1 x2 y2)
                 (when type
                   (add-corners 1)
                   (add-overlap v type d1 d2 cx1 cy1 cx2 cy2))))
              ;; on same line as top edge
              ((= y1 y2 py2)
               (multiple-value-bind (type d1 d2 cy1 cx1 cy2 cx2)
                   (contact py2 px1 py2 px2 y1 x1 y2 x2)
                 (when type
                   (add-corners 2)
                   (add-overlap v type d1 d2 cx1 cy1 cx2 cy2))))
              ;; on same line as right edge
              ((= x1 x2 px2)
               (multiple-value-bind (type d1 d2 cx1 cy1 cx2 cy2)
                   (contact px2 py2 px2 py1 x1 y1 x2 y2)
                 (when type
                   (add-corners 3)
                   (add-overlap v type d1 d2 cx1 cy1 cx2 cy2))))
              ((and (plusp dy) (> y2 py2) passed-corner)
               ;; once we go above top of box, only falling edge can drop
               ;; down to touch it, so check that then stop
               (let ((fc (h-falling-corner-p hole)))
                 (when fc
                   (assert (dll-next fc))

                   (let* ((p (extend-edge-up fc 'dll-prev))
                          (n (extend-edge-right fc 'dll-next)))
                     ;; check for bottom edge of falling corner
                     ;; touching top of placement
                     (loop
                       with v = fc
                       do (let ((x2 (hv-x v))
                                (y2 (hv-y v))
                                (x3 (hv-x (dll-next v)))
                                (y3 (hv-y (dll-next v))))
                            (when (= y2 y3 py2)
                              (multiple-value-bind (type d1 d2 cy1 cx1 cy2 cx2)
                                  (contact py2 px1
                                           py2 px2
                                           y2 x2
                                           y3 x3)
                                (when type
                                  (add-corners 2)
                                  (add-overlap (dll-next v)
                                               type d1 d2 cx1 cy1 cx2 cy2))))
                            (setf v (dll-next v)))
                       until (eql v n))
                     ;; check for left edge of falling corner touching

                     ;; right side of placement, or corner contact
                     (loop
                       with once = nil
                       do
                          (let ((x1 (hv-x p))
                                (y1 (hv-y p))
                                (x2 (hv-x (dll-next p)))
                                (y2 (hv-y (dll-next p))))
                            (when (= x1 x2 px2)
                              (multiple-value-bind (type d1 d2 cx1 cy1 cx2 cy2)
                                  (contact px2 py2 px2 py1 x1 y1 x2 y2)
                                (when type
                                  (unless once
                                    (add-corners 3)
                                    (setf once t))
                                  (add-overlap (dll-next p)
                                               type d1 d2 cx1 cy1 cx2 cy2)))))
                          (setf p (dll-next p))
                       until (eql p fc)))))
               (return nil))
              ((and (plusp dx) (> x2 px2) passed-corner)
               ;; if we pass right edge of box, nothing to right can touch
               ;; placement. If rightmost edge touched placement, we
               ;; should have started from the top vertex of it, and will
               ;; need to check for that separately
               (return nil))
              ;; todo: skip Q->N, W->Q when possible
              )
            (when endp
              ;; check final span, unless it is first node
              (let ((x1 (hv-x v))
                    (y1 (hv-y v))
                    (x2 (hv-x (dll-next v)))
                    (y2 (hv-y (dll-next v))))
                (when (= y1 y2 py2)
                  (multiple-value-bind (type d1 d2 cy1 cx1 cy2 cx2)
                      (contact py2 px1 py2 px2 y1 x1 y2 x2)
                    (when type
                      (add-overlap (dll-next v) type d1 d2 cx1 cy1 cx2 cy2)
                      (setf last-edge first-edge))))
                (return nil))))))
      (assert first-edge)
      ;; add missing corners
      (when (/= first-edge last-edge)
        (add-corners (mod first-edge 4)))
      ;; and make sure shape is closed
      (let ((n (dll-next spans)))
        (when (or (/= (x2 spans) (x1 n))
                  (/= (y2 spans) (y1 n)))
          (add-gap (x1 n) (y1 n)))))

    (values spans len)))

(defun remove-quad-from-hole (hole placement
                              &key (overlap
                                    (or (overlap placement)
                                        (intersect-hole-with-quad placement))))
  (assert (typep overlap 'placement-span))
  (setf hole (hole placement))

  ;; to remove a quad, we walk around the overlap list, collecting a
  ;; pair of start/end points for each gap (and intermediate points),
  ;; creating new points on the hole as needed for start or end
  ;; points. Any points between 2 non-gap contacts (not including
  ;; to/from) are removed (where a corner of placement is in a corner
  ;; of hole, may be up to 4 in a row corresponding to all 4 corners
  ;; of placement, in which case either there is a single span of gap
  ;; in the middle of remaining side, or the hole was removed
  ;; completely)

  ;; once we have those sets, if we only have 1 gap, it is connected
  ;; by a single (possibly angled) segment due to removal of inner
  ;; contact points. walk backwards along list of intermediate points
  ;; adding them between the start and end points.

  ;; if we have multiple gaps, we swap start->prev of first gap with
  ;; start->prev of next gap, repeating for subsequent gaps


  ;; check for removing entire hole
  (when (loop repeat 5 ;; 5 to make sure we notice any corner gaps
              for i = overlap then (dll-next i)
              always (eql :exact (classify i)))
    (let ((r (dll-next hole)))
      (delete-node hole)
      (return-from remove-quad-from-hole
        (dll-next r))))

  ;; first find the start of a gap
  (let ((start nil))
    (if (typep overlap 'overlap-span)
        (do-dll/next (v overlap)
          (when (and (typep v 'gap-span) (not start))
            (setf start v)
            (return nil)))
        (do-dll/prev (v (dll-prev overlap))
          (when (typep v 'overlap-span)
            (setf start (dll-next v))
            (return nil))))
    (assert start)
    (setf overlap start))
  ;; and back up 1 node so we can make sure the first point of the gap
  ;; exists
  (setf overlap (dll-prev overlap))

  (let (;; subholes whose start or endv nodes were removed, or
        ;; affected by a new edge
        (affected-subholes (make-hash-table))
        ;; q nodes that were removed
        (removed-q (make-hash-table))
        ;; list of gaps, with any intermediate points that need to be
        ;; added
        (gaps nil)
        ;; list of any new holes created
        (new-holes nil)
        ;; vertices created or reconnected by placement, so we can
        ;; update classify data
        (classify-vertices (make-hash-table)))
    (labels ((remove-node (n)
               ;; if vertex was falling corner of hole, remove it
               (when (eql n (h-falling-corner-p hole))
                 (setf (h-falling-corner-p hole) nil))
               (when (gethash n classify-vertices)
                 (remhash n classify-vertices))
               ;; if vertex was start or end of a subhole, we need
               ;; to update it
               (let ((sh (hv-sh n)))
                 (when (hv-q n)
                   (setf (hv-n (hv-n n)) nil)
                   (setf (hv-w (hv-w n)) nil)

                   (setf (gethash n removed-q)
                         (list sh (hv-w n) (hv-n n))))
                 (when sh
                   (cond
                     ((eql n (sh-start sh))
                      ;; save it for update later, since we need to
                      ;; search for new start, and might have deleted
                      ;; subhole completely
                      (setf (gethash sh affected-subholes) t))
                     ((eql n (sh-endv sh))
                      ;; move Q nodes forwards, other nodes backwards
                      (let ((p (if (hv-q n)
                                   (dll-next n)
                                   (dll-prev n))))
                        (setf (sh-endv sh) p)
                        (setf (sh-end sh) (hv-x p))
                        (setf (hv-sh p) sh))))))
               (when (eql n (h-vertices hole))
                 (setf (h-vertices hole) (dll-prev n)))
               (delete-node n))
             (mark-affected (start end)
               (declare (ignore end))
               (let ((sh nil))
                 (do-hole-vertices/next (v start dx nil)
                   (when (hv-sh v)
                     (setf sh (hv-sh v))
                     (setf (gethash (hv-sh v) affected-subholes) t)
                     (return nil))
                   (when (and dx (plusp dx))
                     (return nil))
                   (when (hv-q v)
                     (unless (< (hv-y v) (+ (y placement) (h placement)))
                       (return nil))))
                 (let ((leftp nil))
                   (do-hole-vertices/prev (v (dll-prev start) dx nil)
                     (when (hv-sh v)
                       (setf (gethash (hv-sh v) affected-subholes) t)
                       (return nil))
                     (when (and dx (plusp dx) leftp)
                       (return nil))
                     (when (and dx (minusp dx))
                       (setf leftp t))
                     (when (hv-w v)
                       (unless (< (hv-y v) (+ (y placement) (h placement)))
                         (return nil)))))))
             (ordered (a b c)
               (or (<= a b c) (<= c b a)))
             (new-vertex (x y prev)
               (let ((v (make-hole-vertex x y prev)))
                 (setf (gethash v classify-vertices) t)
                 v))
             (add-midpoint (v1 v2 x y)
               (let* ((x1 (hv-x v1))
                      (y1 (hv-y v1))
                      (x2 (hv-x v2))
                      (y2 (hv-y v2))
                      (h (= y1 y2 y))
                      (v (= x1 x2 x)))
                 (assert (or h v))
                 (assert (not (and h v)))
                 (cond
                   ((and (= x1 x) (= y1 y))
                    v1)
                   ((and (= x2 x) (= y2 y))
                    v2)
                   (h
                    (cond
                      ((ordered x1 x x2)
                       (loop while (ordered (hv-x (dll-next v1)) x x2)
                             do (setf v1 (dll-next v1))))
                      ((ordered x x1 x2)
                       (loop until (ordered (hv-x (dll-prev v1)) x x2)
                             do (setf v1 (dll-prev v1)))
                       (assert (= (hv-y v1) y2 y)))
                      (t (error "?")))
                    (if (= (hv-x v1) x)
                        v1
                        (new-vertex x y v1)))
                   (v
                    (cond
                      ((ordered y1 y y2)
                       (loop while (ordered (hv-y (dll-next v1)) y y2)
                             do (setf v1 (dll-next v1))))
                      ((ordered y y1 y2)
                       (loop until (ordered (hv-y (dll-prev v1)) y y2)
                             do (setf v1 (dll-prev v1)))
                       (assert (= (hv-x v1) x2 x)))
                      (t (error "?")))
                    (if (= (hv-y v1) y)
                        v1
                        (new-vertex x y v1)))))))

      ;; collect all the sequences of gaps, and remove points touching
      ;; two contacts
      (let ((to-remove (make-hash-table)))
        (do-dll/next (v overlap endp)
          (let* ((gap (eql :gap (classify v)))
                 (after-contact (not (eql :gap (classify (dll-prev v)))))
                 (before-contact (not (eql :gap (classify (dll-next v)))))
                 (before-corner (eql :corner (classify (dll-next v))))
                 (start-gap (and gap after-contact))
                 (end-gap (and gap before-contact))
                 (mid-contact (and (not gap)
                                   after-contact
                                   (not (eql :from
                                             (classify v))))))

            (when mid-contact
              (loop for a = (a v) then (dll-next a)
                    until (or (eql a (b v))
                              (and (= (hv-x a) (x2 v))
                                   (= (hv-y a) (y2 v))))
                    do (setf (gethash a to-remove) (dll-next a))))
            (when (and endp (not gap) before-contact)
              (loop for b = (b v) then (dll-prev b)
                    until (or (eql b (a v))
                              (and (= (hv-x b) (x1 v))
                                   (= (hv-y b) (y1 v))))
                    do (setf (gethash b to-remove) (dll-next b))))
            (cond
              (start-gap
               (let ((p (dll-prev v))
                     (prev nil))
                 (let ((n (add-midpoint (a p) (b p) (x1 v) (y1 v))))
                   (setf prev n))
                 (push (list prev) gaps)))
              (gap
               (let ((n (new-vertex (x1 v) (y1 v) nil)))
                 (push n (car gaps)))))
            (when end-gap
              (let ((p (dll-next v))
                    (next nil))
                ;; if the gap ends in a corner, add an extra vertex
                ;; since will have 2 separate holes with a vertex at
                ;; that point
                (let ((n (if before-corner
                             (new-vertex (x2 v) (y2 v) (a p))
                             (add-midpoint (a p) (b p) (x2 v) (y2 v)))))
                  (setf next n))
                (push next (car gaps))))))
        (maphash (lambda (k v) (declare (ignore v)) (remove-node k)) to-remove)

        (let ((new (> (length gaps) 1)))
          (when new
            ;; if we are creating new holes, clear existing Q/N/W links
            ;; since they confuse make-subholes
            (do-dll/next (sh (h-subholes hole))
              (when (and (sh-endv sh)
                         (hv-q (sh-endv sh)))
                (let* ((q (sh-endv sh))
                       (n (hv-n q))
                       (w (hv-w q)))
                  (setf (hv-n n) nil)
                  (setf (hv-w w) nil)
                  (setf (hv-q q) nil)
                  (setf (hv-n q) nil)
                  (setf (hv-w q) nil)))))

          ;; then update vertex connections, creating new holes if needed
          (loop for last = (car gaps)
                for (gap next) on gaps
                for h = hole then (make-instance 'hole)
                do (flet ((r (x)
                            (when (gethash x to-remove)
                              (break "removed gap" x))
                            (loop while (gethash x to-remove)
                                  do (setf x (gethash x to-remove)))
                            x))
                     (setf last (mapcar #'r last))
                     (setf gap (mapcar #'r gap)))
                   ;; add hole to dll if new
                   (unless (eql h hole)
                     (insert-before hole h)
                     (push h new-holes))
                   ;; if we are creatinug more holes, we need to
                   ;; rearrange the prev/next links at the gaps to split
                   ;; the holes
                   (when new
                     (setf (h-vertices h) (first gap))
                     (when next
                       ;; link endpoints of gap
                       (let* ((first (first gap))
                              (fnext (first next))
                              (next (dll-next first))
                              (nnext (dll-next fnext)))
                         ;; we need to update classify/corner for all
                         ;; modified vertices
                         (setf (gethash first classify-vertices) t)
                         (setf (gethash fnext classify-vertices) t)
                         (setf (gethash next classify-vertices) t)
                         (setf (gethash nnext classify-vertices) t)
                         (rotatef (dll-next first)
                                  (dll-next fnext))
                         (rotatef (dll-prev next)
                                  (dll-prev nnext)))))
                   ;; add intermediate gap points if any
                   (loop for (p v . n) on gap
                         while n
                         do (insert-after p v))
                   (unless new
                     ;; mark any subholes affected by new edge
                     (mark-affected (car gap) (car (last gap))))
                   ;; remove any extra vertices we added in the middle
                   ;; of edges
                   (loop for v in gap
                         for pv = (dll-prev v)
                         for nv = (dll-next v)
                         when (or (= (hv-x pv) (hv-x v) (hv-x nv))
                                  (= (hv-y pv) (hv-y v) (hv-y nv)))
                           do (when (eql (h-vertices h) v)
                                (setf (h-vertices h) (dll-prev v)))
                              (remove-node v)))))

      (loop for v being the hash-keys of classify-vertices
            do (multiple-value-bind (edge corner)
                   (classify-vertical-edge v)
                 (setf (hv-classify v) edge
                       (hv-corner v) corner)))

      ;; update or rebuild subholes
      (cond
        (new-holes
         ;; if we created new holes, just build all subholes from scratch
         (loop for h in (list* hole new-holes)
               for sh = (make-subholes h :clean t)
               do (assert sh)
                  (setf (h-subholes h) sh)))
        ((null gaps)
         ;; deleted the hole, remove it
         (break "remove hole")
         (delete-node hole))
        (t
         ;; updating subholes is still flaky, try rebuilding from scratch
         (do-dll/next (v (h-vertices hole))
           (setf (hv-q v) nil
                 (hv-w v) nil
                 (hv-n v) nil
                 (hv-sh v) nil))
         (setf (h-subholes hole)
               (make-subholes hole :clean t))))

      ;; if we had a falling corner, make sure it still is one
      (when (and (h-falling-corner-p hole)
                 (or (not (eql (hv-classify (h-falling-corner-p hole))
                               :falling-edge))
                     (not (dll-next (h-falling-corner-p hole)))))
        (setf (h-falling-corner-p hole) nil))

      (assert (dll-next hole))))
  hole)

#++
(ql:quickload '(binpack parachute))

(defun smallest-hole (px)
  (loop with a = 0
        with mh = nil
        with mp = nil
        for p in px
        for h = (hole p)
        when (or (not mp)
                 (< (* (ht-max-height h)
                       (ht-max-width h))
                    a))
          do (setf a (* (ht-max-height h)
                        (ht-max-width h)))
             (setf mh h)
             (setf mp nil)
        when (eql mh (hole p))
          do (push p mp)
        finally (return mp)))
(defun maximize-contact (px)
  (loop with mc = 0
        with mp = nil
        for p in px
        for pm = (penalty-multiplier p)
        do (unless (full-contact p)
             (setf (values (overlap p)
                           (full-contact p))
                   (intersect-hole-with-quad p)))
           (when (or (not mp)
                     (> (* pm (full-contact p))
                        mc))
             (setf mc (* pm (full-contact p))
                   mp nil))
        when (or (not mp) (= mc (* pm (full-contact p))))
          do (push p mp)
        finally (return mp)))

(defun minimize-contact (px)
  (loop with mc = 0
        with mp = nil
        for p in px
        for pm = (- 2 (penalty-multiplier p))
        do (unless (full-contact p)
             (setf (values (overlap p)
                           (full-contact p))
                   (intersect-hole-with-quad p)))
           (when (or (not mp) (< (* (full-contact p) pm) mc))
             (setf mc (* (full-contact p) pm)
                   mp nil))
        when (or (not mp) (= mc (* (full-contact p) pm)))
          do (push p mp)
        finally (return mp)))

(defun ceilingp2 (x)
  (expt 2 (ceiling (log x 2))))

(defclass shaping ()
  ((w :initform 16 :accessor w :initarg :w)
   (h :initform 16 :accessor h :initarg :h)
   ;; if set, try to maintain more square shape
   (square :initform t :accessor square :initarg :square)))

(defclass shaping-po2 (shaping)
  ((square :initform t :accessor square :initarg :square)))

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



(defclass shaping-quantized (shaping)
  ;; increase w,h by multiples of dx or dy
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


(defclass shaping-circle (shaping)
  ;; not expected to actually be useful
  ())

(defmethod shaping-penalty ((s shaping-circle) p)
  (setf (penalty-multiplier p)
        (let ((x (+ (x p) (w p)))
              (y (+ (y p) (h p))))
          (+ (expt x 2) (expt y 2)))))

(defmethod shaping-add ((s shaping-circle) p))


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



(defun minimize-penalty (px)
  (loop with mc = 0
        with mp = nil
        for p in px
        for pm = (penalty-multiplier p)
        do (when (or (not mp)
                     (< pm mc))
             (setf mc pm
                   mp nil))
        when (or (not mp)
                 (= mc pm))
          do (push p mp)
        finally (return mp)))

(defun place (hole w h &key
                         (heuristic1 'minimize-penalty)
                         (heuristic2 'maximize-contact)
                         shaping)
  (let ((px (find-all-placements hole w h)))
    (when shaping
      (loop for p in px do (shaping-penalty shaping p)))
    (progn ;unless (loop for p in px always (zerop (penalty-multiplier p))))
      #++(format t "px:~s " (map 'list 'penalty-multiplier px))
      ;; if we have more than 1 entry, try to pick the best
      (when (cdr px)
        (setf px (funcall heuristic1 px))
        #++(format t "1> ~s " (map 'list 'penalty-multiplier px)))
      ;; if we still have more than 1, maybe try more expensive tests
      (when (and (cdr px) heuristic2)
        (setf px (funcall heuristic2 px))
        #++(format t "2> ~s " (map 'list 'penalty-multiplier px)))
      #++(format t " -> ~s~%" (penalty-multiplier (car px)))
      (cond
        (px
         (let ((p (first px)))
           (when shaping
             (shaping-add shaping p))
           #++(format t "remove ~s,~s @ ~s,~s~%" (w p) (h p) (x p) (y p))
           (setf hole (remove-quad-from-hole hole p))
           (values hole (x p) (y p))))
        (t (values hole nil nil))))))
