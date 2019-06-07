(in-package #:binpack)

;;;
;;;  MAXRECT packing as defined in http://clb.demon.fi/files/RectangleBinPack.pdf
;;;  See also: https://github.com/juj/RectangleBinPack
;;;

(defmacro with-rect ((x y &optional (w (gensym)) (h (gensym))) rect &body body)
  `(destructuring-bind (,x ,y ,w ,h) ,rect
     (declare (ignorable ,x ,y ,w ,h))
     ,@body))

(defun delta-weight (w h rect)
  (with-rect (x y rw rh) rect
    (min (- rw w) (- rh h))))

;; for debugging, needs opticl
#++
(defun draw-rects (file rects)
  (destructuring-bind (x1 y1)
      (loop for (x y w h) in rects
            maximize (+ x w) into mx
            maximize (+ y h) into my
            finally (return (list mx my)))
    (let ((img (opticl:make-8-bit-rgb-image y1 x1)))
      (loop for (x y w h) in rects
            for r = (+ 64 (random (- 255 64)))
            for g = (+ 64 (random (- 255 64)))
            for b = (+ 64 (random (- 255 64)))
            do (opticl:draw-rectangle img y x (+ y h) (+ x w) r g b))
      (opticl:write-image-file file img))))

(defun grow-rects (rects dx dy)
  (destructuring-bind (x1 y1)
      (loop for (x y w h) in rects
            maximize (+ x w) into mx
            maximize (+ y h) into my
            finally (return (list mx my)))
    (let ((x-edges ())
          (y-edges ()))
      (loop
        for r in rects
        do (with-rect (x y w h) r
             (when (= x1 (+ x w))
               (push r y-edges)
               (incf (third r) dx))
             (when (= y1 (+ y h))
               (push r x-edges)
               (incf (fourth r) dy))))
      (setf x-edges (sort x-edges '< :key 'first)
            y-edges (sort y-edges '< :key 'second))
      #++(format t "x:~s~%" x-edges)
      #++(format t "y:~s~%" y-edges)
      (when (and x-edges (plusp dy))
        ;; start outside edge to simplify handling of edge
        (loop with live = (list (list -1 0 1 0))
              with start = nil
              with last = nil
              for x below (+ x1 dx)
              for in = live
              do (loop for edge = (car x-edges)
                       while (and edge (= (first edge) x))
                       do (push (pop x-edges) live))
                 (setf live (loop for l in live
                                  for (lx nil w nil) = l
                                  unless (<= (+ lx w) x)
                                    collect l))
              #++(format t "x: ~s start:~s~% live ~s~% in=~s~%" x start live
                         in)
                 (when (and in (not live))
                   (setf start x))
                 (when (and live (not in))
                   ;; fixme: put rects in an object or something
                   ;; instead of expanding it from the middle like
                   ;; this...
                   (setf last (list start y1 (- x start) dy))
                   #++(format t "add x edge ~s~%" last)
                   (push last (cdr rects))
                   (setf start nil))
              finally (when (and last (< (+ (first last) (third last))
                                         (+ x1 dx)))
                        #++(format t "Resize xlast ~s -> ~s~%" last
                                   (+ x1 dx (- (first last))))
                        (setf (third last)
                              (+ x1 dx (- (first last)))))))
      #++(format t "---~%")
      (when (and y-edges (plusp dx))
        ;; start outside edge to simplify handling of edge
        (loop with live = (list (list 0 -1 0 1))
              with start = nil
              with last = nil
              for y below (+ y1 dy)
              for in = live
              do (loop for edge = (car y-edges)
                       while (and edge (= (second edge) y))
                       do (push (pop y-edges) live))
                 (setf live (loop for l in live
                                  for (nil ly nil h) = l
                                  unless (<= (+ ly h) y)
                                    collect l))
              #++(format t "y: ~s start:~s~% live ~s~% in=~s~%" y start live
                         in)
                 (when (and in (not live))
                   (setf start y))
                 (when (and live (not in))
                   ;; fixme: put rects in an object or something
                   ;; instead of expanding it from the middle like
                   ;; this...
                   (setf last (list x1 start dx (- y start)))
                   #++(format t "add y edge ~s~%" last)
                   (push last (cdr rects))
                   (setf start nil))
              finally (when (and last (< (+ (second last) (fourth last))
                                         (+ y1 dy)))
                        #++(format t "Resize ylast ~s -> ~s~%" last
                                   (+ y1 dy (- (second last))))
                        (setf (fourth last)
                              (+ y1 dy (- (second last))))))))))

(define-condition packing-failed (simple-error)
  ((w :reader w :initarg :w)
   (h :reader h :initarg :h))
  (:report (lambda (c s)
             (format s "Cannot pack any more rectangles (trying to pack ~sx~s)"
                     (w c) (h c)))))

(defun find-free-rect (w h rects)
  (let ((retries 0)
        (max-retries 1000))
    (tagbody
     :retry
       (when (>= retries max-retries)
         (error "something wrong with resizing code? resized ~s~
           times without packing anything" retries))
       (loop with min-rect = (car rects)
             with min-d = (delta-weight w h min-rect)
             for rect in (rest rects)
             for cur-d = (delta-weight w h rect)
             ;; add case for when w and h of free rect exactly matches required w h
             when (or (< min-d 0) (and (>= cur-d 0) (< cur-d min-d)))
               do (setf min-rect rect
                        min-d cur-d)
             finally (return-from find-free-rect
                       (if (< min-d 0)
                           (restart-case
                               (error 'packing-failed :w w :h h)
                             (expand (dx dy)
                               :interactive (lambda ()
                                              (format t "expand by (dx dy):")
                                              (read))
                               (when (or (not (integerp dx))
                                         (not (integerp dy))
                                         (minusp dx) (minusp dy)
                                         (and (zerop dx) (zerop dy)))
                                 (error "can't expand packing by ~sx~s" dx dy))
                               (progn
                                 #++(draw-rects "/tmp/rects1.png" rects)
                                 #++(format t "rects: ~s~%" rects)
                                 (grow-rects rects dx dy)
                                 (setf rects (normalize-free-space rects))
                                 #++(format t "grown: ~s~%" rects)
                                 #++(draw-rects "/tmp/rects2.png" rects)
                                 #++(break "s,jhb"))
                               (incf retries)
                               (go :retry)))
                           min-rect))))))

(defun intersectsp (r0 r1)
  (with-rect (x0 y0 w0 h0) r0
    (with-rect (x1 y1 w1 h1) r1
      (and (< x0 (+ x1 w1))
           (> (+ x0 w0) x1)
           (< y0 (+ y1 h1))
           (> (+ y0 h0) y1)))))


(defun splitsp (coord coord-from coord-to)
  (> coord-to coord coord-from))

(defun subdivide-rect (rect placed)
  (if (intersectsp placed rect)
      (with-rect (x y w h) rect
        (with-rect (xp yp wp hp) placed
          (let (result)
            ;; left part
            (when (splitsp xp x (+ x w))
              (push (list x y (- xp x) h) result))
            ;; right part
            (when (splitsp (+ xp wp) x (+ x w))
              (push (list (+ xp wp) y (- (+ x w) (+ xp wp)) h) result))
            ;; bottom
            (when (splitsp yp y (+ y h))
              (push (list x y w (- yp y)) result))
            ;; top
            (when (splitsp (+ yp hp) y (+ y h))
              (push (list x (+ yp hp) w (- (+ y h) (+ yp hp))) result))
            result)))
      (list rect)))

(defun subdivide-intersecting (rect free-rects)
  (loop for free-rect in free-rects appending (subdivide-rect free-rect rect)))

(defun containsp (outer inner)
  (with-rect (x0 y0 w0 h0) outer
    (with-rect (x1 y1 w1 h1) inner
      (and (>= (+ x0 w0) (+ x1 w1) x1 x0)
           (>= (+ y0 h0) (+ y1 h1) y1 y0)))))

(defun normalize-free-space (rects)
  (loop with rest-filtered = rects
        for (rect . rest) = rest-filtered until (null rect)
        collecting
        (loop with contained-p = nil
              for other-rect in rest
              unless (containsp rect other-rect) collect other-rect into filtered
                when (and (not contained-p) (containsp other-rect rect))
                  do (setf contained-p t)
              finally
                 (setf rest-filtered filtered)
                 (return (unless contained-p rect)))
          into result
        finally (return (delete-if #'null result))))

(defun subrect (w h rect)
  (with-rect (x y) rect
    (list x y w h)))

(defun place-rect (w h free-rects)
  (let* ((free-rect (find-free-rect w h free-rects))
         (result (subrect w h free-rect)))
    (values result (normalize-free-space (subdivide-intersecting result
                                                                 free-rects)))))

(defun pack (dimensions &key width height (order :double-sort))
  (labels ((largest-side (el)
             (max (second el) (third el)))
           (shortest-side (el)
             (min (second el) (third el)))
           (short-side-last ()
             (sort dimensions #'> :key #'shortest-side))
           (double-sorted-dimensions ()
             (ecase order
               (:double-sort
                (sort (short-side-last) #'> :key #'largest-side))
               (:random
                (alexandria:shuffle dimensions)))))
    (let ((maxw 0)
          (maxh 0))
      (values
       (loop with free-rects = (list (list 0 0 width height))
             for (id rect-width rect-height) in (double-sorted-dimensions)
             collect
             (multiple-value-bind (rect new-free-rects)
                 (place-rect rect-width rect-height free-rects)
               (setf free-rects new-free-rects)
               (with-rect (x y w h) rect
                 (setf maxw (max maxw (+ x w)))
                 (setf maxh (max maxh (+ y h)))
                 (list id x y w h)))
             #+finally (progn
                         (draw-rects "/tmp/rects1.png" free-rects)
                         (format t "rects = ~s~%" free-rects)))
       maxw maxh))))

(defun total-pixels (glyph-data)
  (loop for (nil w h) in glyph-data
        sum (* w h)))


(defun %auto-pack (dimensions &key (width :auto) (height :auto)
                                (auto-size-granularity-x 4)
                                (auto-size-granularity-y 1))
  (flet ((ceiling-asgx (x)
           (* auto-size-granularity-x (ceiling x auto-size-granularity-x)))
         (ceiling-asgy (y)
           (* auto-size-granularity-y (ceiling y auto-size-granularity-y))))
    (let* (;; start with size it would take if it could pack perfectly
           (total-pixels (total-pixels dimensions))
           (awidth (cond
                     ((numberp width) width)
                     ((numberp height) (ceiling-asgx (/ total-pixels height)))
                     (t (ceiling-asgx (sqrt total-pixels)))))
           (aheight (cond
                      ((numberp height) height)
                      ((numberp width) (ceiling-asgy (/ total-pixels width)))
                      (t (ceiling-asgy (sqrt total-pixels)))))
           (auto-delta (list
                        (if (eql width :auto) auto-size-granularity-x 0)
                        (if (eql height :auto) auto-size-granularity-y 0))))
      (handler-bind
          ((binpack:packing-failed
             (lambda (c)
               (when (or (eql width :auto)
                         (eql height :auto))
                 (incf awidth (first auto-delta))
                 (incf aheight (second auto-delta))
                 (assert (not (every 'zerop auto-delta)))
                 (apply 'invoke-restart 'binpack:expand auto-delta)))))
        (binpack:pack
         dimensions
         :width awidth
         :height aheight)))))

(defun auto-pack (dimensions &key (width :auto) (height :auto)
                               (auto-size-granularity-x 4)
                               (auto-size-granularity-y 1)
                               optimize-pack)
  (if optimize-pack
      (loop with best = nil
            with best-total = most-positive-fixnum
            with min = (loop for (nil w nil) in dimensions maximize w)
            ;;with total-pixels = (total-pixels dimensions)
            for w2 from (* auto-size-granularity-x
                           (ceiling (* 4 min) auto-size-granularity-x))
            by auto-size-granularity-x
            for (pack h w)
              = (multiple-value-list
                 (%auto-pack
                  (copy-tree dimensions)
                  :width w2 :height height
                  :auto-size-granularity-x auto-size-granularity-x
                  :auto-size-granularity-y auto-size-granularity-y))
            for aspect = (1+ (* 1/100 (- (/ (max w h) (min w h)) 1)))
            for total = (* aspect (* w h))
            when (< total best-total)
              do (format t "auto-sizing: ~s < ~s @ ~s~%"
                         (float total) (float best-total)
                         (list w h))
                 (setf best-total total)
                 (setf best (list pack w h))
            while (> w (* 1/4 h))
            finally (return (values-list best)))
      (%auto-pack dimensions
                  :width width :height height
                  :auto-size-granularity-x auto-size-granularity-x
                  :auto-size-granularity-y auto-size-granularity-y)))
