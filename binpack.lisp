(in-package #:binpack)

;;;
;;;  MAXRECT packing as defined in http://clb.demon.fi/files/RectangleBinPack.pdf
;;;  See also: https://github.com/juj/RectangleBinPack
;;;
(defclass rect ()
  ((id :reader id :initarg :id)
   (x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (w :reader w :initarg :w)
   (h :reader h :initarg :h)))

(defun rect (id x y w h)
  (make-instance 'rect :id id :x x :y y :w w :h h))

(defmacro with-rect ((id x y w h) rect &body body)
  (alexandria:once-only (rect)
    `(let (,@(when id `((,id (id ,rect))))
           ,@(when x `((,x (x ,rect))))
           ,@(when y `((,y (y ,rect))))
           ,@(when w `((,w (w ,rect))))
           ,@(when h `((,h (h ,rect)))))
       ,@body)))

(defgeneric rect-initargs (rect)
  (:method-combination append))

(defmethod rect-initargs append ((r rect))
  (list :id (id r) :x (x r) :y (y r) :w (w r) :h (h r)))

(define-condition packing-failed (simple-error)
  ((w :reader w :initarg :w)
   (h :reader h :initarg :h))
  (:report (lambda (c s)
             (format s "Cannot pack any more rectangles (trying to pack ~sx~s)"
                     (w c) (h c)))))

(defun delta-weight (width height rect)
  (with-slots (x y w h) rect
    (min (- w width) (- h height))))

(defun grow-rects (rects dx dy)
  (destructuring-bind (x1 y1)
      (loop for r in rects
            maximize (+ (x r) (w r)) into mx
            maximize (+ (y r) (h r)) into my
            finally (return (list mx my)))
    (let ((x-edges ())
          (y-edges ()))
      (loop
        for r in rects
        do (with-slots (x y w h) r
             (when (= x1 (+ x w))
               (push r y-edges)
               (incf w dx))
             (when (= y1 (+ y h))
               (push r x-edges)
               (incf h dy))))
      (setf x-edges (sort x-edges '< :key 'x)
            y-edges (sort y-edges '< :key 'y))
      (when (and x-edges (plusp dy))
        ;; start outside edge to simplify handling of edge
        (loop with live = (list (rect nil -1 0 1 0))
              with start = nil
              with last = nil
              for x below (+ x1 dx)
              for in = live
              do (loop for edge = (car x-edges)
                       while (and edge (= (x edge) x))
                       do (push (pop x-edges) live))
                 (setf live (loop for l in live
                                  unless (<= (+ (x l) (w l)) x)
                                    collect l))
                 (when (and in (not live))
                   (setf start x))
                 (when (and live (not in))
                   ;; fixme: put rects in an object or something
                   ;; instead of expanding it from the middle like
                   ;; this...
                   (setf last (rect nil start y1 (- x start) dy))
                   (push last (cdr rects))
                   (setf start nil))
              finally (when (and last (< (+ (x last) (w last))
                                         (+ x1 dx)))
                        (setf (slot-value last 'x)
                              (+ x1 dx (- (x last)))))))
      (when (and y-edges (plusp dx))
        ;; start outside edge to simplify handling of edge
        (loop with live = (list (rect nil 0 -1 0 1))
              with start = nil
              with last = nil
              for y below (+ y1 dy)
              for in = live
              do (loop for edge = (car y-edges)
                       while (and edge (= (y edge) y))
                       do (push (pop y-edges) live))
                 (setf live (loop for l in live
                                  unless (<= (+ (y l) (h l)) y)
                                    collect l))
                 (when (and in (not live))
                   (setf start y))
                 (when (and live (not in))
                   ;; fixme: put rects in an object or something
                   ;; instead of expanding it from the middle like
                   ;; this...
                   (setf last (rect nil x1 start (- y dx) start))
                   (push last (cdr rects))
                   (setf start nil))
              finally (when (and last (< (+ (y last) (h last))
                                         (+ y1 dy)))
                        (setf (slot-value last 'y)
                              (+ y1 dy (- (y last))))))))))

(defun find-free-rect (width height rects)
  (unless rects (error 'packing-failed :w width :h height))
  (let ((retries 0)
        (max-retries 1000))
    (tagbody
     :retry
       (when (>= retries max-retries)
         (error "something wrong with resizing code? resized ~s~
           times without packing anything" retries))
       (loop
         :with min-rect = (first rects)
         :with min-delta = (delta-weight width height min-rect)
         :for rect :in (rest rects)
         :for current-delta = (delta-weight width height rect)
         :when (or (minusp min-delta)
                   (and (not (minusp current-delta))
                        (< current-delta min-delta)))
           :do (setf min-rect rect
                     min-delta current-delta)
         :finally (return-from find-free-rect
                    (if (minusp min-delta)
                        (restart-case
                            (error 'packing-failed :w width :h height)
                          (expand (dx dy)
                            :interactive (lambda ()
                                           (format t "expand by (dx dy):")
                                           (read))
                            (when (or (not (integerp dx))
                                      (not (integerp dy))
                                      (minusp dx) (minusp dy)
                                      (and (zerop dx) (zerop dy)))
                              (error "can't expand packing by ~sx~s"
                                     dx dy))
                            (progn
                              (grow-rects rects dx dy)
                              (setf rects (normalize-free-space rects)))
                            (incf retries)
                            (go :retry)))
                        min-rect))))))

(defun intersectsp (rect1 rect2)
  (with-slots ((x1 x) (y1 y) (w1 w) (h1 h)) rect1
    (with-slots ((x2 x) (y2 y) (w2 w) (h2 h)) rect2
      (and (< x1 (+ x2 w2))
           (> (+ x1 w1) x2)
           (< y1 (+ y2 h2))
           (> (+ y1 h1) y2)))))

(defun subdivide-rect (rect placed)
  (flet ((splitsp (coord from to)
           (> to coord from)))
    (if (intersectsp placed rect)
        (with-slots (file id x y w h) rect
          (with-slots ((px x) (py y) (pw w) (ph h)) placed
            (let ((result))
              (when (splitsp px x (+ x w))
                (push (rect id x y (- px x) h) result))
              (when (splitsp (+ px pw) x (+ x w))
                (push (rect id (+ px pw) y (- (+ x w) (+ px pw)) h) result))
              (when (splitsp py y (+ y h))
                (push (rect id x y w (- py y)) result))
              (when (splitsp (+ py ph) y (+ y h))
                (push (rect id x (+ py ph) w (- (+ y h) (+ py ph))) result))
              result)))
        (list rect))))

(defun containsp (outer inner)
  (with-slots ((ox x) (oy y) (ow w) (oh h)) outer
    (with-slots ((ix x) (iy y) (iw w) (ih h)) inner
      (and (>= (+ ox ow) (+ ix iw) ix ox)
           (>= (+ oy oh) (+ iy ih) iy oy)))))

(defun normalize-free-space (rects)
  (remove
   nil
   (loop :with rest-filtered = rects
         :for (rect . rest) = rest-filtered
         :while rect
         :collect (loop :with containedp
                        :for other-rect :in rest
                        :unless (containsp rect other-rect)
                          :collect other-rect :into filtered
                        :when (and (not containedp)
                                   (containsp other-rect rect))
                          :do (setf containedp t)
                        :finally (setf rest-filtered filtered)
                                 (return (unless containedp rect))))))

(defun resolve-free-rects (rect free-rects)
  (normalize-free-space
   (loop :for free-rect :in free-rects
         :append (subdivide-rect free-rect rect))))

(defun place-rect (rect free-rects)
  (with-slots (w h) rect
    (with-slots ((fx x) (fy y)) (find-free-rect w h free-rects)
      (let ((placed (apply #'make-instance (class-of rect)
                           :x fx :y fy
                           (rect-initargs rect))))
        (list placed (resolve-free-rects placed free-rects))))))

(defun sort-rects (rects)
  (labels ((apply-fn (fn rect)
             (with-slots (w h) rect
               (funcall fn w h)))
           (sort-by (rects fn)
             (stable-sort rects #'> :key (lambda (x) (apply-fn fn x)))))
    (sort-by (sort-by rects #'min) #'max)))

(defun pack (rects width height)
  (let ((maxw 0)
        (maxh 0))
    (values
     (loop :with free-rects = (list (rect nil 0 0 width height))
           :for rect :in (sort-rects rects)
           :for (placed new-free-rects) = (place-rect rect free-rects)
           :do (setf free-rects new-free-rects)
               (setf maxw (max maxw (+ (x placed) (w placed))))
               (setf maxh (max maxh (+ (y placed) (h placed))))
           :collect placed)
     maxw maxh)))

(defun total-pixels (rects)
  (loop for r in rects sum (* (w r) (h r))))


(defun %auto-pack (rects &key (width :auto) (height :auto)
                           (auto-size-granularity-x 4)
                           (auto-size-granularity-y 1))
  (flet ((ceiling-asgx (x)
           (* auto-size-granularity-x (ceiling x auto-size-granularity-x)))
         (ceiling-asgy (y)
           (* auto-size-granularity-y (ceiling y auto-size-granularity-y))))
    (let* (;; start with size it would take if it could pack perfectly
           (total-pixels (total-pixels rects))
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
          ((packing-failed
             (lambda (c)
               (declare (ignorable c))
               (when (or (eql width :auto)
                         (eql height :auto))
                 (incf awidth (first auto-delta))
                 (incf aheight (second auto-delta))
                 (assert (not (every 'zerop auto-delta)))
                 (apply 'invoke-restart 'binpack:expand auto-delta)))))
        (pack rects awidth aheight)))))

(defun auto-pack (rects &key (width :auto) (height :auto)
                          (auto-size-granularity-x 4)
                          (auto-size-granularity-y 1)
                          optimize-pack)
  (if optimize-pack
      (loop with best = nil
            with best-total = most-positive-fixnum
            with minw = (loop for r in rects maximize (w r))
            with minh = (loop for r in rects maximize (h r))
            ;;with total-pixels = (total-pixels dimensions)
            for w2 from (* auto-size-granularity-x
                           (ceiling (* 4 minw) auto-size-granularity-x))
            by auto-size-granularity-x
            for (pack w h)
              = (multiple-value-list
                 (%auto-pack (copy-list rects)
                             :width w2 :height height
                             :auto-size-granularity-x auto-size-granularity-x
                             :auto-size-granularity-y auto-size-granularity-y))
            for aspect = (1+ (* 1/100 (- (/ (max w h) (min w h)) 1)))
            for total = (* aspect (* w h))
            do (format t "auto-sizing: ~sx~s, ~a ~s / ~s~%"
                       w h (if (< total best-total) "++" "--")
                       (float total) (float best-total))
            when (< total best-total)
              do (setf best-total total)
                 (setf best (list pack w h))
            while (and (> h (* 1/4 w))
                       (> h minh))
            finally (return (values-list best)))
      (%auto-pack rects
                  :width width :height height
                  :auto-size-granularity-x auto-size-granularity-x
                  :auto-size-granularity-y auto-size-granularity-y)))
