(in-package #:binpack)

;;;
;;;  MAXRECT packing as defined in http://clb.demon.fi/files/RectangleBinPack.pdf
;;;  See also: https://github.com/juj/RectangleBinPack
;;;


(defun delta-weight (width height rect)
  (with-rect (nil nil nil w h) rect
    (min (- w width) (- h height))))

(defun rl (rect)
  (with-rect (nil x y w h) rect
    (list x y w h)))

#++
(defclass pack-state/mr (pack-state)
  ((free-rects :accessor pack-state-free-rects :initform nil)))
(defmethod pack-state-free-rects ((ps pack-state))
  ;; todo: implement multipage
  (aref (state ps) 0))

(defun start-pack/mr (width height)
  (make-pack-state :free-rects (list (rect nil 0 0 width height))))

(defun reset-pack/mr (state width height)
  (setf (pack-state-free-rects state) (list (rect nil 0 0 width height))))



(defun grow-rects (rects dx dy)
  (destructuring-bind (x1 y1)
      (loop for r in (pack-state-free-rects rects)
            maximize (+ (x r) (w r)) into mx
            maximize (+ (y r) (h r)) into my
            finally (return (list mx my)))
    (let ((x-edges ())
          (y-edges ())
          (new nil))
      (loop
        for r in (pack-state-free-rects rects)
        do (with-rect (nil x y w h) r
             (when (= x1 (+ x w))
               (push r y-edges)
               (incf w dx))
             (when (<= y1 (+ y h))
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
                   (push last (cdr (pack-state-free-rects rects)))
                   (push last new)
                   (setf start nil))
              finally (when (and last (< (+ (x last) (w last))
                                         (+ x1 dx)))
                        (let ((x2 (+ (x last) (w last)))
                              (nx (+ x1 dx (- (x last)))))
                          (setf (w last) (- x2 nx))
                          (setf (x last) nx))))
        (push (rect nil 0 y1 (+ x1 dx) dy) (cdr (pack-state-free-rects rects))))
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
                   (setf last (rect nil x1 start dx (- y start)))
                   (push last (cdr (pack-state-free-rects rects)))
                   (push last new)
                   (setf start nil))
              finally (when (and last (< (+ (y last) (h last))
                                         (+ y1 dy)))
                        (let ((y2 (+ (y last) (h last)))
                              (ny (+ y1 dy (- (y last)))))
                          (setf (h last) (- y2 ny))
                          (setf (y last) ny))))
        (push (rect nil x1 0 dx (+ y1 dy)) (cdr (pack-state-free-rects rects)))))))

(defun find-free-rect (width height rects)
  (unless rects (error 'packing-failed :w width :h height))
  (unless (pack-state-free-rects rects)
    (error 'packing-failed :w width :h height))
  (let ((retries 0)
        (max-retries 1000))
    (tagbody
     :retry
       (when (>= retries max-retries)
         (error "something wrong with resizing code? resized ~s~
           times without packing anything ~sx~s" retries width height))
       (loop
         :with min-rect = (first (pack-state-free-rects rects))
         :with min-delta = (delta-weight width height min-rect)
         :for rect :in (rest (pack-state-free-rects rects))
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
                          (expand-and-continue (dx dy)
                            :report "Increase available space and continue packing."
                            :interactive (lambda ()
                                           (format t "expand by (dx dy):")
                                           (read))
                            (when (or (not (integerp dx))
                                      (not (integerp dy))
                                      (minusp dx) (minusp dy)
                                      (and (zerop dx) (zerop dy)))
                              (error "can't expand packing by ~sx~s"
                                     dx dy))
                            (grow-rects rects dx dy)
                            (setf (pack-state-free-rects rects)
                                  (normalize-free-space
                                   (pack-state-free-rects rects)))
                            (incf retries)
                            (go :retry)))
                        min-rect))))))

(defun intersectsp (rect1 rect2)
  (with-rect (nil x1 y1 w1 h1) rect1
    (with-rect (nil x2 y2 w2 h2) rect2
      (and (< x1 (+ x2 w2))
           (> (+ x1 w1) x2)
           (< y1 (+ y2 h2))
           (> (+ y1 h1) y2)))))

(defun subdivide-rect (rect placed)
  (flet ((splitsp (coord from to)
           (> to coord from)))
    (if (intersectsp placed rect)
        (with-rect (nil x y w h) rect
          (with-rect (nil px py pw ph) placed
            (let ((result))
              (when (splitsp px x (+ x w))
                (push (rect nil x y (- px x) h) result))
              (when (splitsp (+ px pw) x (+ x w))
                (push (rect nil (+ px pw) y (- (+ x w) (+ px pw)) h) result))
              (when (splitsp py y (+ y h))
                (push (rect nil x y w (- py y)) result))
              (when (splitsp (+ py ph) y (+ y h))
                (push (rect nil x (+ py ph) w (- (+ y h) (+ py ph))) result))
              result)))
        (list rect))))

(defun containsp (outer inner)
  (with-rect (nil ox oy ow oh) outer
    (with-rect (nil ix iy iw ih) inner
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
   (loop :for free-rect :in (pack-state-free-rects free-rects)
         :append (subdivide-rect free-rect rect))))

(defun place-rect (rect free-rects)
  (with-rect (nil nil nil w h) rect
    (with-rect (nil fx fy nil nil) (find-free-rect w h free-rects)
      (let ((placed (apply #'make-instance (class-of rect)
                           :x fx :y fy
                           (rect-initargs rect))))
        (list placed (resolve-free-rects placed free-rects))))))

(defun pack-1/mr (rect state)
  (destructuring-bind (placed new-free-rects)
      (place-rect rect state)
    (setf (pack-state-free-rects state) new-free-rects)
    placed))

#++
(defun pack/mr (rects width height)
  (loop
    (restart-case
        (let ((maxw 0)
              (maxh 0))
          (return-from pack/mr
            (values
             (loop :with free-rects = (start-pack width height)
                   :for rect :in (sort-rects (copy-seq rects))
                   :for placed = (pack-1 rect free-rects)
                   :do (setf maxw (max maxw (+ (x placed) (w placed))))
                       (setf maxh (max maxh (+ (y placed) (h placed))))
                   :collect placed)
             maxw maxh)))
      (expand-and-retry (dx dy)
        :report "Increase available space and restart packing"
        :interactive (lambda ()
                       (format t "expand by (dx dy):")
                       (read))
        (when (or (not (integerp dx))
                  (not (integerp dy))
                  (minusp dx) (minusp dy)
                  (and (zerop dx) (zerop dy)))
          (error "can't expand packing by ~sx~s"
                 dx dy))
        (incf width dx)
        (incf height dy)))))

#++
(defun %auto-pack (rects &key (width :auto) (height :auto)
                           (auto-size-granularity-x 4)
                           (auto-size-granularity-y 1)
                           (expand-mode :restart))
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
                 (apply 'invoke-restart (ecase expand-mode
                                          (:restart 'expand-and-retry)
                                          (:continue 'expand-and-continue))
                        auto-delta)))))
        (pack rects awidth aheight)))))

#++
(defun auto-pack/mr (rects &key (width :auto) (height :auto)
                          (auto-size-granularity-x 4)
                          (auto-size-granularity-y 1)
                          optimize-pack
                          (expand-mode (if optimize-pack :continue :restart)))
  (if optimize-pack
      (loop with best = nil
            with best-total = most-positive-fixnum
            with minw = (loop for r in rects maximize (w r))
            with minh = (loop for r in rects maximize (h r))
            with total-pixels = (total-pixels rects)
            ;; search from larger of 2x min width or 4:1 aspect ratio
            for w0 from (* auto-size-granularity-x
                           (ceiling (max (/ (sqrt total-pixels) 2)
                                         (* 2 minw))
                                    auto-size-granularity-x))
            by auto-size-granularity-x
            for last-h = 0
            for last-w = 0
            do (loop for mode in (if (eql optimize-pack :both)
                                     '(:continue :restart)
                                     (list expand-mode))
                     for (pack w h)
                       = (multiple-value-list
                          (%auto-pack
                           rects
                           :width w0 :height :auto
                           :auto-size-granularity-x auto-size-granularity-x
                           :auto-size-granularity-y auto-size-granularity-y
                           :expand-mode mode))
                     for aspect = (1+ (* 1/100 (- (/ (max w h) (min w h)) 1)))
                     for total = (* aspect (* w h))
                     #+do (format t "auto-sizing ~s: ~sx~s, ~a ~s / ~s :: ~s~%"
                                  mode
                                  w h (if (< total best-total) "++" "--")
                                  (float total) (float best-total)
                                  (float (/ (* w h) total-pixels)))
                     do (setf last-h h last-w w)
                     when (< total best-total)
                       do (setf best-total total)
                          (setf best (list pack w h)))
               ;; stop when we hit 2x min height or 1:4 aspect ratio
            while (and (> last-h (* 1/4 last-w))
                       (> last-h (* 2 minh)))
            finally (return (values-list best)))
      (%auto-pack rects
                  :width width :height height
                  :auto-size-granularity-x auto-size-granularity-x
                  :auto-size-granularity-y auto-size-granularity-y
                  :expand-mode expand-mode)))
