(in-package #:binpack)

(defun pack-1 (rect state)
  (pack-1/mr rect state))

(defun pack (rects width height)
  (loop
    (restart-case
        (let ((maxw 0)
              (maxh 0))
          (return-from pack
            (values
             (loop :with free-rects = (start-pack/mr width height)
                   :for rect :in (sort-rects/longest-side-desc (copy-seq rects))
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

(defun auto-pack (rects &key (width :auto) (height :auto)
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
