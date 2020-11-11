;; test suite for "common.lisp" (sorting, etc)
(defpackage #:binpack-test/test-common
  (:use :cl :parachute #:binpack-test/common)
  (:local-nicknames (:a :alexandria-2)
                    (:bc :binpack/common)))
(in-package #:binpack-test/test-common)

(define-test binpack-common)

(define-test (binpack-common rect)
  (let ((r (finish (bc:rect :a 1 2 3 4)))
        (r2 (finish (bc:rect* 5 6)))
        (r3 (finish (bc:rect* 7 8 :b))))
    (is eql :a (bc:id r))
    (is eql 1 (bc:x r))
    (is eql 2 (bc:y r))
    (is eql 3 (bc:w r))
    (is eql 4 (bc:h r))
    (is eql nil (bc:page r))

    (is eql nil (bc:id r2))
    (is eql nil (bc:x r2))
    (is eql nil (bc:y r2))
    (is eql 5 (bc:w r2))
    (is eql 6 (bc:h r2))
    (is eql nil (bc:page r2))

    (is eql :b (bc:id r3))
    (is eql nil (bc:x r3))
    (is eql nil (bc:y r3))
    (is eql 7 (bc:w r3))
    (is eql 8 (bc:h r3))
    (is eql nil (bc:page r3))

    (bc:with-rect (id x y w h) r
      (is eql :a id)
      (is eql 1 x)
      (is eql 2 y)
      (is eql 3 w)
      (is eql 4 h))

    (false (bc:point-in-rect r 0 0))
    (true (bc:point-in-rect r 1 2))
    (true (bc:point-in-rect r 2 3))
    (true (bc:point-in-rect r 4 6))
    (false (bc:point-in-rect r 5 7))

    (is equalp '(:id :a :x 1 :y 2 :w 3 :h 4) (bc:rect-initargs r))

    (finish (setf (bc:x r2) 0
                  (bc:y r2) 0))
    (finish (setf (bc:x r3) 5
                  (bc:y r3) 7))

    (true (bc:intersectsp r r2))
    (false (bc:intersectsp r r3))
    (false (bc:intersectsp r2 r3))

    (true (bc:intersectsp (bc:rect 1 0 0 1 1) (bc:rect 2 0 0 1 1)))
    (false (bc:intersectsp (bc:rect 1 0 0 1 1) (bc:rect 2 0 1 1 1)))
    (false (bc:intersectsp (bc:rect 1 0 0 1 1) (bc:rect 2 1 0 1 1)))
    (true (bc:intersectsp (bc:rect 1 0 0 1 2) (bc:rect 2 0 1 1 1)))
    (false (bc:intersectsp (bc:rect 1 0 0 2 1) (bc:rect 2 0 1 1 1)))
    (false (bc:intersectsp (bc:rect 1 0 0 1 2) (bc:rect 2 1 0 1 1)))
    (true (bc:intersectsp (bc:rect 1 0 0 2 1) (bc:rect 2 1 0 1 1)))

    (true (bc:containsp (bc:rect 1 -1 -1 3 3) (bc:rect 2 0 0 1 1)))
    (true (bc:containsp (bc:rect 1 -1 -1 2 2) (bc:rect 2 0 0 1 1)))
    (true (bc:containsp (bc:rect 1 0 0 1 1) (bc:rect 2 0 0 1 1)))
    (true (bc:containsp (bc:rect 1 0 0 2 2) (bc:rect 2 0 0 1 1)))
    (false (bc:containsp (bc:rect 1 0 0 1 2) (bc:rect 2 0 0 2 2)))
    (false (bc:containsp (bc:rect 1 0 0 2 1) (bc:rect 2 0 0 2 2)))
    (false (bc:containsp (bc:rect 1 0 0 1 1) (bc:rect 2 0 0 2 2)))))



(defun test-sort (rects sort test &key reverse)
  (let* ((r (copy-list rects))
         (s (funcall sort r)))
    (when reverse
      (setf s (reverse s)))
    (loop for (a b) on s
          always (or (not b) (funcall test a b)))))

(define-test (binpack-common sort)
  (let ((rects (random-cuts 512 512 8 123)))
    (labels ((l-s-d (a b)
               (or (> (max (bc:w a) (bc:h a))
                      (max (bc:w b) (bc:h b)))
                   (and (= (max (bc:w a) (bc:h a))
                           (max (bc:w b) (bc:h b)))
                        (>= (min (bc:w a) (bc:h a))
                            (min (bc:w b) (bc:h b))))))
             (a (a b)
               (>= (* (bc:w a) (bc:h a))
                   (* (bc:w b) (bc:h b))))
             (w (a b)
               (>= (bc:w a) (bc:w b)))
             (h (a b)
               (>= (bc:h a) (bc:h b)))
             (w+h (a b)
               (or (> (bc:w a) (bc:w b))
                   (and (= (bc:w a) (bc:w b))
                        (>= (bc:h a) (bc:h b)))))
             (perimeter (a b)
               (>= (+ (bc:w a) (bc:h a))
                   (+ (bc:w b) (bc:h b))))
             (aa1 (a)
               (* (/ (max (bc:w a) (bc:h a))
                     (min (bc:w a) (bc:h a)))
                  (* (bc:w a) (bc:h a))))
             (a*a (a b)
               (>= (aa1 a) (aa1 b))))
      (true (test-sort rects #'bc:sort-rects/longest-side-desc #'l-s-d))
      (true (test-sort rects #'bc:sort-rects/area-desc #'a))
      (true (test-sort rects #'bc:sort-rects/width-desc #'w))
      (true (test-sort rects #'bc:sort-rects/height-desc #'h))
      (true (test-sort rects #'bc:sort-rects/w+h-desc #'w+h))
      (true (test-sort rects #'bc:sort-rects/perimeter-desc #'perimeter))
      (true (test-sort rects #'bc:sort-rects/aspect*area-desc #'a*a))

      (false (test-sort rects #'bc:sort-rects/longest-side-desc #'l-s-d
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/area-desc #'a
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/width-desc #'w
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/height-desc #'h
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/w+h-desc #'w+h
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/perimeter-desc #'perimeter
                        :reverse t))
      (false (test-sort rects #'bc:sort-rects/aspect*area-desc #'a*a
                        :reverse t)))))

(define-test (binpack-common misc)
  (is-values (bc:rects-bounds nil)
    (eql 0) (eql 0) (eql 0))

  (is-values (bc:rects-bounds (list (bc:rect nil 0 0 10 10)))
    (eql 10) (eql 10) (eql 0))

  (is-values (bc:rects-bounds (list (bc:rect nil 0 0 10 10)
                                    (bc:rect nil 0 0 11 11)
                                    (bc:rect nil 0 0 1 1)))
    (eql 11) (eql 11) (eql 0))

  (is-values (bc:rects-bounds (list (bc:rect nil 9 9 1 1)
                                    (bc:rect nil 1 11 1 1)
                                    (bc:rect nil 11 1 1 1)))
    (eql 12) (eql 12) (eql 0))


  (is = 65536 (bc:total-pixels (even-cuts 256 256 4)))
  (is = 65536 (bc:total-pixels (random-cuts 256 256 4)))
)

#++
(test 'binpack-common)
