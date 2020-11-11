#++(ql:quickload '(binpack-test))
(defpackage #:binpack-test

  (:use :cl :parachute #:binpack-test/common)
  (:local-nicknames (:a :alexandria-2)
                    (:b :binpack)
                    (:bc :binpack/common)))
(in-package #:binpack-test)

(define-test binpack)




(defvar *tmp*)
(define-test (binpack packing1)
  ;; test evenly subdivided rects of various sizes
  (loop for i in '(2 16 256)
        do (loop for j in '(2 16 256)
                 do (loop for k in '(1 2 3 8)
                          for rects = (even-cuts i j k)
                          do (setf *tmp* rects)
                             (multiple-value-bind (pack ww hh)
                                 (b:pack rects i j)
                               (in-bounds pack i j)
                               (valid-packing pack)
                               (true (<= ww i))
                               (true (<= hh j)))))))
(define-test (binpack packing2)
  ;; test some random packings
  (let ((w 256)
        (h 256)
        (c 32))
   (loop for d in '(3 4 5 8)
         for a = 0
         do (loop for i below c
                  for rects = (random-cuts w h d i)
                  do (setf *tmp* rects)
                     (multiple-value-bind (pack maxw maxh)
                         (b:auto-pack rects )
                       (true (valid-packing pack))
                       (multiple-value-bind (pw ph)
                           (bc::rects-bounds pack)
                         (unless (< (* pw ph)
                                    (* 1.2 (* w h)))
                           (format t "~&packed into ~s x ~s (~s > ~s ~s)~%"
                                   pw ph (* pw ph)
                                   (* w h)
                                   (float (/ (* pw ph) (* w h)))
                                   ))
                         (incf a (float (/ (* pw ph) (* w h))))
                         (true (< (* pw ph)
                                  (* 1.6 (* w h)))))
                       ))
            (format t "~s: a = ~s ~s~%" d a (/ a c))
            (true (< (/ a c) 1.13)))))

#++
(time (test 'binpack))
