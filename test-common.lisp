#++ (ql:quickload '(binpack parachute))
;; common utilities for test suites
(defpackage binpack-test/common
  (:use :parachute :cl)
  (:local-nicknames (:a :alexandria)
                    (:bc :binpack/common))
  (:export
   #:even-cuts
   #:random-cuts
   #:valid-packing
   #:in-bounds))
(in-package binpack-test/common)

;; prng for generating test cases, so they don't depend on implementation
(defun seed (s)
  (let ((r s))
    ;; 2 values from splitmix64, xor together
    (labels ((\64 (x) (ldb (byte 64 0) x))
             (m (x)
               (setf x (\64 (* (logxor x (ash x -30)) #xBF58476D1CE4E5B9)))
               (setf x (\64 (* (logxor x (ash x -27)) #x94D049BB133111EB)))))
      (declare (inline \64))
      (setf r (logxor (m r)
                      (m (+ r #x9E3779B97f4A7C15))))
      (make-array () :element-type '(unsigned-byte 64)
                     :initial-element (ldb (byte 64 0) r)))))

(defun rng (s)
  (declare (optimize speed)
           (type (simple-array (unsigned-byte 64) 0)
                 s))
  ;; xorshift*
  (labels ((\64 (x)
             (ldb (byte 64 0) x))
           (r (s b)
             (logxor s (\64
                        (ash s b)))))
    (declare (inline \64 r))
    (let ((x (aref s)))
      (declare (type (unsigned-byte 64) x))
      (setf x (r x -12))
      (setf x (r x 25))
      (setf x (r x -27))
      (setf (aref s) x)
      (\64 (* x #x2545F4914F6CDD1D)))))

#++
(let ((s (seed 1)))
  (time (loop repeat 82000000 count (rng s))))



;;; test case generators used for both binpack and binpack2 tests

(defun even-cuts (w h wsteps &optional (hsteps wsteps))
  (let ((bw (max 1 (floor w wsteps)))
        (bh (max 1 (floor h hsteps))))
    (loop for i below (floor w bw)
          append (loop for j below (floor h bh)
                       collect (bc:rect* bw bh)))))

#++
(mapcar (a:rcurry 'bc::print-rect nil)
        (even-cuts 256 256 2 3))

(defun random-cuts (w h d &optional (seed 1))
  (let ((s (seed seed)))
    (labels ((c1 (r x)
               (1+ (floor (* (- x 2) r))))
             (c2 (r x)
               (- x (c1 r x)))
             (r (w h d)
               (if (or (zerop d)
                       (>= 2 w)
                       (>= 2 h))
                   (list (bc:rect* w h))
                   (let ((r (float (/ (rng s) (1- (expt 2 64))))))
                     (if (< r 0.5)
                         (append (r (c1 r w) h (1- d))
                                 (r (c2 r w) h (1- d)))
                         (append (r w (c1 r h) (1- d))
                                 (r w (c2 r h) (1- d))))))
               ))
      (r w h d))))

#++
(mapcar (a:rcurry 'bc::print-rect nil)
        (random-cuts 256 256 3 45))

(defun in-bounds (rects w h)
  (loop for r1 in rects
        for x = (bc:x r1)
        for y = (bc:y r1)
        when x
          do (true (<= 0 x))
             (true (<= 0 y))
             (true (<= (+ x (bc:w r1)) w))
             (true (<= (+ y (bc:h r1)) h)))
  t)

(defun valid-packing (rects)
  (loop for (r1 . rest) on rects
        for x = (bc:x r1)
        for y = (bc:y r1)
        for page = (bc:page r1)
        when x
        do (true y)
           (true page)
           (flet ((doesnt-intersect ()
                    (loop for r2 in rest
                          never (and (eql page (bc:page r2))
                                     (bc:intersectsp r1 r2)))))
             (true (doesnt-intersect))))
  t)
