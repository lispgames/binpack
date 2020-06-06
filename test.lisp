(defpackage #:binpack-test
  (:use :cl :parachute)
  (:local-nicknames (:b :binpack)))
(in-package #:binpack-test)

(define-test binpack)

(defun transform-points (p dx dy s)
  (loop for (x y) on p by #'cddr
        collect (* s (+ x dx))
        collect (* s (+ y dy))))



(defun dll-nth (d n)
  (loop with e = d repeat n do (setf e (b::dll-next e)) finally (return e)))

(defun dll-nth/prev (d n)
  (loop with e = d repeat n do (setf e (b::dll-prev e)) finally (return e)))

(defun dll-contents (d)
  (let ((r nil))
    (b::do-dll/next (n d)
      (push n r))
    (nreverse r)))

(define-test (binpack dll)
  (let ((a (finish (make-instance 'b::dll)))
        (b (finish (make-instance 'b::dll)))
        (c (finish (make-instance 'b::dll)))
        (d (finish (make-instance 'b::dll))))
    (finish (b::insert-after a b))
    (finish (b::insert-after b c))
    (finish (b::insert-after c d))
    (is eql 4 (b::dll-length a))
    (is eql 4 (b::dll-length b))
    (is eql 4 (b::dll-length c))
    (is eql 4 (b::dll-length d))
    (is eql b (b::dll-next a))
    (is eql c (b::dll-next b))
    (is eql d (b::dll-next c))
    (is eql a (b::dll-next d))
    (is eql d (b::dll-prev a))
    (is eql a (b::dll-prev b))
    (is eql b (b::dll-prev c))
    (is eql c (b::dll-prev d))
    (is eql c (dll-nth a 10))
    (is eql c (dll-nth/prev a 10))
    (is equalp
        (list d c b a)
        (let (l)
          (b::do-dll/next (i a)
            (push i l))
          l))
    (is equalp
        (list t d nil c nil b nil a)
        (let (l)
          (b::do-dll/next (i a end)
            (push i l)
            (push end l))
          l))
    (is equalp
        (list b c d a)
        (let (l)
          (b::do-dll/prev (i a)
            (push i l))
          l))
    (is equalp
        (list t b nil c nil d nil a)
        (let (l)
          (b::do-dll/prev (i a end)
            (push i l)
            (push end l))
          l))
    (let ((e (finish (b::delete-node b))))
      (is equalp c e)
      (is equalp a (b::dll-prev e))
      (is equalp d (b::dll-next e))
      (is equalp c (b::dll-next a))
      (is equalp a (b::dll-prev c))
      (is eql nil (b::dll-next b))
      (is eql nil (b::dll-prev b))
      (is = 3 (b::dll-length a))
      (is = 3 (b::dll-length c))
      (is = 3 (b::dll-length d))
      (is eql d (finish (b::delete-node c)))
      (is = 2 (b::dll-length a))
      (is eql a (finish (b::delete-node d)))
      (is = 1 (b::dll-length a))
      (is eql nil (b::delete-node a)))))

(define-test (binpack deq)
  (let ((deq (finish (b::make-deq))))
    (true (b::deq-empty-p deq))
    (false (b::top1 deq))
    (false (b::top2 deq))
    (finish (b::push1 1 deq))
    (is = 1 (b::top1 deq))
    (is = 1 (b::top2 deq))
    (is = 1 (b::dll-length (b::%deq-front deq)))
    (finish (b::pop1 deq))
    (true (b::deq-empty-p deq))
    (false (b::top1 deq))
    (false (b::top2 deq))
    (finish (b::push1 1 deq)) ;; 1
    (finish (b::push1 2 deq)) ;; 2 1
    (is = 1 (b::top2 deq))
    (is = 2 (b::top1 deq))
    (finish (b::push1 3 deq)) ;; 3 2 1
    (is = 1 (b::top2 deq))
    (is = 3 (b::top1 deq))
    (finish (b::push2 4 deq)) ;; 3 2 1 4
    (is = 4 (b::top2 deq))
    (is = 3 (b::top1 deq))
    (is = 4 (b::dll-length (b::%deq-front deq)))
    (is = 4 (b::dll-length (b::%deq-back deq)))
    (is equal '(3 2 1 4)
        (mapcar 'b::deq-v (dll-contents (b::%deq-front deq))))
    (finish (b::pop1 deq))
    (is = 4 (b::top2 deq))
    (is = 2 (b::top1 deq))
    (finish (b::pop2 deq))
    (is = 1 (b::top2 deq))
    (is = 2 (b::top1 deq))
    (finish (b::pop2 deq))
    (is = 2 (b::top2 deq))
    (is = 2 (b::top1 deq))
    (finish (b::pop2 deq))
    (true (b::deq-empty-p deq))
    (is eql nil (b::top2 deq))
    (is eql nil (b::top1 deq))
    (finish (progn
              (b::push1 1 deq)          ; 1
              (b::push2 2 deq)          ; 1 2
              (b::push2 3 deq)          ; 1 2 3
              (b::push1 4 deq)))        ; 4 1 2 3
    (is equal '(4 1 2 3)
        (mapcar 'b::deq-v (dll-contents (b::%deq-front deq))))
    (is eql 4 (b::top1 deq))
    (is eql 3 (b::top2 deq))
    (finish (b::pop2 deq)) ;; 4 1 2
    (is eql 4 (b::top1 deq))
    (is eql 2 (b::top2 deq))
    (finish (b::pop1 deq)) ;; 1 2
    (is eql 1 (b::top1 deq))
    (is eql 2 (b::top2 deq))
    (finish (b::pop1 deq))
    (finish (b::pop1 deq))
    (true (b::deq-empty-p deq))))


(define-test (binpack point)
  (let ((a (finish (b::make-point 1 2)))
        (b (finish (b::make-point 3 4))))
    (true (b::<=x a b))
    (true (b::<=y a b))
    (is = 2 (b::x (b::+x a 1)))
    (is = 0 (b::x (b::-x a 1)))
    (is = 3 (b::x (b::+x a 2)))
    (is = -1 (b::x (b::-x a 2)))
    (is = 7 (b::y (b::+y b 3)))
    (is = 1 (b::y (b::-y b 3)))))


(defvar *hole-fig4*
  '(0 3 0 4 1 4 1 5 2 5 2 6 3 6 3 5 4 5 4 7 6 7 6 6 7 6 7 5 5 5 5 4 7 4
    7 1 6 1 6 0 5 0 5 1 4 1 4 0 2 0 2 1 3 1 3 2 1 2 1 3))

(defvar *hole-fig6*
  '(0 0 0 1 2 1 2 2 3 2 3 3 -1 3 -1 4 -2 4 -2 5 1 5 1 6 7 6 7 7
    12 7 12 8 3 8 3 9 9 9 9 10 15 10 15 11 20 11 20 7 24 7 24 -6
    13 -6 13 -5 4 -5 4 -4 7 -4 7 -3 14 -3 14 -2 9 -2 9 4 5 4 5 -1
    2 -1 2 0))

(defvar *hole-fig6b*
  '(0 0 0 1 2 1 2 2 3 2 3 3 -1 3 -1 4 -2 4 -2 5 1 5 1 6 7 6 7 7
    12 7 12 8 3 8 3 9 9 9 9 10 15 10 15 11 20 11 20 7 24 7 24 -6
    18 -6 18 5 17 5 17 -6
    13 -6 13 -5 4 -5 4 -4 7 -4 7 -3 14 -3 14 -2 9 -2 9 4 5 4 5 -1
    2 -1 2 0))


(defvar *sh-test1*
  #++'(0 0 0 16 32 16 32 32 -16 32 -16 48 -32 48 -32 64 16 64 16 80 64 80
       64 64 80 64 80 0)
  '(0 0 0 1 2 1 2 2 -1 2 -1 3 -2 3 -2 4 1 4 1 5 4 5 4 4 5 4 5 0))
(defvar *sh-test1b*
  ;; same thing without falling edge
  #++'(0 0 0 16 32 16 32 32 -16 32 -16 48
       -32 48 -32 64 16 64 16 80 80 80 80 0)
  '(0 0 0 1 2 1 2 2 -1 2 -1 3 -2 3 -2 4 1 4 1 5 5 5 5 0))
(defvar *hole-box*
  #++'(0 0 0 16 8 16 8 0)
  '(0 0 0 2 1 2 1 0))

#++
(graph-paper::add-shape-points *sh-test1*)
(defun fe-xy (n fe)
  (list (b::x (aref fe n)) (b::y (aref fe n))))

(defmacro test-hole (points width height
                     (sh-top sh-bottom sh-w sh-h)
                     &rest more-subholes)
  (let ((subholes (list* (list sh-top sh-bottom sh-w sh-h)
                         more-subholes)))
    (print
     `(let* ((h (finish (b::%make-hole-from-points ,points)))
             (sh (finish (b::h-subholes h))))
        (is eql h (b::dll-next h))
        (is eql h (b::dll-prev h))
        (is eql 1 (b::dll-length h))
        (is eql ,(length subholes) (b::dll-length (b::h-subholes h)))
        ,@(loop for (sh-top sh-bottom sh-w sh-h) in subholes
                append `((is equalp ',sh-bottom
                             (fe-xy 0 (b::f-d (b::sh-top sh))))
                         (is equalp ',sh-top
                             (fe-xy 0 (b::f-h (b::sh-top sh))))
                         (is equalp ',sh-bottom
                             (fe-xy 0 (b::f-d (b::sh-bottom sh))))
                         (is equalp ',sh-top
                             (fe-xy 0 (b::f-h (b::sh-bottom sh))))
                         (is eql ,sh-w (b::sh-max-width sh))
                         (is eql ,sh-h (b::sh-max-height sh))
                         (finish (b::check-subhole sh))
                         (finish (setf sh (b::dll-next sh)))))

        (is eql ,width (b::ht-max-width h))
        (is eql ,height (b::ht-max-height h))))))

(define-test (binpack hole1)
  (test-hole *hole-box* 1 2
      ((0 2) (0 0) 1 2))
  (test-hole *sh-test1* 7 5
      ((0 1) (0 0) 5 5)
      ((-2 4) (-2 3) 7 3)))


(define-test (binpack hole2)
  (let* ((a (b::make-hole-vertex 0 0))
         (b (b::make-hole-vertex 0 16 a))
         (c (b::make-hole-vertex 8 16 b))
         (d (b::make-hole-vertex 8 0 c))
         (h (b::make-hole d)))
    (is eql h (b::dll-next h))
    (is eql h (b::dll-prev h))
    (is eql 1 (b::dll-length h))
    (is equalp '(0 0) (fe-xy 0 (b::f-d (b::sh-top (b::h-subholes h)))))
    (is equalp '(0 16) (fe-xy 0 (b::f-h (b::sh-top (b::h-subholes h)))))
    (is equalp '(0 0) (fe-xy 0 (b::f-d (b::sh-bottom (b::h-subholes h)))))
    (is equalp '(0 16) (fe-xy 0 (b::f-h (b::sh-bottom (b::h-subholes h)))))
    (is eql 1 (b::dll-length (b::h-subholes h)))
    (is eql 8 (b::ht-max-width h))
    (is eql 16 (b::ht-max-height h))))


(defun make-d* (l end falling points)
  (let ((ft (apply
             #'make-instance
             'b::f-edge
             (loop for (ax y1 y2 g gy) in points
                   collect g into gaps
                   collect gy into gap-y
                   collect (b::make-point ax (min y1 y2)) into d
                   collect (b::make-point ax (max y1 y2)) into h
                   finally (return (list :d (coerce d 'vector)
                                         :h (coerce h 'vector)
                                         :gap (coerce gaps 'vector)
                                         :gap-y (coerce gap-y 'vector)))))))
    (b::make-d ft l end falling)))

(defparameter *subhole-fts*
  #(;; top of sh-test1
    (2 (-2 3 4)
     (1 4 5)
     (4 5 4)
     (5 4 0))
    ;; bottom of sh-test1
    (5 (0 0 1)
     (2 1 5 3 2)
     (4 5 4)
     (5 4 0))
    ;; top of sh-test1b
    (2
     (-2 3 4)
     (1 4 5)
     (5 0 5))
    ;; bottom of sh-test1
    (5
     (0 0 1)
     (2 1 5 3 2)
     (5 0 5))))

(defun check-d (index l spans gaps)
  (format t "check-d ~s~%~s~%" index spans)
  (destructuring-bind (end &rest ft)
      (aref *subhole-fts* index)
    (binpack2-vis::clear-shapes)
    (binpack2-vis::with-polyline (:rgba '(1 0 1 1) :close nil)
      (loop for (x y1 y2) in ft
            do (binpack2-vis::add-shape-point (* x 16) (* y1 16))
               (binpack2-vis::add-shape-point (* x 16) (* y2 16))))
    (let ((falling (loop for y = nil then (max y1 y2)
                         for (x y1 y2) in ft
                         count (and y (<= (max y1 y2) y))))
          (count (length spans)))
      (true (<= 1 falling 2))
      #++(format t "falling = ~s~%" falling)
      (let* ((d (finish (make-d* l end (= falling 2) ft)))
             (gaps2))
        (loop  for (a b) on (coerce d 'list) by #'cddr
               for x1 = (b::x a)
               for y1 = (b::y a)
               for x2 = (b::x b)
               for y2 = (b::y b)
               do (binpack2-vis::with-polyline (:rgba '(1 1 0 1) :close nil)
                    (binpack2-vis::add-shape-point (1+ (* x1 16)) (1+ (* y1 16)))
                    (binpack2-vis::add-shape-point (1+ (* x2 16)) (1+ (* y2 16)))))
        #++(format t "d = ~s~%" d)
        ;; 2 vertices per expected entry
        (is = (* 2 count) (length d))
        ;; both with same y value, and with 2nd right of first
        (loop for b1 = nil then b
              for (a b) on (coerce d 'list) by #'cddr
              for i from 0
              for (sx sy sl) = (pop spans)
              do (true b)
              when b
                do (true (= (b::y a) (b::y b)))
                   (is = sy (b::y a))
                   (is = sx (b::x a))
                   (is = sl (- (b::x b) (b::x a)))
                   ;; segment can have 0 length
                   (true (<= (b::x a) (b::x b)))
                   ;; and right of previous segment if any
                   (when b1
                     (is >= (b::x a) (b::x b1)))
                   (when (and (b::point-gap-p a)
                              (b::gap a))
                     (push i gaps2)))
        (is equalp (sort gaps '<) (sort gaps2 '<))))))

#++
(test 'make-d :report 'interactive)

(define-test (binpack make-d)
  ;; test1 top
  (check-d 0 2 '((-2 4 3) (1 5 1) (2 4 3)) nil)
  ;; test1 bottom
  (check-d 1 2 '((0 1 2) (2 5 0) (2 4 3)) '(1))
  ;; test1b top (no falling edge)
  (check-d 2 2 '((-2 4 3) (1 5 4)) nil)
  ;; test1b bottom
  (check-d 3 2 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 3 '((-2 4 3) (1 5 0) (1 4 4)) nil)
  (check-d 1 3 '((0 1 2) (2 4 3)) '(1))
  (check-d 2 3 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 3 '((0 1 2) (2 5 3)) '(1))

  ;; first gap is too wide, but will be rejected by lower edge
  (check-d 0 4 '((-2 4 3) (1 4 4)) nil)
  ;; spans overlap, but extra will be ignored by other steps?
  (check-d 1 4 '((0 1 1)) '())
  ;; non-falling edge has extra invalid placements at end
  (check-d 2 4 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 4 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 5 '((-2 4 2)) nil)
  (check-d 1 5 '((0 1 0)) '())
  (check-d 2 5 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 5 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 6 '((-2 4 1)) nil)
  (check-d 1 6 '() '())
  (check-d 2 6 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 6 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 7 '((-2 4 0)) nil)
  (check-d 1 7 '() '())
  (check-d 2 7 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 7 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 8 '() nil)
  (check-d 1 8 '() '())
  (check-d 2 8 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 8 '((0 1 2) (2 5 3)) '(1))
  ;; todo: more test cases
  )

(defun make-c* (l end points)
  (let ((fb (apply
             #'make-instance
             'b::f-edge
             (loop for (ax y1 y2 g) in points
                   collect g into gaps
                   collect (b::make-point ax (min y1 y2)) into d
                   collect (b::make-point ax (max y1 y2)) into h
                   finally (return (list :d (coerce d 'vector)
                                         :h (coerce h 'vector)
                                         :gap (coerce gaps 'vector)))))))
    (b::make-c fb l end)))



(defparameter *subhole-fbs*
  #(;; top of sh-test1
    (2
     (-2 3 4)
     (-1 2 3)
     (5 2 4))
    ;; bottom of sh-test1
    (5
     (0 0 1)
     (5 4 0))
    ;; subholes of fig6b (offset by -11, -2 to match debug render)
    (3
     (-11 -2 -1)
     (-9 -3 -2)
     (-6 -3 2)
     (-2 -4 2)
     (6 -4 3)
     (13 3 5))
    (13
     (-7 -7 -6)
     (2 -8 -7)
     (6 -8 3)
     (7 -8 3)
     (13 -8 5))
    (1
     (-8 6 7)
     (9 6 8))
    (-8
     (-13 2 3)
     (-12 1 2)
     (-6 1 2)
     (6 2 3)
     (13 3 4))))

(defun check-c (index l spans)
  (format t "~&*~%*~%check-c ~s = ~s~%" index spans)
  (destructuring-bind (end &rest ft)
      (aref *subhole-fbs* index)
    (format t "fb = ~s~%" ft)
    (binpack2-vis::clear-shapes)
    (binpack2-vis::with-polyline (:rgba '(1 0 1 1) :close nil)
      (loop for (x y2 y1) in ft
            do (binpack2-vis::add-shape-point (* x 16) (* y1 16))
               (binpack2-vis::add-shape-point (* x 16) (* y2 16))))
    (let ((count (length spans)))
      (let* ((c (finish (make-c* l end ft))))
        (format t "c = ~s~%" c)
        (loop  for (a b) on (coerce c 'list) by #'cddr
               for x1 = (b::x a)
               for y1 = (b::y a)
               for x2 = (b::x b)
               for y2 = (b::y b)
               do (binpack2-vis::with-polyline (:rgba '(1 1 0 1) :close nil)
                    (binpack2-vis::add-shape-point (1+ (* x1 16)) (1+ (* y1 16)))
                    (binpack2-vis::add-shape-point (1+ (* x2 16)) (1+ (* y2 16)))))
        ;; 2 vertices per expected entry
        (is = (* 2 count) (length c))
        ;; both with same y value, and with 2nd right of first
        (loop for b1 = nil then b
              for (a b) on (coerce c 'list) by #'cddr
              for i from 0
              for (sx sy sl) = (pop spans)
              do (true b)
              when b
                do (true (= (b::y a) (b::y b)))
                   (is = sy (b::y a))
                   (is = sx (b::x a))
                   (is = sl (- (b::x b) (b::x a)))
                   ;; segment can have 0 length
                   (true (<= (b::x a) (b::x b)))
                   ;; and right of previous segment if any
                   (when b1
                     (is >= (b::x a) (b::x b1)))
                   (false (and (b::point-gap-p a) (b::gap a))))))))

(define-test (binpack make-c)
  ;; currently C extends past left side of hole
  ;; test1 top
  (check-c 0 1 '((-2 3 1) (-1 2 5)))
  ;; test1 bottom
  (check-c 1 1 '((0 0 4)))

  (check-c 0 2 '((-2 3 1) (-1 2 4)))
  (check-c 1 2 '((0 0 3)))

  (check-c 0 3 '((-2 3 1) (-1 2 3)))
  (check-c 1 3 '((0 0 2)))

  (check-c 0 4 '((-2 3 1) (-1 2 2)))
  (check-c 1 4 '((0 0 1)))

  (check-c 0 5 '((-2 3 1) (-1 2 1)))
  (check-c 1 5 '((0 0 0)))

  (check-c 0 6 '((-2 3 1) (-1 2 0)))
  (check-c 1 6 '())

  (check-c 0 7 '((-2 3 0)))
  (check-c 1 7 '())

  (check-c 0 8 '())
  (check-c 1 8 '())

  ;; fig6b

  (check-c 2 1 '((-11 -2 2) (-9 -3 2) (-7 2 5) (-2 -4 7)))
  (check-c 3 1 '((-7 -7 9) (2 -8 3) (5 3 2) (7 -8 5)))
  (check-c 4 1 '((-8 6 16)))
  (check-c 5 1 '((-13 2 1) (-12 1 5)))

  (check-c 2 2 '((-11 -2 2) (-9 -3 1) (-8 2 6) (-2 -4 6)))
  (check-c 3 2 '((-7 -7 9) (2 -8 2) (4 3 3) (7 -8 4)))
  (check-c 4 2 '((-8 6 15)))
  (check-c 5 2 '((-13 2 1) (-12 1 4)))

  (check-c 2 3 '((-11 -2 2) (-9 -3 0) (-9 2 7) (-2 -4 5)))
  (check-c 3 3 '((-7 -7 9) (2 -8 1) (3 3 4) (7 -8 3)))
  (check-c 4 3 '((-8 6 14)))
  (check-c 5 3 '((-13 2 1) (-12 1 3) (-9 2 12)))

  (check-c 2 4 '((-11 -2 1) (-10 2 8) (-2 -4 4) (2 3 7)))
  (check-c 3 4 '((-7 -7 9) (2 -8 0) (2 3 5) (7 -8 2)))
  (check-c 4 4 '((-8 6 13)))
  (check-c 5 4 '((-13 2 1) (-12 1 2) (-10 2 12)))

  (check-c 2 5 '((-11 -2 0) (-11 2 9) (-2 -4 3) (1 3 7)))
  (check-c 3 5 '((-7 -7 8) (1 3 6) (7 -8 1)))
  (check-c 4 5 '((-8 6 12)))
  (check-c 5 5 '((-13 2 1) (-12 1 1) (-11 2 12)))

  (check-c 2 6 '((-11 2 9) (-2 -4 2) (0 3 7)))
  (check-c 3 6 '((-7 -7 7) (0 3 7) (7 -8 0)))
  (check-c 4 6 '((-8 6 11)))
  (check-c 5 6 '((-13 2 1) (-12 1 0) (-12 2 12)))

  (check-c 2 7 '((-11 2 9) (-2 -4 1) (-1 3 7)))
  (check-c 3 7 '((-7 -7 6) (-1 3 7)))
  (check-c 4 7 '((-8 6 10)))
  (check-c 5 7 '((-13 2 7)))

  (check-c 2 8 '((-11 2 9) (-2 -4 0) (-2 3 7)))
  (check-c 3 8 '((-7 -7 5) (-2 3 7)))
  (check-c 4 8 '((-8 6 9)))
  (check-c 5 8 '((-13 2 7)))

  (check-c 2 9 '((-11 2 8) (-3 3 7)))
  (check-c 3 9 '((-7 -7 4) (-3 3 7)))
  (check-c 4 9 '((-8 6 8)))
  (check-c 5 9 '((-13 2 7)))

  (check-c 2 10 '((-11 2 7) (-4 3 7)))
  (check-c 3 10 '((-7 -7 3) (-4 3 7)))
  (check-c 4 10 '((-8 6 7)))
  (check-c 5 10 '((-13 2 7)))

  (check-c 2 11 '((-11 2 6) (-5 3 7)))
  (check-c 3 11 '((-7 -7 2) (-5 3 7)))
  (check-c 4 11 '((-8 6 6)))
  (check-c 5 11 '((-13 2 7)))

  (check-c 2 12 '((-11 2 5) (-6 3 7)))
  (check-c 3 12 '((-7 -7 1) (-6 3 7)))
  (check-c 4 12 '((-8 6 5)))
  (check-c 5 12 '((-13 2 7)))

  (check-c 2 13 '((-11 2 4) (-7 3 7)))
  (check-c 3 13 '((-7 -7 0) (-7 3 7)))
  (check-c 4 13 '((-8 6 4)))
  (check-c 5 13 '((-13 2 6)))

  (check-c 2 14 '((-11 2 3) (-8 3 7)))
  (check-c 3 14 '((-7 3 6)))
  (check-c 4 14 '((-8 6 3)))
  (check-c 5 14 '((-13 2 5)))

  (check-c 2 15 '((-11 2 2) (-9 3 7)))
  (check-c 3 15 '((-7 3 5)))
  (check-c 4 15 '((-8 6 2)))
  (check-c 5 15 '((-13 2 4) (-9 3 7)))

  (check-c 2 16 '((-11 2 1) (-10 3 7)))
  (check-c 3 16 '((-7 3 4)))
  (check-c 4 16 '((-8 6 1)))
  (check-c 5 16 '((-13 2 3) (-10 3 7)))

  (check-c 2 17 '((-11 2 0) (-11 3 7)))
  (check-c 3 17 '((-7 3 3)))
  (check-c 4 17 '((-8 6 0)))
  (check-c 5 17 '((-13 2 2) (-11 3 7)))

  (check-c 2 18 '((-11 3 6)))
  (check-c 3 18 '((-7 3 2)))
  (check-c 4 18 'NIL)
  (check-c 5 18 '((-13 2 1) (-12 3 7)))

  (check-c 2 19 '((-11 3 5)))
  (check-c 3 19 '((-7 3 1)))
  (check-c 4 19 'NIL)
  (check-c 5 19 '((-13 2 0) (-13 3 7)))

  (check-c 2 20 '((-11 3 4)))
  (check-c 3 20 '((-7 3 0)))
  (check-c 4 20 'NIL)
  (check-c 5 20 '((-13 3 6)))

  (check-c 2 21 '((-11 3 3)))
  (check-c 3 21 'NIL)
  (check-c 4 21 'NIL)
  (check-c 5 21 '((-13 3 5)))

  (check-c 2 22 '((-11 3 2)))
  (check-c 3 22 'NIL)
  (check-c 4 22 'NIL)
  (check-c 5 22 '((-13 3 4)))

  (check-c 2 23 '((-11 3 1)))
  (check-c 3 23 'NIL)
  (check-c 4 23 'NIL)
  (check-c 5 23 '((-13 3 3)))

  (check-c 2 24 '((-11 3 0)))
  (check-c 3 24 'NIL)
  (check-c 4 24 'NIL)
  (check-c 5 24 '((-13 3 2)))

  (check-c 2 25 'NIL)
  (check-c 3 25 'NIL)
  (check-c 4 25 'NIL)
  (check-c 5 25 '((-13 3 1)))

  (check-c 2 26 'NIL)
  (check-c 3 26 'NIL)
  (check-c 4 26 'NIL)
  (check-c 5 26 '((-13 3 0)))

  (check-c 2 27 'NIL)
  (check-c 3 27 'NIL)
  (check-c 4 27 'NIL)
  (check-c 5 27 'NIL))


#++
(loop with a = #2a((1x1 2x1 3x1)
                   (1x2 2x2 3x2))
      for w from 1 below 4
      do (loop for h from 1 below 4
               when (array-in-bounds-p a (1- w) (1- h))
                 do (format t "w=~s,h=~s, a=~s~%" w h (aref a (1- w) (1- h)))))

(defparameter *placing-tests*
  ;; list of x y, and 2d array of # of times something with dimension
  ;; WxH should fit in element W-1,H-1 (no 0 dimension), so array is
  ;;
  ;; #2a((1x1 1x2 1x3)
  ;;     (2x1 2x2 2x3)) etc
  ;;
  ;; 0
  #(((-6 -5 -6 -1 -4 -1 -4 -2 1 -2 1 -3 -2 -3 -2 -4 -5 -4 -5 -5)
     #2a ((1 1 1 1 0)
          (1 1 1 0 0)
          (1 1 0 0 0)
          (1 1 0 0 0)
          (1 0 0 0 0)
          (1 0 0 0 0)
          (0 0 0 0 0)))
    ((-14 -5 -14 -1 -7 -1 -7 -2 -9 -2 -9 -3 -11 -3 -11 -4 -13 -4 -13 -5)
     #2a ((1 1 1 1 0)
          (1 1 1 0 0)
          (1 1 0 0 0)
          (1 0 0 0 0)
          (1 0 0 0 0)
          (1 0 0 0 0)
          (1 0 0 0 0)
          (0 0 0 0 0)))
    ((-16 -5 -17 -5 -17 -4 -20 -4 -20 -3 -22 -3 -22 -2 -23 -2 -23 -1 -16 -1)
     #2a ((4 3 2 1 0)
          (2 2 2 0 0)
          (2 2 1 0 0)
          (2 2 1 0 0)
          (1 1 0 0 0)
          (1 1 0 0 0)
          (0 0 0 0 0)))
    ((19 12 20 12 20 8 19 8)
     #2a ((1 1 1 1 0)
          (0 0 0 0 0)))
    ((16 12 17 12 17 9 16 9)
     #2a ((1 1 1 0)
          (0 0 0 0)))
    ;; 5
    ((14 12 14 10 13 10 13 12)
     #2a ((1 1 0)
          (0 0 0)))
    ((6 12 10 12 10 11 6 11)
     #2a ((1 0)
          (1 0)
          (1 0)
          (1 0)
          (0 0)))
    ((0 12 3 12 3 11 0 11)
     #2a ((1 0)
          (1 0)
          (1 0)
          (0 0)))
    ((-4 12 -2 12 -2 11 -4 11)
     #2a ((1 0)
          (1 0)
          (0 0)))
    ((-10 12 -6 12 -6 8 -10 8)
     #2a ((1 1 1 1 0)
          (1 1 1 1 0)
          (1 1 1 1 0)
          (1 1 1 1 0)
          (0 0 0 0 0)))
    ;; 10
    ((-15 12 -12 12 -12 9 -15 9)
     #2a ((1 1 1 0)
          (1 1 1 0)
          (1 1 1 0)
          (0 0 0 0)))
    ((-19 12 -17 12 -17 10 -19 10)
     #2a ((1 1 0)
          (1 1 0)
          (0 0 0)))
    ((-22 12 -21 12 -21 11 -22 11)
     #2a ((1 0)
          (0 0)))

    ((11 4 14 4 14 6 19 6 19 2 16 2 16 4 15 4 15 2 13 2 13 3 11 3)
     #2a ((3 2 1 1 0)
          (4 3 1 1 0)
          (3 2 1 1 0)
          (2 1 0 0 0)
          (1 1 0 0 0)
          (0 0 0 0 0)))
    ((1 3 3 3 3 4 4 4 4 6 9 6 9 2 6 2 6 4 5 4 5 2 1 2)
     #2a ((2 2 1 1 0)
          (3 3 1 1 0)
          (2 2 1 1 0)
          (1 1 0 0 0)
          (1 1 0 0 0)
          (0 0 0 0 0)))
    ;; 15
    ((-8 5 -3 5 -3 2 -6 2 -6 4 -8 4)
     #2a ((2 1 1 0)
          (2 1 1 0)
          (2 1 1 0)
          (1 0 0 0)
          (1 0 0 0)))
    ((-15 4 -13 4 -13 5 -10 5 -10 2 -13 2 -13 3 -15 3)
     #2a ((2 1 1 0)
          (2 1 1 0)
          (2 1 1 0)
          (1 0 0 0)
          (1 0 0 0)))
    ((-22 2 -22 3 -20 3 -20 5 -17 5 -17 2)
     #2a ((1 1 1 0)
          (1 1 1 0)
          (1 1 1 0)
          (1 0 0 0)
          (1 0 0 0)))))

(defun test-placing (points)
  (let* ((*standard-output* (make-broadcast-stream))
         (x1 (loop for x in points by 'cddr minimize x))
         (y1 (loop for y in (cdr points) by 'cddr minimize y))
         (x2 (loop for x in points by 'cddr maximize x))
         (y2 (loop for y in (cdr points) by 'cddr maximize y))
         (w1 (- x2 x1))
         (h1 (- y2 y1))
         (hole (finish
                (b::%make-hole-from-points
                 (print (transform-points (print points)
                                             (print (- x1))
                                             (print(- y1)) 1)
                        *debug-io*))))
         (ref (make-hash-table :test 'equalp))
         (place (make-hash-table :test 'equalp)))
    (format *debug-io* "~%~%testing ~s @ ~s ~s~%" points (- x1) (- y1))

    (finish
     (time
      (loop
        for w from 1 to (+ w1 2)
        do (loop
             for h from 1 to (+ h1 2)
             for p = (b::find-all-placements hole w h)
             do (loop
                  for x from -1 upto w1
                  do (loop for y from -1 upto h1
                           for v = (b::valid-placement-p hole w h x y)
                           when v
                             do (setf (gethash (list x y w h) ref) t)))
                (loop for i in p
                      for x = (b::x i)
                      for y = (b::y i)
                      for l = (list x y w h)
                      do ;; ref implementation also found it
                         (true (gethash l ref))
                         ;; didn't duplicate it
                         (false (gethash l place))
                         (setf (gethash l place) t))))))
    ;; make sure we found everything the ref found
    (loop for i in (alexandria:hash-table-keys ref)
          always (true (gethash i place)))))

(define-test (binpack placing)
  (loop for x across *placing-tests*
        for i from 0
        do (format t "testing ~s~%" i)
           (test-placing (car x)))
  #++(test-placing *hole-fig4*) ;; fig 4 is an invalid hole, don't test it
  (test-placing *hole-fig6*)
  (test-placing *hole-fig6b*)
  (test-placing *sh-test1*)
  (test-placing *sh-test1b*)
  (test-placing *hole-box*))
#++
(time
 (test 'binpack))



#++
(test 'placing :report 'interactive)

#++
(untrace)


#++
(do-dll/next
    (v
     (h-vertices
      (make-hole
       (loop for (x y) on *hole-fig6*
             by 'cddr
             for v = (make-hole-vertex x y nil) then (make-hole-vertex x y v)
             finally (return v)))))
  (format t "~&~s: ~s, ~s ~s (~s) (~s) ~%" v (hv-classify v) (hv-x v) (hv-y v)
          (hv-n v) (hv-w v)))

#++
(b::do-dll/next (v (b::h-vertices(b::init-hole 256 256)))
  (format t "~&~s: ~s, ~s~%" v (b::hv-x v) (b::hv-y v)))
#++
(do-dll/prev (v (init-hole 256 256) end)
  (format t "~&~s: ~s, ~s ~s~%" v (hv-x v) (hv-y v) end))

(setf *print-circle* t)
#++
(init-hole 256 256)

#++(ql:quickload '(binpack parachute))

