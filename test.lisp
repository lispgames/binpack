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
      (is eql nil (b::delete-node a))))


  (let ((a (finish (make-instance 'b::dll)))
        (b (finish (make-instance 'b::dll)))
        (c (finish (make-instance 'b::dll)))
        (d (finish (make-instance 'b::dll))))
    (finish (b::insert-before a b))
    (finish (b::insert-before b c))
    (finish (b::insert-before c d))
    (is eql 4 (b::dll-length a))
    (is eql 4 (b::dll-length b))
    (is eql 4 (b::dll-length c))
    (is eql 4 (b::dll-length d))
    (is eql b (b::dll-prev a))
    (is eql c (b::dll-prev b))
    (is eql d (b::dll-prev c))
    (is eql a (b::dll-prev d))
    (is eql d (b::dll-next a))
    (is eql a (b::dll-next b))
    (is eql b (b::dll-next c))
    (is eql c (b::dll-next d))
    (is equalp
        (list b c d a)
        (let (l)
          (b::do-dll/next (i a)
            (push i l))
          l))))

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
        (binpack2-vis::clear-shapes)
        (binpack2-vis::add-shape-points ,points :close nil)
        (print ,points)
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
      ((-2 4) (-2 3) 7 3)
      ((0 1) (0 0) 5 5)))

#++
(test 'hole1 :report 'interactive)


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
                   collect (when g (list gy g)) into gaps
                   collect (b::make-point ax (min y1 y2)) into d
                   collect (b::make-point ax (max y1 y2)) into h
                   finally (return (list :d (coerce d 'vector)
                                         :h (coerce h 'vector)
                                         :gaps (coerce gaps 'vector)))))))
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

(defun brute-force-d (l end falling ft)
  (when (> (+ l (caar ft))
           (caar (last ft)))
    (return-from brute-force-d #()))
  (unless falling
    (return-from
     brute-force-d
      (let ((r (coerce
                (loop for first = t then nil
                      for ((ax y1 y2 g gy) . more) on ft
                      unless first
                        collect (b::make-point ax
                                               (if more
                                                   (min y1 y2)
                                                   (max y1 y2)))
                      when more
                        collect (b::make-point-gap ax (max y1 y2)
                                                   (when gy (list gy g))))
                'vector)))
        (format t " nf: ~s~%"
                (map 'list #'b::pp r))
        r)))
  (let* ((ft (coerce ft 'vector))
         (r (1- (length ft)))
         (f (1- r))
         (re (aref ft r))
         (fe (aref ft f))
         (lsx nil)
         (lsn nil)
         (fex (first fe))
         (fey (third fe))
         (rex (first re)))
    (coerce
     (append
      (loop for i below f
            for (x1 y1 y2 g gy) = (aref ft i)
            for (x2) = (aref ft (1+ i))
            for x3 = (if (>= y2 fey)
                         (- fex l)
                         (- rex l))
            #+do (format t "~s,~s,~s / ~s, ~s,~s~%"
                         x1 x2 x3 y1 y2 fey)
            when (> x1 (- rex l))
              ;; no more spans fit
              do (loop-finish)

            when (and (= (+ x1 l) rex)
                      (<= y2 fey))
              ;; exact fit under falling edge (not sure this is needed?)
              append (list (b::make-point-gap x1 (min y2 fey)
                                              (when gy (list gy g)))
                           (b::make-point rex (min y2 fey)))
              and do
            #++(format t"lsx -> nil 1~%")
               (setf lsx nil)
               (loop-finish)
            when (and (<= x1 x3)
                      (< x3  x2)
                      (= y2 fey))
              ;; spans gap before falling edge
              collect (b::make-point-gap x1 y2 (when gy (list gy g)))
              and collect (b::make-point rex y2)
              and do (setf lsx nil)
            #++(format t "lsx -> nil 2~%")
               (loop-finish)
            when (and (>= x3 x1)
                      (<= x1 end))
              #++(or (< (+ x1 l) fex)
                     (and (< y2 fey)
                          (< (+ x1 l) rex)))
            collect (b::make-point-gap x1 y2 (when gy (list gy g)))
            and collect (b::make-point (min x3 x2 end) y2)
            when (and (< y1 fey) (>= y2 fey))
              do (assert (not lsx))
                 (setf lsx x1)
            #++(format t "lsx -> ~s~%" lsx)
               (setf lsn (aref ft i)))
      (cond
        ((and lsx (< lsx (- fex l)))
         #++ (format t "lsx = ~s, fex = ~s, l = ~s~%" lsx fex l)
         (list (b::make-point-open (- fex l) fey)
               (b::make-point rex fey)))
        (lsx
         #++ (format t "lsx = ~s, fex = ~s, l = ~s, lsn = ~s~%" lsx fex l
                     (cdddr lsn))
         (list (b::make-point-gap lsx fey
                                  (when (< 3 (length lsn))
                                    (reverse (cdddr lsn))))
               (b::make-point rex fey)))))
     'vector)))

(defun check-d (index l spans gaps)
  (format t "~&~%check-d ~s~%~s~%" index spans)
  (destructuring-bind (end &rest ft)
      (aref *subhole-fts* index)
    (binpack2-vis::clear-shapes)
    (binpack2-vis::with-polyline (:rgba '(1 0 1 1) :close nil)
      (loop for (x y1 y2) in ft
            do (binpack2-vis::add-shape-point (* x 1) (* y1 1))
               (binpack2-vis::add-shape-point (* x 1) (* y2 1))))
    (let ((falling (loop for y = nil then (max y1 y2)
                         for (x y1 y2) in ft
                         count (and y (<= (max y1 y2) y))))
          (count (length spans)))
      (true (<= 1 falling 2))
      #++(format t "falling = ~s~%" falling)
      (let* ((d (finish (make-d* l end (= falling 2) ft)))
             (bd (finish (brute-force-d l end (= falling 2) ft)))
             (gaps2))
        (loop  for (a b) on (coerce d 'list) by #'cddr
               for x1 = (b::x a)
               for y1 = (b::y a)
               for x2 = (b::x b)
               for y2 = (b::y b)
               do (binpack2-vis::with-polyline (:rgba '(1 1 0 1) :close nil)
                    (binpack2-vis::add-shape-point (+ (* x1 1) 0.1)
                                                   (- (* y1 1) 0.25))
                    (binpack2-vis::add-shape-point (- (* x2 1) 0.1)
                                                   (- (* y2 1) 0.25))))
        (loop  for (a b) on (coerce bd 'list) by #'cddr
               for x1 = (b::x a)
               for y1 = (b::y a)
               for x2 = (b::x b)
               for y2 = (b::y b)
               do (binpack2-vis::with-polyline (:rgba '(0 1 1 1) :close nil)
                    (binpack2-vis::add-shape-point (+ (* x1 1) 0.1)
                                                   (- (* y1 1) 0.35))
                    (binpack2-vis::add-shape-point (- (* x2 1) 0.1)
                                                   (- (* y2 1) 0.35))))
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
                              (b::gaps a))
                     (push i gaps2)))
        (is equalp (sort gaps '<) (sort gaps2 '<))))))

(defun check-d2 (ft end l)
  #++(format t "~&~%check-d2 ~s, ~s : ~s~%"end l ft)
  (binpack2-vis::clear-shapes)
  (binpack2-vis::with-polyline (:rgba '(1 0 1 1) :close nil)
    (loop for (x y1 y2) in ft
          do (binpack2-vis::add-shape-point (* x 1) (* y1 1))
             (binpack2-vis::add-shape-point (* x 1) (* y2 1))))
  (let ((falling (loop for y = nil then (max y1 y2)
                       for (x y1 y2) in ft
                       count (and y (<= (max y1 y2) y)))))
    (true (<= 1 falling 2))
    #++(format t "falling = ~s~%" falling)
    (let* ((d (finish (make-d* l end (= falling 2) ft)))
           (bd (finish (brute-force-d l end (= falling 2) ft))))
      (loop  for (a b) on (coerce d 'list) by #'cddr
             for x1 = (b::x a)
             for y1 = (b::y a)
             for x2 = (b::x b)
             for y2 = (b::y b)
             do (binpack2-vis::with-polyline (:rgba '(1 0 0 1) :close nil)
                  (binpack2-vis::add-shape-point (+ (* x1 1) 0.1)
                                                 (- (* y1 1) 0.25))
                  (binpack2-vis::add-shape-point (- (* x2 1) 0.1)
                                                 (- (* y2 1) 0.25))))
      (loop  for (a b) on (coerce bd 'list) by #'cddr
             for x1 = (b::x a)
             for y1 = (b::y a)
             for x2 = (b::x b)
             for y2 = (b::y b)
             do (binpack2-vis::with-polyline (:rgba '(0 1 1 1) :close nil)
                  (binpack2-vis::add-shape-point (+ (* x1 1) 0.1)
                                                 (- (* y1 1) 0.35))
                  (binpack2-vis::add-shape-point (- (* x2 1) 0.1)
                                                 (- (* y2 1) 0.35))))
      #++(format t "d = ~s~%" d)
      ;; same # of vertices from both
      (is = (length bd) (length d))
      ;; 2 vertices per entry
      (is = 0 (mod (length d) 2))
      ;; both with same y value, and with 2nd right of first
      (loop for b1 = nil then b
            for (a b) on (coerce d 'list) by #'cddr
            for i from 0
            for (a2 b2) on (coerce bd 'list) by #'cddr
            do (true b)
            when b
              do (true (= (b::y a) (b::y b)))
                 (true (= (b::y a2) (b::y b2)))
                 (true (= (b::y a2) (b::y a)))
                 (true (= (b::x a2) (b::x a)))
                 (true (= (b::x b2) (b::x b)))
                 ;; segment can have 0 length
                 (true (<= (b::x a) (b::x b)))
                 ;; and right of previous segment if any
                 (when b1
                   (is >= (b::x a) (b::x b1)))
                 (true (equalp (and (b::point-gap-p a) (b::gaps a))
                               (and (b::point-gap-p a2) (b::gaps a2))))))))

(defun test-d ()
  (flet ((gaps (n y1 y2)
           (loop for i below n
                 for y = (+ y1 (random (- y2 y1)))
                 collect (random (- y2 y))
                 collect y)))
    (let* ((fe-step 2)
           (step-step (* fe-step 2)))
      (loop
        for gaps below 2
        do (loop
             for steps below 4
             for sh = (* steps step-step)
             for sw = (* (1- steps) step-step)
             do (loop
                  for gw from 1 below (* step-step 2) by fe-step
                  do (loop
                       for gh from fe-step below sh by fe-step
                       for r = nil
                       do (loop
                            for y below sh by step-step
                            for y2 = (+ y step-step)
                            for gap = (gaps gaps y y2)
                            for x from 0 by step-step
                            do (push (list* x y y2 gap)
                                     r))
                          ;; falling edge
                          (push (list (+ sw gw) sh (- sh gh)) r)
                          ;; rightmost edge
                          (let ((rx (+ sw gw gw)))
                            (push (list rx (- sh gh) 0) r)
                            (loop for i from 1 below (1+ rx)
                                  do (check-d2 (reverse r) rx i)
                                  #++(sleep 0.15))))))))))
#++
(time (test 'make-d :report 'interactive))

(define-test (binpack make-d)
  ;; test1 top
  (check-d 0 2 '((-2 4 3) (1 5 1) (2 4 3)) nil)
  ;; test1 bottom
  (check-d 1 2 '((0 1 2) (2 5 0) (2 4 3)) '(1 2))
  ;; test1b top (no falling edge)
  (check-d 2 2 '((-2 4 3) (1 5 4)) nil)
  ;; test1b bottom
  (check-d 3 2 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 3 '((-2 4 3) (1 5 0) (1 4 4)) nil)
  (check-d 1 3 '((0 1 2) (2 4 3)) '(1))
  (check-d 2 3 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 3 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 4 '((-2 4 7)) nil)
  (check-d 1 4 '((0 1 1)) '())
  (check-d 2 4 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 4 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 5 '((-2 4 7)) nil)
  (check-d 1 5 '((0 1 5)) '())
  (check-d 2 5 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 5 '((0 1 2) (2 5 3)) '(1))

  (check-d 0 6 '((-2 4 7)) nil)
  (check-d 1 6 '() '())
  (check-d 2 6 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 6 '() '())

  (check-d 0 7 '((-2 4 7)) nil)
  (check-d 1 7 '() '())
  (check-d 2 7 '((-2 4 3) (1 5 4)) nil)
  (check-d 3 7 '() '())

  (check-d 0 8 '() nil)
  (check-d 1 8 '() '())
  (check-d 2 8 '() nil)
  (check-d 3 8 '() '())

  ;; automated test cases
  (test-d)
  (check-d2 '((1 3 7)
              (5 7 9)
              (10 9 6))
            7 5))

(defun make-c* (l end points)
  (let ((fb (apply
             #'make-instance
             'b::f-edge
             (loop for (ax y1 y2) in points
                   collect nil into gaps
                   collect (if (< y1 y2)
                               (b::make-point-bottom-left ax (min y1 y2))
                               (b::make-point ax (min y1 y2)))
                     into d
                   collect (b::make-point ax (max y1 y2)) into h
                   finally (return (list :d (coerce d 'vector)
                                         :h (coerce h 'vector)
                                         :gaps (coerce gaps 'vector)))))))
    (b::make-c fb l end)))



(defparameter *subhole-fbs*
  #(;; top of sh-test1
    (2
     (-2 3 4)
     (-1 2 3)
     (5 4 2))
    ;; bottom of sh-test1
    (5
     (0 0 1)
     (5 4 0))
    ;; subholes of fig6b (offset by -11, -2 to match debug render)
    (3
     (-11 -2 -1)
     (-9 -3 -2)
     (-6 2 -3)
     (-2 -4 2)
     (6 3 -4)
     (13 5 3))
    (13
     (-7 -7 -6)
     (2 -8 -7)
     (6 3 -8)
     (7 -8 3)
     (13 5 -8))
    (1
     (-8 6 7)
     (9 8 6))
    (-8
     (-13 2 3)
     (-12 1 2)
     (-6 2 1)
     (6 3 2)
     (13 4 3))))

(defun check-c (index l spans)
  (format t "~&*~%*~%check-c ~s = ~s~%" index spans)
  (destructuring-bind (end &rest ft)
      (aref *subhole-fbs* index)
    (format t "fb = ~s~%" ft)
    (binpack2-vis::clear-shapes)
    (binpack2-vis::with-polyline (:rgba '(1 0 1 1) :close nil)
      (loop for (x y2 y1) in ft
            do (binpack2-vis::add-shape-point (* x 1) (* y1 1))
               (binpack2-vis::add-shape-point (* x 1) (* y2 1))))
    (binpack2-vis::with-polyline (:rgba '(1 0 0 1) :close nil)
      (binpack2-vis::add-shape-point (* end 1) 0)
      (binpack2-vis::add-shape-point (* end 1) 16))
    (let ((count (length spans)))
      (let* ((c (finish (make-c* l end ft))))
        (format t "c = ~s~%" c)
        (loop  for (a b) on (coerce c 'list) by #'cddr
               for x1 = (b::x a)
               for y1 = (b::y a)
               for x2 = (b::x b)
               for y2 = (b::y b)
               do (binpack2-vis::with-polyline (:rgba '(1 1 0 1) :close nil)
                    (binpack2-vis::add-shape-point (+ (* x1 1) 0.25)
                                                   (+ (* y1 1) 0.25))
                    (binpack2-vis::add-shape-point (- (* x2 1) 0.25)
                                                   (+ (* y2 1) 0.25))))
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
                   (false (and (b::point-gap-p a) (b::gaps a))))))))


(defun brute-force-c (w end points)
  (let* ((points (coerce points 'vector))
         (lp (length points))
         (r nil)
         (x1 (car (aref points 0)))
         (x2 (car (aref points (1- lp))))
         (last-y nil)
         (p1 (make-array (- x2 x1 w -2) :initial-element nil))
         (b (make-array (- x2 x1) :initial-element nil)))
    (loop
      ;; at every position between start and end of the points
      for lx from x1 upto (- x2 w)
      for rx = (+ lx w)
      for i from 0
      ;; drop a box of width W and see where it lands
      for y = (loop for j below (1- lp)
                    for (x1 y12 y11) = (aref points j)
                    for (x2 y22 y21) = (aref points (1+ j))
                    for y1 = y12
                    when (or (and (<= x1 lx) (< lx x2))
                             ;;(<= x1 rx x2)
                             (and (< x1 rx) (<= rx x2))
                                        ;(<= lx x1 rx)
                             (and (<= lx x1) (< x1 rx))
                             (and (< lx x2) (<= x2 rx)))
                      maximize y1
                    while (<= x1 rx))
      do (setf (aref p1 i) (list lx y)))
    #++(format t "~s~%"  p1)
    ;; build a vector of where bottom-left edges are
    (loop for i below lp
          for (x y2 y1) = (aref points i)
          when (< y2 y1)
            do (setf (aref b (- x x1)) (list y2 y1)))
    ;; assemble list of placements
    (loop for last-x = nil then x
          for (x y) across p1
          for (by1 by2) across b
          while x
          when (>= x end)
            do (if (and (= x end)
                        (> y last-y))
                   (progn
                                        ;(break "fhj")
                     #++(format t "~s ~s ~s~%" x1 last-x (- end w))
                     (push (b::make-point-open last-x
                                               last-y)
                           r)
                     (push (b::make-point-open (min (max x1 last-x)
                                                    end)
                                               y)
                           r)
                     (setf last-x end
                           last-y y))
                   (progn #++ (format t "@@ ~s ~s ~s~%" x1 last-x end)
                          #++(when (equalp (list x1 last-x end)
                                           '(0 2 3))
                               (format t "  ~s~%" (map 'list 'b::pp r)))
                          (setf last-x end)))
               (loop-finish)
          finally
             (when (and last-x last-y)
               (push (b::make-point-open (min end last-x) last-y) r))
          when (and last-y
                    (/= y last-y))
            ;; end previous span
            do (push (b::make-point (if (< y last-y) x (1- x))
                                    last-y)
                     r)
          unless (eql last-y y)
            ;; start a new span
            do (if (and by1 (<= by1 y) (< y by2))
                   (push (b::make-point-bottom-left x y) r)
                   (push (b::make-point-open (max last-x (- x w)) y) r))
               (setf last-y y))
    (assert (evenp (length r)))
    #++
    (when r (assert (/= (b::x (first r))
                        (b::x (second r)))))
    (nreverse r)))

(defun gen-c (heights &key (spacing 1))
  (unless (vectorp heights)
    (setf heights (coerce heights 'vector)))
  (let ((m (reduce 'max heights)))
    (loop with l = (length heights)
          for i from -1 below l
          for x from 0 by spacing
          for y1 = (if (<= 0 i (1- l))
                       (aref heights i)
                       (1+ m))
          for y2 = (if (<= 0 (1+ i) (1- l))
                       (aref heights (1+ i))
                       (1+ m))
          unless (= y1 y2) collect (list x y2 y1))))

(defun map-heights (fun edge-count height-count)
  (loop for i below (expt height-count edge-count)
        for z = 0
        for n1 = nil
        for h = (loop with n = i
                      for j below edge-count
                      for x = (mod n height-count)
                      do (setf n1 (or n1 (plusp x)))
                         (when (and (zerop x) (not n1))
                           (incf z))
                      collect x
                      do (setf n (floor n height-count)))
        do (when (= 5 z)
             (format t "~s: ~s~%" i h))
           (funcall fun h)))

(defun check-c2a (test &key (end1 1) (vis nil))
  (let ((ft (gen-c test))
        (edge-count (length test)))
    #++(format t "fb = ~s~%" ft)
    (loop
      for end from (min edge-count (max 1 end1)) below (1+ edge-count)
      do (loop
           for l from 1 below edge-count
           do (when vis
                (binpack2-vis::clear-shapes)
                (binpack2-vis::with-polyline (:rgba '(0 1 0 1)
                                              :close nil)
                  (binpack2-vis::add-shape-point 0 -0.2)
                  (binpack2-vis::add-shape-point l -0.2)
                  )
                (binpack2-vis::with-polyline (:rgba '(1 0 1 1)
                                              :close nil)
                  (loop
                    for (x y2 y1) in ft
                    do (binpack2-vis::add-shape-point (* x 1) (* y1 1))
                       (binpack2-vis::add-shape-point (* x 1) (* y2 1))))
                (binpack2-vis::with-polyline (:rgba '(1 0 0 1)
                                              :close nil)
                  (binpack2-vis::add-shape-point (* end 1) 0)
                  (binpack2-vis::add-shape-point (* end 1) 16)))
              (let* ((c (make-c* l end ft))
                     (c2 (brute-force-c l end ft)))
                #++(format t "c = ~s~%" c)
                #++(break "~s" (map 'list 'b::pp c))
                (when vis
                  (loop
                    for (a b) on (coerce c 'list) by #'cddr
                    for x1 = (b::x a)
                    for y1 = (b::y a)
                    for x2 = (b::x b)
                    for y2 = (b::y b)
                    do (binpack2-vis::with-polyline (:rgba '(1 1 0 1) :close nil)
                         (binpack2-vis::add-shape-point
                          (+ (* x1 1) 0.25)
                          (+ (* y1 1) 0.25))
                         (binpack2-vis::add-shape-point
                          (- (* x2 1) 0.25)
                          (+ (* y2 1) 0.25))))

                  (loop
                    for (a b) on (coerce c2 'list) by #'cddr
                    for x1 = (b::x a)
                    for y1 = (b::y a)
                    for x2 = (b::x b)
                    for y2 = (b::y b)
                    do (binpack2-vis::with-polyline (:rgba '(0 1 1 1) :close nil)
                         (binpack2-vis::add-shape-point
                          (+ (* x1 1) 0.25)
                          (+ (* y1 1) 0.4))
                         (binpack2-vis::add-shape-point
                          (- (* x2 1) 0.25)
                          (+ (* y2 1) 0.4)))))
                #++(break "~s~%~s"
                          (map 'list 'b::pp c)
                          (map 'list 'b::pp c2)
                          )
                ;; same # of placements from both algorithms
                (assert (= (length c) (length c2)))
                ;; 2 points per placement
                (assert (evenp (length c)))
                (loop for b1 = nil then b
                      for (a b) on (coerce c 'list) by #'cddr
                      for (a2 b2) on (coerce c2 'list) by #'cddr
                      for i from 0
                      do (assert b)
                      when b
                        do ;; both with same y value, and with 2nd right of first
                           (assert (= (b::y a) (b::y b)))
                           ;; segment can have 0 length
                           (assert (<= (b::x a) (b::x b)))
                           ;; and same points from both algorithhm
                           (assert (= (b::x a) (b::x a2)))
                           (assert (= (b::y a) (b::y a2)))
                           (assert (= (b::x b) (b::x b2)))
                           (assert (= (b::y b) (b::y b2)))
                           (assert
                            (eql (typep a 'b::point-bottom-left)
                                 (typep a2 'b::point-bottom-left)))
                           ;; and right of previous segment if any
                           (when b1
                             (assert ( >= (b::x a) (b::x b1))))
                           (assert (not (b::point-gap-p a)))
                           (assert (not (b::point-gap-p b))))))))

  )
(defun check-c2 (edge-count height-count &key (end1 1))
  #++(format t "~&*~%*~%check-c2 ~s , ~s~%" edge-count height-count)

  (map-heights (lambda (a) (check-c2a a :end1 end1) )
               edge-count height-count)
  )
(defvar *exit* t)
(defun check-c2r (edge-count height-count &key (end1 1))
  (setf *exit* nil)
  (loop for i from 0
        for a = (loop repeat edge-count collect (random height-count))
        until *exit*
        do (check-c2a a :end1 end1)
        when (zerop (mod i 1000))
          do (format t "~s~%" i)
        )

  (map-heights (lambda (a) (check-c2a a :end1 end1) )
               edge-count height-count)
  )
#++
(time (check-c2 16 3 :end1 99))
;;  3942.431 seconds of real time
#++
(time (check-c2 16 4 :end1 99))
;; 348.496 seconds of real time
#++(time (check-c2 20 5 :end1 1))
#++(time (check-c2r 20 6))



;(check-c2a '(13 13 13 12 12 7 7 4 7 6 7 4 4 7 7 7 2 2 2 0 0) :end1 20)
(check-c2a '(13 13 13 12 12 7 7 4 7 6 7 4 4 7 4 4 0 0) :end1 21)
(check-c2a '(3 2 0 1 0 1 0 1 0 0 1 0 0 0 0) :end1 199)
(0 3 2 0 1 0 1 0 1 0 0 1 0 0 0 0 0 0 0 0)
(0 3 2 0 1 0 1 0 1 0 0 1 0 0 0 0)
#++
(test 'make-c :report 'interactive)

(define-test (binpack make-c)
  ;; currently C extends past left side of hole
  ;; test1 top
  (check-c 0 1 '((-2 3 1) (-1 2 3)))
  ;; test1 bottom
  (check-c 1 1 '((0 0 4)))

  (check-c 0 2 '((-2 3 1) (-1 2 3)))
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

  (check-c 2 1 '((-11 -2 2) (-9 -3 2) (-7 2 5) (-2 -4 5)))
  (check-c 3 1 '((-7 -7 9) (2 -8 3) (5 3 2) (7 -8 5)))
  (check-c 4 1 '((-8 6 9)))
  (check-c 5 1 '((-13 2 1) (-12 1 4)))

  (check-c 2 2 '((-11 -2 2) (-9 -3 1) (-8 2 6) (-2 -4 5)))
  (check-c 3 2 '((-7 -7 9) (2 -8 2) (4 3 3) (7 -8 4)))
  (check-c 4 2 '((-8 6 9)))
  (check-c 5 2 '((-13 2 1) (-12 1 4)))

  (check-c 2 3 '((-11 -2 2) (-9 -3 0) (-9 2 7) (-2 -4 5)))
  (check-c 3 3 '((-7 -7 9) (2 -8 1) (3 3 4) (7 -8 3)))
  (check-c 4 3 '((-8 6 9)))
  (check-c 5 3 '((-13 2 1) (-12 1 3) (-9 2 1)))

  (check-c 2 4 '((-11 -2 1) (-10 2 8) (-2 -4 4) (2 3 1)))
  (check-c 3 4 '((-7 -7 9) (2 -8 0) (2 3 5) (7 -8 2)))
  (check-c 4 4 '((-8 6 9)))
  (check-c 5 4 '((-13 2 1) (-12 1 2) (-10 2 2)))

  (check-c 2 5 '((-11 -2 0) (-11 2 9) (-2 -4 3) (1 3 2)))
  (check-c 3 5 '((-7 -7 8) (1 3 6) (7 -8 1)))
  (check-c 4 5 '((-8 6 9)))
  (check-c 5 5 '((-13 2 1) (-12 1 1) (-11 2 3)))

  (check-c 2 6 '((-11 2 9) (-2 -4 2) (0 3 3)))
  (check-c 3 6 '((-7 -7 7) (0 3 7) (7 -8 0)))
  (check-c 4 6 '((-8 6 9)))
  (check-c 5 6 '((-13 2 1) (-12 1 0) (-12 2 4)))

  (check-c 2 7 '((-11 2 9) (-2 -4 1) (-1 3 4)))
  (check-c 3 7 '((-7 -7 6) (-1 3 7)))
  (check-c 4 7 '((-8 6 9)))
  (check-c 5 7 '((-13 2 5)))

  (check-c 2 8 '((-11 2 9) (-2 -4 0) (-2 3 5)))
  (check-c 3 8 '((-7 -7 5) (-2 3 7)))
  (check-c 4 8 '((-8 6 9)))
  (check-c 5 8 '((-13 2 5)))

  (check-c 2 9 '((-11 2 8) (-3 3 6)))
  (check-c 3 9 '((-7 -7 4) (-3 3 7)))
  (check-c 4 9 '((-8 6 8)))
  (check-c 5 9 '((-13 2 5)))

  (check-c 2 10 '((-11 2 7) (-4 3 7)))
  (check-c 3 10 '((-7 -7 3) (-4 3 7)))
  (check-c 4 10 '((-8 6 7)))
  (check-c 5 10 '((-13 2 5)))

  (check-c 2 11 '((-11 2 6) (-5 3 7)))
  (check-c 3 11 '((-7 -7 2) (-5 3 7)))
  (check-c 4 11 '((-8 6 6)))
  (check-c 5 11 '((-13 2 5)))

  (check-c 2 12 '((-11 2 5) (-6 3 7)))
  (check-c 3 12 '((-7 -7 1) (-6 3 7)))
  (check-c 4 12 '((-8 6 5)))
  (check-c 5 12 '((-13 2 5)))

  (check-c 2 13 '((-11 2 4) (-7 3 7)))
  (check-c 3 13 '((-7 -7 0) (-7 3 7)))
  (check-c 4 13 '((-8 6 4)))
  (check-c 5 13 '((-13 2 5)))

  (check-c 2 14 '((-11 2 3) (-8 3 7)))
  (check-c 3 14 '((-7 3 6)))
  (check-c 4 14 '((-8 6 3)))
  (check-c 5 14 '((-13 2 5)))

  (check-c 2 15 '((-11 2 2) (-9 3 7)))
  (check-c 3 15 '((-7 3 5)))
  (check-c 4 15 '((-8 6 2)))
  (check-c 5 15 '((-13 2 4) (-9 3 1)))

  (check-c 2 16 '((-11 2 1) (-10 3 7)))
  (check-c 3 16 '((-7 3 4)))
  (check-c 4 16 '((-8 6 1)))
  (check-c 5 16 '((-13 2 3) (-10 3 2)))

  (check-c 2 17 '((-11 2 0) (-11 3 7)))
  (check-c 3 17 '((-7 3 3)))
  (check-c 4 17 '((-8 6 0)))
  (check-c 5 17 '((-13 2 2) (-11 3 3)))

  (check-c 2 18 '((-11 3 6)))
  (check-c 3 18 '((-7 3 2)))
  (check-c 4 18 'NIL)
  (check-c 5 18 '((-13 2 1) (-12 3 4)))

  (check-c 2 19 '((-11 3 5)))
  (check-c 3 19 '((-7 3 1)))
  (check-c 4 19 'NIL)
  (check-c 5 19 '((-13 2 0) (-13 3 5)))

  (check-c 2 20 '((-11 3 4)))
  (check-c 3 20 '((-7 3 0)))
  (check-c 4 20 'NIL)
  (check-c 5 20 '((-13 3 5)))

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
                 (transform-points points (- x1) (- y1) 1))))
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

(define-test (binpack misc)
  (let ((a (finish (make-instance 'b::placement :x 1 :y 5 :w 2 :h 3))))
    (true (b::point-on-rect 1 5 a))
    (true (b::point-on-rect 1 6 a))
    (true (b::point-on-rect 1 8 a))
    (true (b::point-on-rect 2 5 a))
    (true (b::point-on-rect 3 5 a))
    (true (b::point-on-rect 2 8 a))
    (true (b::point-on-rect 3 8 a))
    (false (b::point-on-rect 0 5 a))
    (false (b::point-on-rect 0 6 a))
    (false (b::point-on-rect 0 8 a))
    (false (b::point-on-rect 4 5 a))
    (false (b::point-on-rect 4 6 a))
    (false (b::point-on-rect 4 8 a))
    (false (b::point-on-rect 2 6 a))
    #++
    (flet ((hv (x y)
             (make-instance 'b::hole-vertex :x x :y y)))
      (true (b::hv-span-overlaps-rect (hv 1 0) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 1 0) (hv 1 6) a))
      (true (b::hv-span-overlaps-rect (hv 1 0) (hv 1 8) a))
      (true (b::hv-span-overlaps-rect (hv 1 0) (hv 1 10) a))

      (true (b::hv-span-overlaps-rect (hv 1 5) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 1 5) (hv 1 6) a))
      (true (b::hv-span-overlaps-rect (hv 1 5) (hv 1 8) a))
      (true (b::hv-span-overlaps-rect (hv 1 5) (hv 1 10) a))

      (true (b::hv-span-overlaps-rect (hv 1 7) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 1 7) (hv 1 6) a))
      (true (b::hv-span-overlaps-rect (hv 1 7) (hv 1 8) a))
      (true (b::hv-span-overlaps-rect (hv 1 7) (hv 1 10) a))

      (true (b::hv-span-overlaps-rect (hv 1 8) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 1 8) (hv 1 6) a))
      (true (b::hv-span-overlaps-rect (hv 1 8) (hv 1 8) a))
      (true (b::hv-span-overlaps-rect (hv 1 8) (hv 1 10) a))

      (true (b::hv-span-overlaps-rect (hv 1 10) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 1 10) (hv 1 6) a))
      (true (b::hv-span-overlaps-rect (hv 1 10) (hv 1 8) a))
      (false (b::hv-span-overlaps-rect (hv 1 10) (hv 1 10) a))


      (true (b::hv-span-overlaps-rect (hv 3 0) (hv 3 5) a))
      (true (b::hv-span-overlaps-rect (hv 3 0) (hv 3 6) a))
      (true (b::hv-span-overlaps-rect (hv 3 0) (hv 3 8) a))
      (true (b::hv-span-overlaps-rect (hv 3 0) (hv 3 10) a))
      (true (b::hv-span-overlaps-rect (hv 3 5) (hv 3 5) a))
      (true (b::hv-span-overlaps-rect (hv 3 5) (hv 3 6) a))
      (true (b::hv-span-overlaps-rect (hv 3 5) (hv 3 8) a))
      (true (b::hv-span-overlaps-rect (hv 3 5) (hv 3 10) a))
      (true (b::hv-span-overlaps-rect (hv 3 7) (hv 3 5) a))
      (true (b::hv-span-overlaps-rect (hv 3 7) (hv 3 6) a))
      (true (b::hv-span-overlaps-rect (hv 3 7) (hv 3 8) a))
      (true (b::hv-span-overlaps-rect (hv 3 7) (hv 3 10) a))
      (true (b::hv-span-overlaps-rect (hv 3 8) (hv 3 5) a))
      (true (b::hv-span-overlaps-rect (hv 3 8) (hv 3 6) a))
      (true (b::hv-span-overlaps-rect (hv 3 8) (hv 3 8) a))
      (true (b::hv-span-overlaps-rect (hv 3 8) (hv 3 10) a))
      (true (b::hv-span-overlaps-rect (hv 3 10) (hv 3 5) a))
      (true (b::hv-span-overlaps-rect (hv 3 10) (hv 3 6) a))
      (true (b::hv-span-overlaps-rect (hv 3 10) (hv 3 8) a))
      (false (b::hv-span-overlaps-rect (hv 3 10) (hv 3 10) a))


      (false (b::hv-span-overlaps-rect (hv 2 0) (hv 1 5) a))
      (false (b::hv-span-overlaps-rect (hv 2 0) (hv 1 6) a))
      (false (b::hv-span-overlaps-rect (hv 2 0) (hv 1 8) a))
      (false (b::hv-span-overlaps-rect (hv 2 0) (hv 1 10) a))
      (false (b::hv-span-overlaps-rect (hv 2 5) (hv 1 6) a))
      (false (b::hv-span-overlaps-rect (hv 2 5) (hv 1 8) a))
      (false (b::hv-span-overlaps-rect (hv 2 5) (hv 1 10) a))
      (false (b::hv-span-overlaps-rect (hv 2 7) (hv 1 5) a))
      (false (b::hv-span-overlaps-rect (hv 2 7) (hv 1 6) a))
      (false (b::hv-span-overlaps-rect (hv 2 7) (hv 1 10) a))
      (false (b::hv-span-overlaps-rect (hv 2 8) (hv 1 5) a))
      (false (b::hv-span-overlaps-rect (hv 2 8) (hv 1 6) a))
      (false (b::hv-span-overlaps-rect (hv 2 8) (hv 1 10) a))
      (false (b::hv-span-overlaps-rect (hv 2 10) (hv 1 5) a))
      (false (b::hv-span-overlaps-rect (hv 2 10) (hv 1 6) a))
      (false (b::hv-span-overlaps-rect (hv 2 10) (hv 1 8) a))
      (false (b::hv-span-overlaps-rect (hv 2 10) (hv 1 10) a))

      (true (b::hv-span-overlaps-rect (hv 0 5) (hv 1 5) a))
      (true (b::hv-span-overlaps-rect (hv 0 5) (hv 10 5) a))
      (true (b::hv-span-overlaps-rect (hv 0 8) (hv 1 8) a))
      (true (b::hv-span-overlaps-rect (hv 0 8) (hv 10 8) a))


      )

    )
  )

#++
(time
 (test 'binpack))



#++
(time (test 'binpack :report 'quiet))


#++
(test 'make-d :report 'interactive)

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
