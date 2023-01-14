(in-package #:binpack/2)

;;;
;;;  MAXRECT packing as defined in http://clb.demon.fi/files/RectangleBinPack.pdf
;;;  See also: https://github.com/juj/RectangleBinPack
;;;


(defun delta-weight2 (width height rect)
  (with-rect (nil nil nil w h) rect
    (min (- w width) (- h height))))


(defun add-page/mr2 (state)
  (let ((f (make-array 1 :adjustable t
                         :fill-pointer 1
                         :initial-element (rect nil 0 0
                                                (width state)
                                                (height state)))))
    (vector-push-extend f (state state))))


(defun place/mr2 (free w h)
  (let ((best nil)
        (best-score -1))
    (loop for r across free
          for score = (delta-weight2 w h r)
          when (and (>= score 0)
                    (or (not best)
                        (< score best-score)))
            do (setf best r
                     best-score score)
          until (zerop best-score))
    (when best (values (x best) (y best)))))

(defun place/mr2/shape (free w h shaping)
  (let ((best nil)
        (best-score -1)
        (best-penalty nil)
        (tmp (rect* w h)))
    (loop for r across free
          for score = (delta-weight2 w h r)
          for penalty = (progn
                          (setf (x tmp) (x r)
                                (y tmp) (y r))
                          (shaping-penalty shaping tmp))
          when (and (>= score 0)
                    (or (not best)
                        (and (< score best-score)
                             (or (zerop penalty)
                                 (not best-penalty)
                                 (<= penalty best-penalty)))
                        (and best-penalty
                             (plusp best-penalty)
                             (<= penalty best-penalty))))
            do (setf best r
                     best-score score
                     best-penalty penalty)
          until (zerop best-score))
    (when best (values (x best) (y best)))))


(defun rl (rect)
  (with-rect (nil x y w h) rect
    (list x y w h)))

(defvar *fs* (make-hash-table))
(defvar *ns* (make-hash-table))

(defun pack-1/@/mr2 (rect state page)
  (when (aref (state state) page)
    (multiple-value-bind (x y)
        (if (shaping state)
            (place/mr2/shape (aref (state state) page) (w rect) (h rect)
                             (shaping state))
            (place/mr2 (aref (state state) page) (w rect) (h rect)))
      (if (and x y)
          (setf (page rect) page
                (x rect) x
                (y rect) y)
          (setf (page rect) nil
                (x rect) nil
                (y rect) nil))
      (when (x rect)
        (let* ((old (make-array 32 :adjustable t :fill-pointer 0))
               (new (make-array 32 :adjustable t :fill-pointer 0))
               (x1 (x rect))
               (y1 (y rect))
               (x2 (+ x1 (w rect)))
               (y2 (+ y1 (h rect)))
               (page (aref (state state) page)))
          ;; subdivide rects overlapping placement, and filter out any
          ;; that can't contain or be contained by the new rects
          (loop for i across page
                for ix1 = (x i)
                for iy1 = (y i)
                for ix2 = (+ ix1 (w i))
                for iy2 = (+ iy1 (h i))
                do (cond
                     ((not (binpack/common::touches i rect))
                      (assert (not (binpack/common::intersects i rect)))
                      (vector-push-extend i old))
                     ((not (binpack/common::intersects i rect))
                      (vector-push-extend i new))
                     (t
                      (assert (binpack/common::intersects i rect))
                      (assert (binpack/common::touches i rect))
                      (when (< ix1 x1 ix2)
                        (vector-push-extend (rect nil ix1 iy1 (- x1 ix1) (h i))
                                            new))
                      (when (< ix1 x2 ix2)
                        (vector-push-extend (rect nil x2 iy1 (- ix2 x2) (h i))
                                            new))
                      (when (< iy1 y1 iy2)
                        (vector-push-extend (rect nil ix1 iy1 (w i) (- y1 iy1))
                                            new))
                      (when (< iy1 y2 iy2)
                        (vector-push-extend (rect nil ix1 y2 (w i) (- iy2 y2))
                                            new)))))
          ;; copy rects that can't contain new rects into output
          (if (< (array-total-size page) (length old))
              (adjust-array page (ceiling (* 1.5 (+ (length old) (length new))))
                            :fill-pointer (length old))
              (setf (fill-pointer page) (fill-pointer old)))
          (replace page old :end2 (fill-pointer old))
          ;; move any rects that aren't contained by another into output
          (loop with l = (1- (fill-pointer new))
                with i = 0
                while (< i l)
                do (loop with r1 = (aref new i)
                         with j = (1+ i)
                         while (<= j l)
                         do (let ((r2 (aref new j)))
                              ;; if r2 contains r1, drop r1 and skip
                              ;; rest of this loop
                              (when (binpack/common::contains r2 r1)
                                (rotatef (aref new i) (aref new l))
                                (decf l)
                                (decf i)
                                (loop-finish))
                              ;; if r1 contains r2, drop r2
                              (if (binpack/common::contains r1 r2)
                                  (progn
                                    (rotatef (aref new j) (aref new l))
                                    (decf l))
                                  ;; otherwise, check next j
                                  (incf j))))
                   (incf i)
                   ;; contained rects were moved to end, update fill
                   ;; pointer once to remove them
                finally (setf (fill-pointer new) (1+ l)))
          (let* ((start (fill-pointer page))
                 (l2 (+ start (length new))))
            (if (< (array-total-size page) l2)
                (adjust-array page (ceiling (* 2 l2))
                              :fill-pointer l2)
                (incf (fill-pointer page) (length new)))
            (replace page new :start1 start :end2 (fill-pointer new))))))))
