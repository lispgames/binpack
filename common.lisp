(in-package #:binpack/common)

(defclass rect ()
  ;; ignore by packer, used by caller to associate rect with actual
  ;; texture/etc being packed
  ((%id :reader id :initarg :id)
   ;; width/height of rect, set by caller
   (%w :accessor w :initarg :w)
   (%h :accessor h :initarg :h)
   ;; position of rect once packed
   (%x :accessor x :initarg :x :initform nil)
   (%y :accessor y :initarg :y :initform nil)
   ;; page onto which rect has been packed (always 0 until multipage
   ;; packing is implemented)
   (%page :accessor page :initform nil)))

(defun rect (id x y w h)
  (make-instance 'rect :id id :x x :y y :w w :h h))

(defun rect* (w h &optional (id nil))
  (make-instance 'rect :id id :w w :h h))

(defmacro with-rect ((id x y w h) rect &body body)
  (alexandria:once-only (rect)
    `(symbol-macrolet (,@(when id `((,id (id ,rect))))
                       ,@(when x `((,x (x ,rect))))
                       ,@(when y `((,y (y ,rect))))
                       ,@(when w `((,w (w ,rect))))
                       ,@(when h `((,h (h ,rect)))))
       ,@body)))

(defun point-in-rect (rect px py)
  (with-rect (nil x y w h) rect
    (and (<= x px (+ x w))
         (<= y py (+ y h)))))

(defun print-rect (r &optional (stream *standard-output*))
  (format stream "~sx~s~@[@~{~s,~s,~s~}~]~@[=~s~]"
          (w r) (h r)
          (when (x r)
            (list (x r) (y r) (or (page r) 0)))
          (id r)))
(print-rect (rect 3 33 22 1 2) nil)

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

;; for debugging, needs opticl
#++
(defun draw-rects (file rects)
  (destructuring-bind (x1 y1)
      (loop for r in rects
            maximize (+ (x r) (w r)) into mx
            maximize (+ (y r) (h r)) into my
            finally (return (list mx my)))
    (let ((img (opticl:make-8-bit-rgb-image (max 1 y1) (max 1 x1))))
      (loop for x in rects
            for r = (+ 64 (random (- 255 64)))
            for g = (+ 64 (random (- 255 64)))
            for b = (+ 64 (random (- 255 64)))
            do (opticl:fill-rectangle img (y x) (x x)
                                      (+ (y x) (h x))
                                      (+ (x x) (w x))
                                      r g b))
      (loop for x in rects
            for r = (+ 64 (random (- 255 64)))
            for g = (+ 64 (random (- 255 64)))
            for b = (+ 64 (random (- 255 64)))
            do (opticl:draw-rectangle img
                                      (y x) (x x)
                                      (+ (y x) (h x))
                                      (+ (x x) (w x))
                                      r g b))
      (opticl:write-image-file file img))))

(defclass pack-state ()
  ;; vector with fill pointer, containing all pages being
  ;; filled. (possibly NIL for a page that is completely filled)
  ((state :initarg :state :accessor state)
   ;; keyword indicating which packing algorithm is in use (needed to
   ;; allocate extra pages)
   (algorithm :initarg :algorithm :reader algorithm)
   ;; max dimensions of a page
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   ;; SHAPING-* instance or NIL
   (shaping :initarg :shaping :accessor shaping)
   ;; NIL for single page, :first-fit, :best-fit, or :last-page
   (page-policy :initarg :page-policy :accessor page-policy :initform nil)))



;; sort functions. all use stable-sort, so can run 2 in a row for some
;; cases of "sort by X then Y". All are destructive, so copy sequence
;; before use if needed and save result.

(defun sort-rects/longest-side-desc (rects)
  (labels ((apply-fn (fn rect)
             (with-rect (nil nil nil w h) rect
               (funcall fn w h)))
           (sort-by (rects fn)
             (stable-sort rects #'> :key (lambda (x) (apply-fn fn x)))))
    (sort-by (sort-by rects #'min) #'max)))

(defun sort-rects/area-desc (rects)
  (labels ((a (r)
             (with-rect (nil nil nil w h) r
               (* w h))))
    (stable-sort rects '> :key #'a)))

(defun sort-rects/width-desc (rects)
  (stable-sort rects '> :key #'w))

(defun sort-rects/height-desc (rects)
  (stable-sort rects '> :key #'h))

(defun sort-rects/w+h-desc (rects)
  (setf rects (stable-sort rects '> :key #'h))
  (stable-sort rects '> :key #'w))

(defun sort-rects/perimeter-desc (rects)
  (labels ((p (r)
             (with-rect (nil nil nil w h) r
               (+ (* 2 w) (* 2 h)))))
    (stable-sort rects '> :key #'p)))

(defun sort-rects/aspect*area-desc (rects)
  (labels ((aa (r)
             (with-rect (nil nil nil w h) r
               (* (/ (max w h) (min w h))
                  w h))))
    (stable-sort rects '> :key #'aa)))

(defun total-pixels (rects)
  (loop for r in rects sum (* (w r) (h r))))

(defun rects-bounds (rects)
  (loop for r in rects
        when (x r)
          maximize (+ (x r) (w r)) into w
          and maximize (+ (y r) (h r)) into h
          and maximize (or (page r) 0) into page
        finally (return (values w h page))))


(defun intersectsp (rect1 rect2)
  (with-rect (nil x1 y1 w1 h1) rect1
    (with-rect (nil x2 y2 w2 h2) rect2
      (and (< x1 (+ x2 w2))
           (> (+ x1 w1) x2)
           (< y1 (+ y2 h2))
           (> (+ y1 h1) y2)))))

(defun containsp (outer inner)
  (with-rect (nil ox oy ow oh) outer
    (with-rect (nil ix iy iw ih) inner
      (and (>= (+ ox ow) (+ ix iw) ix ox)
           (>= (+ oy oh) (+ iy ih) iy oy)))))

