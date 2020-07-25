(defpackage #:binpack2-vis
  (:use :cl #:3b-glim-example/s #:3b-glim-example/s-shaders)
  (:local-nicknames (#:glim #:3b-glim/s)
                    (#:2d #:3b-glim/2d)
                    (#:a #:alexandria-2)
                    (#:b #:binpack)))

(in-package binpack2-vis)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec3) ;; position
     (1 :vec3) ;; normal
     (2 :vec4) ;; uv
     (3 :vec4) ;; color
     )))
(defvar *validate* nil)
(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0))
  (glim:attrib-f 0 x y z))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun normal (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 2 x y z w))
(defun color (r g b &optional (a 1.0))
  (glim:attrib-f 3 r g b a))

(defparameter *esc* nil)

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))
(defparameter *kx* 0)
(defparameter *ky* 0)
(defparameter *mouse* t)

(Defparameter *undo* nil)
(defun regression-file ()
  (format nil "/tmp/binpack/regression.~a.lisp" (get-universal-time)))


(defvar *replays* nil)


(defclass binpack2-vis (scratchpad)
  ((scale :initform 1 :accessor scale)
   (draw-scale :initform 16 :accessor draw-scale)
   (spacing :initform 16 :accessor spacing)
   (origin :initform :center :accessor origin)
   (shapes :initform nil :accessor shapes)
   (x1 :initform 0 :accessor x1)
   (x2 :initform 0 :accessor x2)
   (y1 :initform 0 :accessor y1)
   (y2 :initform 0 :accessor y2)
   (gmx :initform 0 :accessor gmx)
   (gmy :initform 0 :accessor gmy)
   (snap :initform t :accessor snap)
   (show-colors :initform nil :accessor show-colors)
   (color-picker-w :initform 22 :accessor color-picker-w)
   (color-picker-bits :initform 3 :accessor color-picker-bits)
   (color-picker-col :initform '(1 0 0 1) :accessor color-picker-col)
   (edit-point :initform nil :accessor edit-point)
   (editing-point :initform nil :accessor editing-point)
   (edit-point-prev :initform nil :accessor edit-point-prev)
   (placing :initform nil :accessor placing)
   (places :initform nil :accessor places)
   (placesxy :initform (list 0 0) :accessor placesxy)
   (placed :initform nil :accessor placed)
   (saved :initform nil :accessor saved)
   (replay :initform nil :accessor replay)
   (replay-save :initform nil :accessor replay-save)
   (replay-delay :initform 0.1 :accessor replay-delay)
   (replay-time :initform 0 :accessor replay-time)
   (pwx :initform 1 :accessor pwx)
   (pwy :initform 1 :accessor pwy)
   (hole :initform nil :accessor hole)
   (tool :initform 'polyline :accessor tool))

  (:default-initargs :shaders '((:tex :vertex vertex/simple
                                      :fragment frag/tex)
                                (:solid :vertex vertex/simple
                                        :fragment frag/solid))))

(defvar *w* nil)

(defun reset (w &key replay)
  (if replay
      (if (eql replay :saved)
          (setf (Replay w) (or (replay-save w)
                               (reverse (placed w))))
          (setf (replay w)
                (reverse (placed w))))
      (setf (replay w) nil
            (replay-save w) nil))
  (setf (hole w) (b::init-hole 16 16))
  (when (replay w)
    (unless (eql (caar (replay w)) :init)
      (push (list :init 16 16) (replay w))))
  (setf (placed w) nil)
  (unless (replay w)
    (push (list :init 16 16) (placed w))
    (setf (saved w) nil))
  (if replay
      (setf (placing w) nil)
      (setf (placing w) t))
  (setf (places w) nil))

(defun replay-file (f)
  (cond
    ((consp f)
     (setf (replay *w*) f))
    (t
     (setf (replay *w*) (with-open-file (s f) (read s)))))
  #++(format t "replaying file ~s:~% ~s~%" f (replay *w*))
  (unless (eql (caar (replay *w*)) :init)
    (push (list :init 16 16) (replay *w*)))
  (setf (replay-save *w*) (replay *w*)))

(defun save-regression (w)
  (with-open-file (f (regression-file) :direction :output
                                       :if-does-not-exist :create)
    (write (reverse (placed w))
           :stream f :readably t)))


(defmacro with-regression-restart ((w) &body body)
  `(restart-case
       (progn ,@body)
     (:retry ()
      :report (lambda (s)
                (format s "save regression test if needed, then reset and replay it"))
       (unless (saved ,w)
         (save-regression w))
       (reset ,w :replay t))
     (:dump-and-restart ()
      :report (lambda (s)
                (format s "save regression test, then reset and continue"))
       (unless (saved ,w)
         (save-regression ,w))
       (reset ,w))
     (:cancel-replay ()
      :report (lambda (s)
                (format s "reset and stop replay"))
       (reset ,w :replay :saved)
       (setf (replay ,w) nil))
     (:stop-replay ()
      :report (lambda (s)
                (format s "stop replay!"))
       (setf (placing ,w) nil)
       (setf (pwx ,w) nil
             (pwy ,w) nil)
       (setf (replay ,w) nil))))

(defun save-undo (w)
  (push (shapes w) *undo*))


#++
(setf (color-picker-w *w*) 12
      (color-picker-bits *w*) 4)
#++
(setf (color-picker-w *w*) 4
      (color-picker-bits *w*) 5)
#++
(setf (color-picker-w *w*) 16
      (color-picker-bits *w*) 3)

(defclass point ()
  ((x :initform 0 :accessor x :initarg :x)
   (y :initform 0 :accessor y :initarg :y)))

(defclass shape ()
  ((rgba :initform '(1 0 0 1) :accessor rgba :initarg :rgba)
   (vertex-rgba :initform '(1 0.5 1 1) :accessor vertex-rgba)
   (drawing :initform t :accessor drawing)))

(defclass polyline (shape)
  ((points :initform (make-array 0 :adjustable t :fill-pointer 0)
           :reader points)
   (closed :initform nil :accessor closed)))

(defmethod draw-shape ((w binpack2-vis) (s polyline))
  (glim:with-draw (:lines :shader :solid)
    (apply #'color (rgba s))
    (loop for prev = nil then p
          for p across (points s)
          when prev
            do (vertex (x prev) (y prev))
               (vertex (x p) (y p))
          finally (when (and p (closed s))
                    (let ((e (aref (points s) 0)))
                      (vertex (x p) (y p))
                      (vertex (x e) (y e))))
                  (when (and p (drawing s))
                    (color 1 1 1 1)
                    (vertex (x p) (y p))
                    (vertex (gmx w) (gmy w)))))
  (when (vertex-rgba s)
    (gl:point-size 3)
    (glim:with-draw (:points :shader :solid)
      (apply #'color (vertex-rgba s))
      (loop for p across (points s)
            do (vertex (x p) (y p)))))
  (dispatch-draws w))

(defmethod click-shape ((w binpack2-vis) (s null) x y)
  (push (make-instance (tool w) :rgba (color-picker-col w)) (shapes w))
  (assert (not (closed (car (shapes w)))))
  (click-shape w (car (shapes w)) x y))

(defmethod click-shape ((w binpack2-vis) (s polyline) x y)
  (cond
    ((drawing s)
     (if (and (plusp (length (points s)))
              (= x (x (aref (points s) 0)))
              (= y (y (aref (points s) 0))))
         (progn
           (setf (closed s) t)
           (setf (drawing s) nil))
         (vector-push-extend (make-instance 'point :x x :y y)
                             (points s))))
    (t (click-shape w nil x y))))

(defmethod print-shape ((s polyline))
  (format t "polyline: ~s points, :closed ~s~%" (length (points s))
          (closed s))
  (loop for p across (points s)
        do (format t " ~s ~s~%" (x p) (y p))))

(defun translate-mouse (w x y &key snap)
  (let ((mx (+ (x1 w)
               (/ x (/ (wx w)
                       (- (x2 w) (x1 w))))))
        (my (+ (y1 w)
               (/ (- (wy w) y)
                  (/ (wy w)
                     (- (y2 w) (y1 w)))))))
    (if (and snap (snap w))
        (let ((s (spacing w)))
          (values (* s (round mx s))
                  (* s (round my s))))
        (values mx my))))

(defun uniforms ()
  (glim:uniform 'modelview (glim:ensure-matrix :modelview))
  (glim:uniform 'proj (glim:ensure-matrix :projection))
  #++
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview)))
  (glim:uniform 'normal-matrix (glim:ensure-matrix :modelview)))

(defmacro with-polyline ((&key (rgba ''(1 0 0 1))
                            (close t))
                         &body body)
  `(when *w*
     (progn
       (finish-shape *w*)
       (start-polyline :rgba ,rgba)
       ,@body
       (finish-shape *w* :close ,close))))

(defparameter *wait* nil)
(defun do-replay (w)
  (restart-case
      (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
        (when (and (> now (+ (replay-time w)
                             #++(replay-delay w)))
                   (not (eql *wait* 0)))
          (when (numberp *wait*)
            (decf *wait*))
          (setf (replay-time w) now)
          (let ((r (pop (replay w))))
            (push r (placed w))
            #++ (format t "~%replay ~s~%" r)
            (let (#++(*standard-output* (make-broadcast-stream)))
              (ecase (car r)
                (:init
                 (setf (placed w) nil)
                 (setf (placing w) nil)
                 (setf (places w) nil)
                 (setf (pwx w) nil
                       (pwy w) nil)
                 (setf (hole w) (apply #'b::init-hole (cdr r))))
                (:show
                 (setf (pwx w) (second r)
                       (pwy w) (third r))
                 (setf (places w) nil)
                 (with-polyline (:rgba `(1 0 1 1) :close t)
                   (add-shape-point 0 0)
                   (add-shape-point 0 (pwy w))
                   (add-shape-point (pwx w) (pwy w))
                   (add-shape-point (pwx w) 0))
                 #++(format t "pw* = ~s, ~s~%" (pwx w) (pwy w))
                 (setf (placing w) nil)
                 (when (or (not (places w))
                           (/= (pwx w) (first (placesxy w)))
                           (/= (pwy w) (second (placesxy w))))
                   (setf (places w)
                         (binpack::find-all-placements (hole w)
                                                       (pwx w) (pwy w)))
                   (setf (placesxy w) (list (pwx w) (pwy w))))
                 (when *validate*
                   (let ((x (loop for p in (places w)
                                  unless (b::valid-placement-p* p)
                                    collect p)))
                     (when x
                       (break "invalid placement(s) ~sx~s @ ~s?"
                              (b::w (car x)) (b::h (car x))
                              (mapcar 'b::pp x)))))
                 #++(format t "places = ~s~%" (places w)))
                (:place
                 (setf (pwx w) nil
                       (pwy w) nil)
                 (destructuring-bind (x y wx wy) (cdr r)
                   (glim:with-draw (:quads :shader :solid)
                     (color 0 1 0 1)
                     (let ((x1 (* x (spacing w)))
                           (x2 (* (+ x wx) (spacing w)))
                           (y1 (* y (spacing w)))
                           (y2 (* (+ y wy) (spacing w))))
                       (vertex x1 y1)
                       (vertex x2 y1)
                       (vertex x2 y2)
                       (vertex x1 y2)))
                   (dispatch-draws w)
                   (let ((ok nil))
                     (loop for p in (places w)
                           #+do (format *debug-io* "~s,~s ~sx~s =? ~s,~s ~sx~s?"
                                        x y wx wy
                                        (b::x p) (b::y p)
                                        (b::w p) (b::h p))
                           when (and (= x (b::x p))
                                     (= y (b::y p))
                                     (= wx (b::w p))
                                     (= wy (b::h p)))
                             do (setf (hole w)
                                      (b::remove-quad-from-hole (hole w) p))
                                (setf ok t)
                                (loop-finish))
                     (unless ok
                       #++(glut:swap-buffers)
                       (let ((x1 x)
                             (x2 (+ x wx))
                             (y1 y)
                             (y2 (+ y wy)))
                         (with-polyline (:rgba `(1 1 1 1) :close t)
                           (add-shape-point x1 y1)
                           (add-shape-point x2 y1)
                           (add-shape-point x2 y2)
                           (add-shape-point x1 y2)))
                       (break "failed to place ~sx~s @ ~s,~s?"
                              wx wy x y)))))))
            (when (and (not (replay w))
                       (not (places w)))
              (setf (placing w) t
                    (pwx w) nil
                    (pwy w) nil))
            (when (and (hole w) *validate*)
              (b::check-hole (hole w)))
            (redraw-hole w)
            (when (and (eql (car r) :show)
                       (pwx w)
                       (pwy w))
              (with-polyline (:rgba `(1 0 1 1) :close t)
                (add-shape-point 0 0)
                (add-shape-point 0 (pwy w))
                (add-shape-point (pwx w) (pwy w))
                (add-shape-point (pwx w) 0))))))
    (:replay ()
     :report "replay the placements"
      (reset w :Replay :saved))
    (:cancel-replay ()
     :report "cancel replay and reset"
      (reset w :Replay :saved)
      (setf (replay w) nil)
      (setf (placing w) t)
      (redraw-hole w))
    (:stop-replay ()
     :report "cancel replay"
      (setf (replay w) nil)
      (setf *replays* nil)
      (setf (placing w) t)
      (redraw-hole w))))

(defmethod display ((w binpack2-vis) now)
  (setf *w* w)
  (when (and *replays* (not (replay w)))
    (when (> (length *replays*) 2)
      (format t "~s replays left~%" (length *replays*)))
    (replay-file (#-sbcl pop
                  #+sbcl sb-ext:atomic-pop
                  *replays*)))
  (glim:with-state (*format*)
    (when (editing-point w)
      (setf (x (editing-point w)) (gmx w)
            (y (editing-point w)) (gmy w)))

    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :sample-alpha-to-coverage
               :polygon-smooth :line-smooth
               :blend :multisample :texture-2d)

    (gl:blend-func :src-alpha :one-minus-src-alpha)

    (gl:disable :cull-face :lighting :light0 :light1
                :depth-test :sample-alpha-to-coverage)
    (glim:uniform 'debug1 *debug*)
    (glim:uniform 'tex0 0)
    (glim:uniform 'tex1 1)
    (glim:uniform 'flip 0)

    (let* ((x1 0)
           (y1 0)
           (scale (scale w))
           (step (spacing w))
           (x2 (/ (wx w) scale))
           (y2 (/ (wy w) scale)))
      (glim:with-pushed-matrix (:modelview)
        (ecase (origin w)
          (:center
           (setf x2 (/ (wx w) scale))
           (setf y2 (/ (wy w) scale))
           (setf x1 (- x2))
           (setf y1 (- y2)))
          (:lower-left
           (glim:translate -1 -1 0))
          ;; todo: add others
          )
        (when (aref *flags* 1)
          (incf x1 (/ (wx w) 2 scale))
          (incf y1 (/ (wy w) 2 scale))
          (incf x2 (/ (wx w) 2 scale))
          (incf y2 (/ (wy w) 2 scale))
          (glim:translate -0.5 -0.5 0))
        (when (aref *flags* 2)
          (incf x1 (/ (wx w) 4 scale))
          (incf y1 (/ (wy w) 4 scale))
          (incf x2 (/ (wx w) 4 scale))
          (incf y2 (/ (wy w) 4 scale))
          (glim:translate -0.25 -0.25 0))

        (glim:scale (/ (wx w)) (/ (wy w)) 1)
        (glim:translate 0.5 0.5 0)
        (glim:scale scale scale 1)
        (uniforms)
        (setf (x1 w) x1)
        (setf (x2 w) x2)
        (setf (y1 w) y1)
        (setf (y2 w) y2)
        (setf (values (gmx w) (gmy w))
              (translate-mouse w (mx w) (my w)))
                                        ;(format t "x1=~s,~s, 2=~s,~s~%" x1 y1 x2 y2)

        (when (replay w)
          (do-replay w))


        (glim:with-draw (:lines :shader :solid)
          (color 0.2 0.1 0.2 1)
          (when (minusp y1)
            (loop for x from 0 downto x1 by step
                  do (vertex x y1)
                     (vertex x y2)))
          (loop for x from 0 upto x2 by step
                do (vertex x y1)
                   (vertex x y2))
          (when (minusp y1)
            (loop for y from 0 downto y1 by step
                  do (vertex x1 y)
                     (vertex x2 y)))
          (loop for y from 0 upto y2 by step
                do (vertex x1 y)
                   (vertex x2 y))

          (color 0.2 0.2 0.2 1)
          (vertex -500 -500)
          (vertex 500 500)
          (vertex -500 500)
          (vertex 500 -500)

          (color 0.2 0.2 0.4 1)
          (vertex x1 0)
          (vertex x2 0)
          (vertex 0 y1)
          (vertex 0 y2))
        (with-regression-restart (w)
          (when (and (placing w) (hole w))
            (flet ((s (x) (* (spacing w)
                             (if (minusp x) -1 1)
                             (ceiling (abs x) (spacing w)))))
              (let* ((mx (s (gmx w)))
                     (my (s (gmy w)))
                     (in 2))
                (glim:with-draw (:lines :shader :solid)
                  (color 1 0.1 1.0 1)
                  (vertex 0 0) (vertex 0 (s my))
                  (vertex 0 (s my)) (vertex (s mx) (s my))
                  (vertex (s mx) (s my)) (vertex (s mx) 0)
                  (vertex (s mx) 0) (vertex 0 0))
                (when (and (not (zerop mx)) (not (zerop my)))
                  (glim:with-draw (:quads :shader :solid)
                    (color 0 1 0 0.1)
                    (let* ((mx (/ (abs mx) 16))
                           (my (/ (abs my) 16))
                           (px (binpack::find-all-placements (hole w) mx my)))
                      (setf (places w) px)
                      (loop with mx = (* mx 16)
                            with my = (* my 16)
                            for p in px
                            for x = (* 16 (binpack::x p))
                            for y = (* 16 (binpack::y p))
                            do (vertex (+ x in) (+ y in))
                               (vertex (+ x in) (+ y my (- in)))
                               (vertex (+ x mx (- in) in) (+ y my (- in)))
                               (vertex (+ x mx (- in) in) (+ y in))))))))))
        (when (and (not (placing w)) (places w))
          (let* ((mx (/ (gmx w) 16))
                 (my (/ (gmy w) 16))
                 (in 2))
            (color 0 1 0 0.1)
            (loop for p in (places w)
                  for x1 = (* 16 (b::x p))
                  for y1 = (* 16 (b::y p))
                  for x2 = (+ x1 (* 16 (b::w p)))
                  for y2 = (+ y1 (* 16 (b::h p)))

                  when (b::point-in-rect p mx my)
                    do (glim:with-draw (:quads :shader :solid)
                         (vertex (+ x1 in) (+ y1 in))
                         (vertex (+ x1 in) (+ y2 (- in)))
                         (vertex (+ x2 (- in) in) (+ y2 (- in)))
                         (vertex (+ x2 (- in) in) (+ y1 in))))))

        (dispatch-draws w)
        (ignore-errors
         (when (shapes w)
           (loop for s in (reverse (shapes w)) do (draw-shape w s))))
        (dispatch-draws w)))
    (when (show-colors w)
      (glim:with-pushed-matrix (:modelview)
        (glim:load-identity)
        (glim:translate -1 -1 0)
        (glim:scale (/ 2 (wx w)) (/ 2 (wy w)) 1)
        (uniforms)
        (let* ((ww (color-picker-w w))
               (bits (color-picker-bits w))
               (cols (expt 2 bits))
               (rows (* cols (floor (wy w) (* cols ww)))))
          (assert (< bits 6))
          (glim:with-draw (:quads :shader :solid)
            (loop for div = (expt 2.0 bits)
                  for c from 0 below (expt 2 (* bits 3))
                  for x1 = (* ww (+ (mod (floor c) cols)
                                    (* cols (floor c (* rows cols)))))
                  for y1 = (* ww (floor (mod c (* rows cols))
                                        cols))
                  for x2 = (+ x1 ww)
                  for y2 = (+ y1 ww)
                  do (color (/ (ldb (byte bits 0) c) div)
                            (/ (ldb (byte bits bits) c) div)
                            (/ (ldb (byte bits (* bits 2)) c) div)
                            1)
                     (vertex x1 y1)
                     (vertex x1 y2)
                     (vertex x2 y2)
                     (vertex x2 y1))))
        (dispatch-draws w)))))

(defmethod redraw-hole ((w binpack2-vis))
  (when (hole w)
    (clear-shapes)
    (let ((*w* w))
      (with-regression-restart (w)
        (b::do-dll/next (hole (hole w))
          (draw-hole hole (aref *flags* 7))
          (when (and (aref *flags* 8) (pwx w))
            (draw-cd hole (pwx w)))
          (when (and (pwx w) (pwy w))
            (let ((l (list :show (pwx w) (pwy w))))
              (unless (equalp l (car (placed w)))
                (push l (placed w))))
            (draw-placements hole (pwx w) (pwy w))))))))

(defmethod mouse ((w binpack2-vis) button state x y)
  (format t "~s ~s~%" button state)
  (setf *esc* nil)
  (when (eql state :down)
    (case button
      (:left-button
       (cond
         ((placing w)
          (setf (pwx w) (ceiling (abs (gmx w)) (spacing w)))
          (setf (pwy w) (ceiling (abs (gmy w)) (spacing w)))

          (redraw-hole w)
          (setf (placing w) nil))
         (t
          (when (and (hole w) (places w))
            (with-regression-restart (w)
              (loop for p in (places w)
                    when (b::point-in-rect p (/ (gmx w) 16)
                                           (/ (gmy w) 16))
                      do (push (list :place
                                     (b::x p) (b::y p)
                                     (b::w p) (b::h p))
                               (placed w))
                         (setf (hole w)
                               (b::remove-quad-from-hole (hole w) p))
                         (loop-finish))
              (when (hole w)
                (b::do-dll/next (h (hole w))
                  (b::check-hole h)))
              (setf (places w) nil
                    (pwx w) nil
                    (pwy w) nil
                    (placing w) t)
              (redraw-hole w))))))
      (:right-button
       (when (shapes w)
         (setf (closed (first (shapes w)))
               t))))))

(defun finish-shape (w &key close)
  (when (shapes w)
    (when close
      (setf (closed (first (shapes w))) t))
    (setf (drawing (first (shapes w))) nil)))

(defun rnd-rgb (&key (alpha 1))
  (let ((a (random (* 2 pi))))
    (list (1+ (sin a))
          (1+ (sin (+ a (* 1/3 pi))))
          (1+ (sin (+ a (* 2/3 pi))))
          alpha)))

(defun clear-shapes ()
  (when *w*
    (setf (shapes *w*) nil)))

(defun add-shape-points (points &key (close t))
  (when *w*
    (finish-shape *w*)
    (push (make-instance 'polyline) (shapes *w*))
    (let ((s (draw-scale *w*)))
      (loop for (x y) on points by #'cddr
            do (vector-push-extend (make-instance 'point :x (* s x) :y (* s y))
                                   (points (first (shapes *w*))))))
    (finish-shape *w* :close close)))

(defun start-polyline (&key (rgba '(1 0 0 1)))
  (when *w*
    (finish-shape *w*)
    (push (make-instance 'polyline :rgba rgba) (shapes *w*))))

(defun add-shape-point (x y)
  (when *w*
    (let ((s (draw-scale *w*)))
      (vector-push-extend (make-instance 'point :x (* s x) :y (* s y))
                          (points (first (shapes *w*)))))))



(defmethod mouse-wheel ((window binpack2-vis) button state x y)
  (format t "wheel ~s ~s~%" button state)
  (if (eql state :up)
      (setf (scale window) (* (scale window) 1.1))
      (setf (scale window) (/ (scale window) 1.1))))

(defun cancel-edit-point (w)
  (setf (edit-point w) nil)
  (when (editing-point w)
    (setf (x (editing-point w)) (car (edit-point-prev w))
          (y (editing-point w)) (cdr (edit-point-prev w))))
  (setf (editing-point w) nil))

(defmethod keyboard-up ((window binpack2-vis) key x y)
  (format t "key up ~s~%" key)
  (case key
    (:key-left-ctrl
     (setf (show-colors window) nil))
    (:key-left-alt
     (when (edit-point window)
       (format t "end edit point~%")
       (cancel-edit-point window)))))


(defmethod keyboard :around ((window binpack2-vis) key x y)
  (cond
    ((and (eql key #\q) *esc*)
     (destroy-window window))
    ((and (eql key #\d) *esc*)
     (pop (shapes window)))
    ((eql key #\esc)
     (cond
       ((and (shapes window) (drawing (car (shapes window))))
        (pop (shapes window)))
       ((edit-point window)
        (cancel-edit-point window)))
     (save-undo window)
     (setf *esc* t))
    (t
     (setf *esc* nil)
     (call-next-method))))

(defmethod keyboard ((window binpack2-vis) key x y)
  (declare (ignore x y))
  (print key)
  (case key
    (:key-left-ctrl (setf (show-colors window) t))
    (:key-left-alt (setf (edit-point window) t))
    (:key-right
     (setf *mouse* nil))
    (:key-left
     (setf *mouse* nil))
    (:key-down
     (setf *mouse* nil))
    (:key-up
     (setf *mouse* nil))

    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))
       (when (>= i 7)
         (redraw-hole window))))
    #++(#\p
        (finish-shape window)
        (setf (tool window) 'polyline)
        (push (make-instance 'polyline :rgba (color-picker-col window))
              (shapes window)))
    (#\p (setf (placing window) (not (placing window))))
    (#\r
     (reset window :replay :saved))
    (#\c
     (when (hole window)
       (b::check-hole (hole window))))
    (#\s
     (save-regression window))
    (#\w
     (setf *wait* 1))
    (#\i
     (reset window)
     (redraw-hole window))
    (#\d
     (loop for s in (shapes window)
           do (print-shape s)))
    (#\l
     (glut:reshape-window 1024 768))

    #++(#\d
        (unless *debug* (setf *debug* 0))
        (setf *debug* (mod (1+ *debug*) 4))
        (format t "debug = ~s~%" *debug*))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod entry ((w binpack2-vis) state)
  (format t "mouse -> ~s~%" state)
  #++(when (eql state :left) (break "enter ~s~%" state))
  (setf *mouse* (eql state :entered)))

(defmethod init-gl ((w binpack2-vis))
  (setf *esc* nil)
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun binpack2-vis (&rest args)
  (glut:display-window (apply #'make-instance 'binpack2-vis args)))

(defun draw-hole (hole &optional draw-subholes)
  (with-polyline (:close t)
    (b::do-dll/next (n (b::h-vertices hole))
      (add-shape-point (b::hv-x n) (b::hv-y n))))
  (when draw-subholes
    (let ((ofs 0))
                                        ;(b::check-hole hole)
      (b::do-dll/next (n (b::h-subholes hole))
        (incf ofs)
        #++
        (with-polyline (:rgba (rnd-rgb) :close t)
          (block nil
            (b::do-hole-vertices/next (v (b::sh-start n) dx dy)
              (add-shape-point (+ ofs (b::hv-x v)) (+ ofs (b::hv-y v)))
              (when (and dy (zerop dy) (b::hv-q v))
                (b::jump (b::hv-q v))
                #++(setf v (b::dll-prev (b::hv-q v)))
                #++(add-shape-point (+ ofs (b::hv-x v)) (+ ofs (b::hv-y v))))
              (when (and dx (zerop dx) (b::hv-n v))
                (b::jump (b::hv-n v))
                #++(setf v (b::dll-prev (b::hv-n v)))
                #++
                (add-shape-point (+ ofs (b::hv-x v))
                                 (+ ofs (b::hv-y v)))))))

        (let* ((c1 (1+ (floor ofs)))
               (col #++(rnd-rgb)
                    (list (ldb (byte 1 0) c1)
                          (ldb (byte 1 1) c1)
                          (ldb (byte 1 2) c1)
                          1))
               (ofs (/ ofs 16)))
          (with-polyline (:rgba col :close nil)
            (loop
              for d across (b::f-d (b::sh-top n))
              for h across (b::f-h (b::sh-top n))
              for x = (b::x d)
              for y1 = (b::y d) then (if (= y2 (b::y d))
                                         (b::y d)
                                         (b::y h))
              for y2 = (b::y h) then (if (= y1 (b::y d))
                                         (b::y h)
                                         (b::y d))
              for gaps across (b::f-gaps (b::sh-top n))
              do (add-shape-point (+ ofs (b::x d)) (+ ofs y1))
                 (add-shape-point (+ ofs x) (+ ofs y2))))
          #++
          (with-polyline (:rgba col :close nil)
            (loop with end = (b::sh-end n)
                  for d across (b::f-d (b::sh-top n))
                  for h across (b::f-h (b::sh-top n))
                  for x = (b::x d)
                  for y1 = (b::y d) then (if (= y2 (b::y d))
                                             (b::y d)
                                             (b::y h))
                  for y2 = (b::y h) then (if (= y1 (b::y d))
                                             (b::y h)
                                             (b::y d))
                  for gap across (b::f-gap (b::sh-top n))
                  when (> x end)
                    do (add-shape-point (+ ofs end) (+ ofs y1))
                    and return nil
                  do (add-shape-point (+ ofs (b::x d)) (+ ofs y1))
                     (when gap
                       (add-shape-point (+ ofs x) (+ ofs (- y2 gap)))
                       (add-shape-point (+ ofs x 1)
                                        (+ ofs (- y2 (* 1/2 gap))))
                       (add-shape-point (+ ofs x -1)
                                        (+ ofs (- y2 (* 1/2 gap)))))
                     (add-shape-point (+ ofs x) (+ ofs y2))))
          #++
          (with-polyline (:rgba (mapcar (lambda (a) (* a 0.7)) col)
                          :close nil)
            (loop with end = (b::sh-end n)
                  for x1 = nil then x
                  for d across (b::f-d (b::sh-top n))
                  for h across (b::f-h (b::sh-top n))
                  for x = (b::x d)
                  for y1 = (b::y d) then (if (= y2 (b::y d)) (b::y d) (b::y h))
                  for y2 = (b::y h) then (if (= y1 (b::y d)) (b::y h) (b::y d))
                  when (and x1 (<= x1 end x))
                    do (add-shape-point (+ ofs end) (+ ofs y1))
                  when (< end x)
                    do (add-shape-point (+ ofs (b::x d)) (+ ofs y1))
                       (add-shape-point (+ ofs (b::x h)) (+ ofs y2))))
          (incf ofs (/ 0.5 16))

          (with-polyline (:rgba col :close nil)
            (loop with end = (b::sh-end n)
                  for d across (b::f-d (b::sh-bottom n))
                  for h across (b::f-h (b::sh-bottom n))
                  for x = (b::x d)
                  for y1 = (b::y h) then (if (= y2 (b::y d)) (b::y d) (b::y h))
                  for y2 = (b::y d) then (if (= y1 (b::y d)) (b::y h) (b::y d))
                  when (> x end)
                    do (add-shape-point (+ ofs end) (+ ofs y1))
                    and return nil
                  do (add-shape-point (+ ofs (b::x d)) (+ ofs y1))
                     (add-shape-point (+ ofs (b::x h)) (+ ofs y2))))

          (with-polyline (:rgba (mapcar (lambda (a) (* a 0.6)) col)
                          :close nil)
            (loop with end = (b::sh-end n)
                  for x1 = nil then x
                  for d across (b::f-d (b::sh-bottom n))
                  for h across (b::f-h (b::sh-bottom n))
                  for x = (b::x d)
                  for y1 = (b::y h) then (if (= y2 (b::y d)) (b::y d) (b::y h))
                  for y2 = (b::y d) then (if (= y1 (b::y d)) (b::y h) (b::y d))
                  when (and x1 (<= x1 end x))
                    do (add-shape-point (+ ofs end) (+ ofs y1))
                  when (< end x)
                    do (add-shape-point (+ ofs (b::x d)) (+ ofs y1))
                       (add-shape-point (+ ofs (b::x h)) (+ ofs y2))))
          (incf ofs (/ -0.5 16)))))))

(defun draw-cd (hole l1)
  (let ((ofs 0))
    (b::do-dll/next (n (b::h-subholes hole))
      (incf ofs)
      (let* ((c1 (1+ (floor ofs)))
             (col (list (ldb (byte 1 0) c1)
                        (ldb (byte 1 1) c1)
                        (ldb (byte 1 2) c1)
                        1)))

        (let* ((l (* l1))
               (c (b::make-c (b::sh-bottom n) l (b::sh-end n)))
               (ofs (/ (+ ofs 2) 16)))
          #++(format t "c = ~s~%"
                     (loop for i across c collect (list (b::x i) (b::y i))))
          (loop for (p1 p2) on (coerce c 'list) by #'cddr
                for i from 0 by (/ 0.3 16)
                do (with-polyline (:rgba col ;(rnd-rgb)
                                   :close nil)
                     (add-shape-point (+ ofs (b::x p1))
                                      (+ ofs (b::y p1) i))
                     #++(add-shape-point (+ ofs (b::x p1) l)
                                         (+ ofs (b::y p1) i))
                     (add-shape-point (+ ofs (b::x p2))
                                      (+ ofs (b::y p2) i))
                     (add-shape-point (+ ofs (b::x p2))
                                      (+ ofs (b::y p2) i 3/16)))
                   (typecase p1
                     (b::point-open
                      (with-polyline (:rgba col ;(rnd-rgb)
                                      :close nil)
                        (add-shape-point (+ ofs (b::x p1) -3/16)
                                         (+ ofs (b::y p1) -3/16))
                        (add-shape-point (+ ofs (b::x p1))
                                         (+ ofs (b::y p1)))
                        (add-shape-point (+ ofs (b::x p1) +3/16)
                                         (+ ofs (b::y p1) -3/16))))
                     (b::point-bottom-left
                      (with-polyline (:rgba col ;(rnd-rgb)
                                      :close nil)
                        (add-shape-point (+ ofs (b::x p1) -3/16)
                                         (+ ofs (b::y p1) 3/16))
                        (add-shape-point (+ ofs (b::x p1))
                                         (+ ofs (b::y p1)))
                        (add-shape-point (+ ofs (b::x p1) +3/16)
                                         (+ ofs (b::y p1) 3/16)))))
                   (when (typep p2 'b::point-open)
                     (with-polyline (:rgba col ;(rnd-rgb)
                                     :close nil)
                       (add-shape-point (+ ofs (b::x p2) -3/16)
                                        (+ ofs (b::y p2) -3/16))
                       (add-shape-point (+ ofs (b::x p2))
                                        (+ ofs (b::y p2)))
                       (add-shape-point (+ ofs (b::x p2) +3/16)
                                        (+ ofs (b::y p2) -3/16)))))
          (let* ((d (b::make-d (b::sh-top n) l (b::sh-end n)
                               (b::sh-falling-corner-p n)))
                 (ofs (max 1/16 (+ ofs 1/16))))
            #++(format t "d = ~s~%"
                       (loop for i across d collect (list (b::x i) (b::y i))))
            (loop for (p1 p2) on (coerce d 'list) by #'cddr
                  for i from 0 by (/ 0.3 16)
                  do (with-polyline (:rgba col ;(rnd-rgb)
                                     :close nil)
                       #++(when (and (typep p2 'b::point-gap)
                                     (b::gaps p2))
                            (add-shape-point (+ ofs (b::x p1))
                                             (+ (- ofs)
                                                (b::y p2)
                                                (- (b::gap p2))
                                                i)))
                       (add-shape-point (+ ofs (b::x p1))
                                        (+ (- ofs) (b::y p1) i))
                       #++(add-shape-point (+ ofs (b::x p1) l)
                                           (+ ofs (b::y p1) i))
                       (when p2
                         (add-shape-point (+ ofs (b::x p2))
                                          (+ (- ofs) (b::y p2) i))
                         (add-shape-point (+ ofs (b::x p2))
                                          (+ (- ofs) (b::y p2) i -4/16))))
                     (when (b::point-open-p p1)
                       (with-polyline (:rgba col ;(rnd-rgb)
                                       :close nil)
                         (add-shape-point (+ ofs (b::x p1) -2/16)
                                          (+ (- ofs) (b::y p1) -2/16))
                         (add-shape-point (+ ofs (b::x p1))
                                          (+ (- ofs) (b::y p1)))
                         (add-shape-point (+ ofs (b::x p1) +2)
                                          (+ (- ofs) (b::y p1) -2/16)))))))))))

(defun draw-placements (hole w h)
  (let* ((ofs 1)
         (in 2/16)
         (c1 (1+ (floor ofs)))
         (col (list (ldb (byte 1 0) c1)
                    (ldb (byte 1 1) c1)
                    (ldb (byte 1 2) c1)
                    1)))
    (let ((px (b::find-all-placements hole w h)))
      (loop for p in px
            for x = (b::x p)
            for y = (b::y p)
            for w = (b::w p)
            for h = (b::h p)
                                        ;for in from in by 1
            for z from 0
            for -in = (- in)
            for +in = in
            for o = (b::intersect-hole-with-quad p)
            ;;for col = '(0 1 0 1)
            do (with-polyline (:rgba col :close t)
                 (add-shape-point (+ x   +in) (+ y   +in))
                 (add-shape-point (+ x w -in) (+ y   +in))
                 (add-shape-point (+ x w -in) (+ y h -in))
                 (add-shape-point (+ x   +in) (+ y h -in)))
               (unless o
                 (break "no intersection hole ~s, p ~s?"
                        hole p))
               (b::do-dll/next (i o)
                 (let ((+in 0 #++(+ 2 +in)))
                   (when (typep i 'b::overlap-span)
                     (with-polyline (:rgba `(,(/ z 8) 1 1 1) :close nil)
                       (let ((x1 (b::hv-x (b::a i)))
                             (y1 (b::hv-y (b::a i)))
                             (x2 (b::hv-x (b::b i)))
                             (y2 (b::hv-y (b::b i)))
                             (d1 (alexandria:clamp (b::start i) 0 1))
                             (d2 (alexandria:clamp (b::end i) 0 1)))
                         (cond
                           ((or (= d1 d2 0))
                            (add-shape-point (+ x1 +in 3/16)
                                             (+ y1 +in 3/16))
                            (add-shape-point (+ x1 +in -3/16)
                                             (+ y1 +in -3/16)))
                           ( (= d1 d2 1)
                             (add-shape-point (+ x2 +in -3/16)
                                              (+ y2 +in 3/16))
                             (add-shape-point (+ x2 +in 3/16)
                                              (+ y2 +in -3/16)))
                           (t
                            (add-shape-point (+ (alexandria:lerp d1 x1 x2) +in)
                                             (+ (alexandria:lerp d1 y1 y2) +in))
                            (add-shape-point (+ (alexandria:lerp d2 x1 x2) +in)
                                             (+ (alexandria:lerp d2 y1 y2) +in)))))))))))
    #++(b::do-dll/next (n (b::h-subholes hole))
         (incf ofs)
         (

          (let* ((c (b::make-c (b::sh-bottom n) w (b::sh-end n)))
                 (d (b::make-d (b::sh-top n) w (b::sh-end n)
                               (b::sh-falling-corner-p n)))
                 (p (b::placing w h c d)))
            (format t "p = ~s~%" p)
            (loop for i in p
                  for in from in by 1
                  for -in = (- in)
                  for +in = in
                  ;;for col = '(0 1 0 1)
                  do (with-polyline (:rgba col :close t)
                       (add-shape-point (+ (b::x i)   +in) (+ (b::y i)   +in))
                       (add-shape-point (+ (b::x i) w -in) (+ (b::y i)   +in))
                       (add-shape-point (+ (b::x i) w -in) (+ (b::y i) h -in))
                       (add-shape-point (+ (b::x i)   +in) (+ (b::y i) h -in)))))))))

(defun set-hole (hole &optional w h)
  (when *w*
    (setf (hole *w*) hole)
    (when w
      (setf (pwx *w*) (max 1 w)))
    (when h
      (setf (pwy *w*) (max 1 h)))
    (redraw-hole *w*)))
#++
(ql:quickload '(binpack parachute
                alexandria sb-cga cl-opengl
                3b-glim/example/s 3b-glim/2d 3bgl-shader))
#++
(binpack2-vis :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(binpack2-vis :pos-x 3100 :pos-y 300 :width 700 :height 700)
#++
(binpack2-vis :pos-x 00 :pos-y 300 :width 700 :height 700)
#++
(glut:show-window)
#++
(clear-shapes)



#++
(defun newest-file (pattern)
  (let ((a (directory pattern))
        (d 0)
        (f nil))
    (loop for x in a
          for xd = (uiop:safe-file-write-date x)
          when (and xd (> xd d))
            do (setf f x d xd))
    f))
#++
(replay-file (newest-file "c:/tmp/binpack/regres*.lisp"))
#++
(replay-file (newest-file "c:/tmp/binpack/test*.BAD.lisp"))

#++
(setf *replays*
      (directory "c:/tmp/binpack/test*.BAD.lisp"))

#++(Setf *replays* nil)
#++
(setf *wait* nil)
#++
(replay-file #P"c:/tmp/binpack/test.11E1424B6EA7EFFB.BAD.lisp")
