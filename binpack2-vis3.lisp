(defpackage #:binpack2-vis3
  (:use :cl #:3b-glim-example/s #:3b-glim-example/s-shaders)
  (:local-nicknames (#:glim #:3b-glim/s)
                    (#:2d #:3b-glim/2d)
                    (#:a #:alexandria-2)
                    (#:b #:binpack)))

(in-package binpack2-vis3)

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



(defvar *replays* nil)


(defclass binpack2-vis3 (scratchpad)
  ((scale :initform 1 :accessor scale)
   (draw-scale :initform 16 :accessor draw-scale)
   (spacing :initform 16 :accessor spacing)
   (origin :initform :center :accessor origin)
   (x1 :initform 0 :accessor x1)
   (x2 :initform 0 :accessor x2)
   (y1 :initform 0 :accessor y1)
   (y2 :initform 0 :accessor y2)
   (gmx :initform 0 :accessor gmx)
   (gmy :initform 0 :accessor gmy)
   (snap :initform t :accessor snap)
   (packing :initform nil :accessor packing)
   (playing :initform nil :accessor playing)
   (index :initform 0 :accessor index))

  (:default-initargs :shaders '((:tex :vertex vertex/simple
                                      :fragment frag/tex)
                                (:solid :vertex vertex/simple
                                        :fragment frag/solid))))

(defvar *w* nil)

(defun rnd-rgb (&key (alpha 1))
  (let ((a (random (* 2 pi))))
    (list (1+ (sin a))
          (1+ (sin (+ a (* 1/3 pi))))
          (1+ (sin (+ a (* 2/3 pi))))
          alpha)))

(defun random-rgb (&key (alpha 1))
  (list (+ 0.25 (random 0.75))
        (+ 0.25 (random 0.75))
        (+ 0.25 (random 0.75))
        alpha))

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

(defmethod display ((w binpack2-vis3) now)
  (setf *w* w)
  (when (and *replays*
             (not (playing w))
             #++(or (not (packing w))
                 (>= (index w)
                     (length (packing w)))))
    (setf (packing w) (sb-ext:atomic-pop *replays*)
          (index w) (get-internal-real-time)
          (playing w) t))
  (let ((*random-state* (sb-ext:seed-random-state 123)))
    (glim:with-state (*format*)

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
        (when (and (packing w)
                   (not (aref *flags* 3)))
          (destructuring-bind (&key (t1 0)
                                 ((:w wx) 11) ((:h wy) 11)
                                 (p 0) (v #()))
              (packing w)
            (declare (ignore v t1))
            (let ((px (+ wx (* (+ 100 wx) (mod (1- p) (ceiling (sqrt p))))))
                  (py (+ wy (* (+ 100 wy) (floor (1- p) (ceiling (sqrt p)))))))
              (setf scale (min (/ (* 1.8 (wx w)) px)
                               (/ (* 1.8 (wy w)) py))))))
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
            (incf x1 (/ (wx w) 10/9 scale))
            (incf y1 (/ (wy w) 10/9 scale))
            (incf x2 (/ (wx w) 10/9 scale))
            (incf y2 (/ (wy w) 10/9 scale))
            (glim:translate -0.9 -0.9 0))
          #++(when (aref *flags* 1)
               (incf x1 (/ (wx w) 2 scale))
               (incf y1 (/ (wy w) 2 scale))
               (incf x2 (/ (wx w) 2 scale))
               (incf y2 (/ (wy w) 2 scale))
               (glim:translate -0.5 -0.5 0))
          #++
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
          (when (packing w)
            (destructuring-bind (&key (t1 0)
                                   ((:w wx) 11) ((:h wy) 11)
                                   (p 0) (v #()))
                (packing w)
              (glim:with-draw (:lines :shader :solid)
                (loop for i below p
                      for px = (* (+ wx 100)
                                  (mod i (ceiling (sqrt p))))
                      for py = (* (+ wy 100)
                                  (floor i (ceiling (sqrt p))))
                      do (color (ldb (byte 1 0) i)
                                (ldb (byte 1 1) i)
                                (ldb (byte 1 2) i)
                                1)
                         (color (random 2) (random 2) (random 2) 1)
                         (vertex px py) (vertex (+ px wx) py)
                         (vertex (+ px wx) py) (vertex (+ px wx) (+ py wy))
                         (vertex (+ px wx) (+ py wy)) (vertex px (+ py wy))
                         (vertex px (+ py wy)) (vertex px py)))
              
              (glim:with-draw (:quads :shader :solid)
                (setf (playing w) nil)
                (loop with dt = (- (get-internal-real-time)
                                      (index w))
                      for (.x1 .y1 rwx rwy i tn) across v
                      for px = (* (+ wx 100)
                                  (mod i (ceiling (sqrt p))))
                      for py = (* (+ wy 100)
                                  (floor i (ceiling (sqrt p))))
                      for x1 = (+ .x1 px)
                      for y1 = (+ .y1 py)
                      for x2 = (+ x1 rwx)
                      for y2 = (+ y1 rwy)
                      for playing = (and tn
                                         (< (* (if (aref *flags* 4)
                                                   20
                                                   1)
                                               (- tn t1))
                                            dt))
                      do (apply #'color (random-rgb :alpha 0.75))
                         (vertex x1 y1)
                         (vertex x2 y1)
                         (vertex x2 y2)
                         (vertex x1 y2)
                         (setf (playing w) (not playing))
                      while playing))))
          (dispatch-draws w))))))


#++
(defmethod mouse ((w binpack2-vis3) button state x y)
  (format t "~s ~s~%" button state)
  (setf *esc* nil)
  (when (eql state :down)
    (case button
      (:left-button
       )
      (:right-button
       ))))


(defmethod mouse-wheel ((window binpack2-vis3) button state x y)
  (format t "wheel ~s ~s~%" button state)
  (if (eql state :up)
      (setf (scale window) (* (scale window) 1.1))
      (setf (scale window) (/ (scale window) 1.1))))

#++
(defmethod keyboard-up ((window binpack2-vis3) key x y)

  )


(defmethod keyboard :around ((window binpack2-vis3) key x y)
  (cond
    ((and (eql key #\q) *esc*)
     (destroy-window window))
    ((eql key #\esc)
     (setf *esc* t))
    (t
     (setf *esc* nil)
     (call-next-method))))

(defmethod keyboard ((window binpack2-vis3) key x y)
  (declare (ignore x y))
  (print key)
  (case key
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
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\r
     (setf (packing window) nil
           (index window) 0))
    (#\l
     (glut:reshape-window 1280 700))

    (#\space
     (setf (index window) 0))
    #++(#\d
        (unless *debug* (setf *debug* 0))
        (setf *debug* (mod (1+ *debug*) 4))
        (format t "debug = ~s~%" *debug*))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod entry ((w binpack2-vis3) state)
  (format t "mouse -> ~s~%" state)
  #++(when (eql state :left) (break "enter ~s~%" state))
  (setf *mouse* (eql state :entered)))

(defmethod init-gl ((w binpack2-vis3))
  (setf *esc* nil)
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun binpack2-vis3 (&rest args)
  (glut:display-window (apply #'make-instance 'binpack2-vis3 args)))

#++
(ql:quickload '(binpack parachute
                alexandria sb-cga cl-opengl
                3b-glim/example/s 3b-glim/2d 3bgl-shader))
#++
(binpack2-vis3 :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(binpack2-vis3 :pos-x 3100 :pos-y 300 :width 700 :height 700)
#++
(binpack2-vis3 :pos-x 00 :pos-y 300 :width 700 :height 700)
#++
(glut:show-window)
