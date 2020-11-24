(in-package #:binpack/2)

;; benchmarking code for binpack2 stuff

;; generate a set of rects to be placed taking up WxH
(defun gen-test (w h)
  ;; todo: other distributions
  (let ((a 0)
        (max (* w h))
        (w1 (max 1 (min (floor w 3) 15)))
        (h1 (max 1 (min (floor h 3) 15))))
    (loop while (< a max)
          for x = (+ 3 (random w1))
          for y = (+ 3 (random h1))
          collect (list x y)
          do (incf a (* x y)))))

(defun gen-test2 (w h)
  ;; todo: other distributions
  (let ((a 0)
        (max (* w h))
        (w1 20.0)
        (h1 8.0))
    (loop while (< a max)
          for s = (expt 1.25 (1+ (random w1)))
          for x = (ceiling (+ 2 (random s)))
          for y = (ceiling (+ 2 (random s)))
          collect (list x y)
          do (incf a (* x y)))))

(defun random-placement (l)
  (list (alexandria:random-elt l)))


(defun xy> (a b)
  (if (= (car a) (car b))
      (> (cadr a) (cadr b))
      (> (car a) (car b))))
(defun xy< (a b)
  (if (= (car a) (car b))
      (< (cadr a) (cadr b))
      (< (car a) (car b))))
(defun yx> (a b)
  (if (= (cadr a) (cadr b))
      (> (car a) (car b))
      (> (cadr a) (cadr b))))

(defun xy<> (a b)
  (if (= (car a) (car b))
      (> (cadr a) (cadr b))
      (< (car a) (car b))))

(defun mxy> (a b)
  (if (= (max (car a) (cadr a))
         (max (car b) (cadr b)))
      (> (min (car a) (cadr a))
         (min (car b) (cadr b)))
      (> (max (car a) (cadr a))
         (max (car b) (cadr b)))))

(defun a> (a b)
  (let ((a1 (* (car a) (cadr a)))
        (a2 (* (car b) (cadr b))))
    (if (= a1 a2)
        (xy> a b)
        (> a1 a2))))

(defun run1 (wh &key sort)
  (let* ((w1 wh)
         (h1 wh)
         (hole (init-hole w1 h1))
         (a 0)
         (c 0)
         (tx (gen-test w1 h1))
         (r (list (list :init w1 h1))))
    (time
     (progn
       (time
        (when sort
          (setf tx (sort tx (if (eql sort t) #'xy> sort)))))
       (loop with x = nil
             with y = nil
             for (w h) in tx
             do (setf (values hole x y)
                      (place hole w h #+:heuristic1 'random-placement
                                      #+:HEURISTIC1 'minimize-contact
                                      :heuristic1 'smallest-hole
                                      :heuristic2 'maximize-contact))
                (push (list :show w h) r)
             when x
               do (incf a (* w h))
                  (incf c)
                  (push (list :place x y w h) r)
             while hole)))
    (sb-ext:atomic-push (nreverse r) binpack2-vis::*replays*)
    (format t "placed ~s/~s @ ~s%~%"
            c (length tx) (* 100.0 (/ a (* w1 h1))))))

(defun run2 (w1 h1 tx &key sort shape)
  (let* ((hole (init-hole w1 h1))
         (a 0)
         (c 0)
         (r (make-array 32 :adjustable t :fill-pointer 0))
         (mx 0)
         (my 0))
    (time
     (progn
       (time
        (when sort
          (setf tx (sort tx (if (eql sort t) #'xy> sort)))))
       (loop with x = nil
             with y = nil
             for (w h) in tx
             do (setf (values hole x y)
                      (place hole w h #+:heuristic1 'random-placement
                                      #+:HEURISTIC1 'minimize-contact
                                        ;:heuristic1 'smallest-hole
                                        ;:heuristic2 'maximize-contact
                                      :shaping shape))
             when x
               do (incf a (* w h))
                  (incf c)
                  (vector-push-extend (list x y w h) r)
                  (setf mx (max mx (+ x w)))
                  (setf my (max my (+ y h)))
             while hole)))
    (sb-ext:atomic-push  r binpack2-vis2::*replays*)
    (format t "placed ~s/~s @ ~s%~%"
            c (length tx) (* 100.0 (/ a (* w1 h1))))
    (format t " final size = ~sx~s @ ~s%~%"
            mx my (* 100.0 (/ a (* mx my))))))


(defun bench-file (fn)
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (bin)
          (placed 0)
          (unplaced 0)
          (i 0))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf bin (init-hole w h))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (multiple-value-bind (h x y)
                   (place bin rw rh)
                 (declare (ignore y))
                 (setf bin h)
                 (if x
                     (incf placed)
                     (incf unplaced))
                 (incf i)
                 (when (zerop (mod i 100))
                   (let ((t2 (get-internal-real-time)))
                     (format t "~s = ~sms~%"
                             i
                             (floor (- t2 start)
                                    (/ internal-time-units-per-second
                                       1000)))))))
      (let ((t2 (get-internal-real-time)))
        (format t "~s done @ ~sms~%"
                i
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))
      (format t "packed ~s rects, ~s unpacked~%" placed unplaced))))

(defun bench-file/a (fn)
  (alexandria:with-input-from-file (f fn)
    (let* ((w (* 2 (read f)))
           (h (* 2 (read f)))
           (start)
           (bin)
           (placed 0)
           (unplaced 0)
           (i 0)
           (mx 0)
           (my 0)
           (shape (make-instance 'shaping-quantized
                                 :w  (floor w 2) :h (floor h 2)
                                 :dx 1 :dy 1)))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf bin (init-hole w h))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (multiple-value-bind (h x y)
                   (place bin rw rh :shaping shape)
                 (setf bin h)
                 (setf mx (max mx (+ x rw)))
                 (setf my (max my (+ y rh)))
                 (if x
                     (incf placed)
                     (incf unplaced))
                 (incf i)
                 (when (zerop (mod i 100))
                   (let ((t2 (get-internal-real-time)))
                     (format t "~s = ~sms~%"
                             i
                             (floor (- t2 start)
                                    (/ internal-time-units-per-second
                                       1000)))))))
      (let ((t2 (get-internal-real-time)))
        (format t "~s done @ ~sms~%"
                i
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))
      (format t "packed ~s rects, ~s unpacked~%" placed unplaced)
      (format t " final size = ~sx~s~%" mx my))))

(defun bench-file/1 (fn)
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (bin)
          (placed 0)
          (unplaced 0)
          (i 0))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf bin (start-pack w h))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (let ((ok (ignore-errors (pack-1 (rect nil 0 0 rw rh) bin))))
                 (if ok
                     (incf placed)
                     (incf unplaced))
                 (incf i)
                 (when (zerop (mod i 100))
                   (let ((t2 (get-internal-real-time)))
                     (format t "~s = ~sms~%"
                             i
                             (floor (- t2 start)
                                    (/ internal-time-units-per-second
                                       1000)))))))
      (let ((t2 (get-internal-real-time)))
        (format t "~s done @ ~sms~%"
                i
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))
      (format t "packed ~s rects, ~s unpacked~%" placed unplaced))))

(defun bench-file/s (fn)
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (bin)
          (placed 0)
          (unplaced 0)
          (i 0)
          (v (make-array 1 :adjustable t :fill-pointer 0)))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf bin (init-hole w h))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (vector-push-extend (cons rw rh) v))

      (let ((t2 (get-internal-real-time)))
        (format t "load+sort ~sms~%"
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))
      (sort v (lambda (a b)
                (if (= (car a) (car b))
                    (> (cdr a) (cdr b))
                    (> (car a) (car b)))))
      (loop for (rw . rh) across v
            do (multiple-value-bind (h x y)
                   (place bin rw rh)
                 (declare (ignore y))
                 (setf bin h)
                 (if x
                     (incf placed)
                     (incf unplaced))
                 (incf i)
                 (when (zerop (mod i 100))
                   (let ((t2 (get-internal-real-time)))
                     (format t "~s = ~sms~%"
                             i
                             (floor (- t2 start)
                                    (/ internal-time-units-per-second
                                       1000)))))))
      (let ((t2 (get-internal-real-time)))
        (format t "~s done @ ~sms~%"
                i
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))
      (format t "packed ~s rects, ~s unpacked~%" placed unplaced))))



#++
(bench-file/a "e:/tmp/binpackb6s.txt")
#++
(bench-file/1 "e:/tmp/binpackb5s2.txt")
#++
(bench-file/1 "d:/tmp/rects.noto")
#++
(bench-file/s "e:/tmp/binpackb3.txt")

(defun load-file (fn)
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f))
          (h (read f))
          (v (make-array 1 :adjustable t :fill-pointer 0)))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (vector-push-extend (list rw rh) v))
      (list w h (coerce v 'list)))))

#++
(apply #'run2 (load-file "d:/tmp/rects.noto"))
#++
(run2 9096 9096
      #++(gen-test 2048 2048)
      (third (load-file "e:/tmp/binpackb9s.txt"))
                                        ;:sort 'xy>
      :shape (make-instance
              'shaping-quantized
              :w 2048 :h 2048
              :dx 4 :dy 1))

#++
(let* ((f1 "e:/tmp/binpackb9")
       (w 2048)
       (h 2048)
       (r (gen-test2 w h)))
  (alexandria:with-output-to-file (f (format nil "~a.txt" f1))
    (format f "~s ~s~%" w h)
    (loop for (x y) in r
          do (format f "~s ~s~%" x y)))
  (setf r (sort r 'xy>))
  (alexandria:with-output-to-file (f (format nil "~as.txt" f1))
    (format f "~s ~s~%" w h)
    (loop for (x y) in r
          do (format f "~s ~s~%" x y)))
  (setf r (sort r 'xy<))
  (alexandria:with-output-to-file (f (format nil "~as2.txt" f1))
    (format f "~s ~s~%" w h)
    (loop for (x y) in r
          do (format f "~s ~s~%" x y))))

#++
(run 400 :sort #'xy>)
#++(run 800 :sort #'yx>)
#++(setf *exit* t)
#++
(progn
  (setf *exit* nil)
  (time
   (loop for i from 0
         until *exit*
         do #++ (format t "~s~%" i)
         #+sbcl (sb-ext:atomic-incf (car *tests*))
                (run-binpack2-test))))
#++
(progn
  (setf *exit* nil)
  (loop until *exit*
        for x = (car *tests*)
        do (sleep 1)
           (let ((y (car *tests*)))
             (format t "~s (~s / sec)~%" y (- y x)))))
#++
(setf *break-on-signals* t)
