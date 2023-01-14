(in-package #:binpack/2)

;; some of binpack2-bench rewritten to use full API so packer is selectable

#++
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
                                      #+:heuristic1 'minimize-contact
                                      :heuristic1 'smallest-hole
                                      :heuristic2 'maximize-contact))
                (push (list :show w h) r)
             when x
               do (incf a (* w h))
                  (incf c)
                  (push (list :place x y w h) r)
             while hole)))
    #++    (sb-ext:atomic-push (nreverse r) binpack2-vis::*replays*)
    (format t "placed ~s/~s @ ~s%~%"
            c (length tx) (* 100.0 (/ a (* w1 h1))))))

#++
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
                                      #+:heuristic1 'minimize-contact
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
    #++(sb-ext:atomic-push  r binpack2-vis2::*replays*)
    (format t "placed ~s/~s @ ~s%~%"
            c (length tx) (* 100.0 (/ a (* w1 h1))))
    (format t " final size = ~sx~s @ ~s%~%"
            mx my (* 100.0 (/ a (* mx my))))))


(defun bench-file2 (fn &key (algorithm :chazelle))
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (pack nil)
          (placed 0)
          (unplaced 0)
          (i 0))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf pack (start-pack w h :packer algorithm))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (let ((r (pack-1/@ (rect* rw rh) pack 0)))
                 (if (x r)
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

(defun bench-file/2a (fn &key (algorithm :chazelle))
  (alexandria:with-input-from-file (f fn)
    (let* ((w (* 2 (read f)))
           (h (* 2 (read f)))
           (start)
           (pack)
           (placed 0)
           (unplaced 0)
           (placed-area 0)
           (i 0)
           (mx 0)
           (my 0)
           (shape (make-instance 'shaping-quantized
                                 :w  (floor w 2) :h (floor h 2)
                                 :dx 1 :dy 1)))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf pack (start-pack w h :packer algorithm
                                 :growth-policy shape))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (let ((r (pack-1/@ (rect* rw rh) pack 0)))
                 (setf mx (max mx (+ (x r) rw)))
                 (setf my (max my (+ (y r) rh)))
                 (if (x r)
                     (progn
                       (incf placed)
                       (incf placed-area (* rw rh)))
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
      (format t " final size = ~sx~s~%" mx my)
      (format t "  = ~s (placed = ~s)~%" (* mx my) placed-area))))

(defun bench-file/21 (fn &key (algorithm :chazelle))
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (pack)
          (placed 0)
          (unplaced 0)
          (i 0))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf pack (start-pack w h :packer algorithm))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (let ((ok (x (pack-1/@ (rect nil 0 0 rw rh) pack 0))))
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

(defun bench-file/2s (fn &key (algorithm :chazelle))
  (alexandria:with-input-from-file (f fn)
    (let ((w (read f)) (h (read f))
          (start)
          (pack)
          (placed 0)
          (unplaced 0)
          (i 0)
          (v (make-array 1 :adjustable t :fill-pointer 0)))
      (format t "initializing bin to size ~s x ~s~%" w h)
      (setf start (get-internal-real-time))
      (setf pack (start-pack w h :packer algorithm))
      (loop for rw = (read f nil nil)
            for rh = (read f nil nil)
            while (and rw rh)
            do (vector-push-extend (cons rw rh) v))

      (setf v (sort v (lambda (a b)
                        #++(> (* (car a) (cdr a))
                              (* (car b) (cdr b)))
                        #++
                        (> (+ (car a) (cdr a))
                           (+ (car b) (cdr b)))

                        (if (= (car a) (car b))
                            (> (cdr a) (cdr b))
                            (> (car a) (car b)))
                        #++
                        (if (= (cdr a) (cdr b))
                            (> (car a) (car b))
                            (> (cdr a) (cdr b))))))
      (format t "-> ~s~%" (subseq v 0 123))
      (let ((t2 (get-internal-real-time)))
        (format t "load+sort ~sms~%"
                (floor (- t2 start)
                       (/ internal-time-units-per-second
                          1000))))

      (loop for (rw . rh) across v
            do (let ((r (pack-1/@ (rect* rw rh) pack 0)))
                 (if (x r)
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
(bench-file/2a "e:/tmp/binpackb6s.txt")
#++
(bench-file/2a "e:/tmp/binpackb6s.txt" :algorithm :maxrects2)
#++
(bench-file/21 "e:/tmp/binpackb5s2.txt")
#++
(bench-file/21 "e:/tmp/binpackb5s2.txt" :algorithm :maxrects2)
#++
(bench-file/21 "d:/tmp/rects.noto")
#++
(bench-file/21 "d:/tmp/rects.noto" :algorithm :maxrects2)
#++
(bench-file/2s "e:/tmp/binpackb3.txt")
#++
(bench-file/2s "e:/tmp/binpackb3.txt" :algorithm :maxrects2)
#++
(bench-file/2s "d:/tmp/rects.noto")
#++
(bench-file/2s "d:/tmp/rects.noto" :algorithm :maxrects2)
#++
(bench-file/2s "e:/tmp/binpackb3.txt" :algorithm :maxrects2)

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
