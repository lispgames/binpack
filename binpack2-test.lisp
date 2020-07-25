(in-package #:binpack)

;; random test generator for binpack2 stuff

;; pick a random seed for use in following randomness
;;   (universal time, internal time,or just random?)
;; generate random box to place
;; list placements
;; compare to brute-force serach
;; pick random placement / try removal
;; repeat until n% filled (or N boxes with no placements)
;; then repeat with smaller random ranges until 100% full

;; on error, or on full, save seed, size and list of placements to a
;; file with name indicating if it failed

(defun brute-force-placements (holes w h)
  (assert (not (zerop w)))
  (assert (not (zerop h)))
  (let ((r (make-hash-table :test 'equalp)))
    (do-dll/next (hole holes)
      (with-minmax (xmin xmax mmx)
        (with-minmax (ymin ymax mmy)
          (do-dll/next (v (h-vertices hole))
            (mmx (hv-x v))
            (mmy (hv-y v)))
          (loop for i from xmin upto (- xmax w)
                do (loop for j from ymin upto (- ymax h)
                         when (valid-placement-p hole w h i j)
                           do (setf (gethash (list i j w h) r) t))))))
    r))

(defun bfp/f-1 (hole w h r)
  (do-dll/next (v (h-vertices hole))
    (let* ((p (dll-next v))
           (dx (- (hv-x p) (hv-x v))))
      (unless (zerop dx)
        (assert (= (hv-y p) (hv-y v))))
      ;; can only place on a leftwards edge
      (when (minusp dx)
        (loop with y = (hv-y v)
              for x from (- (hv-x p) w) below (+ (hv-x v) w)
              when (valid-placement-p hole w h x y)
                do (setf (gethash (list x y w h) r) t))))))


(defun brute-force-placements/fast (holes w h)
  (assert (not (zerop w)))
  (assert (not (zerop h)))
  (let ((r (make-hash-table :test 'equalp)))
    (do-dll/next (hole holes)
      (bfp/f-1 hole w h r))
    r))

#++
(brute-force-placements (init-hole 16 16) 5 1)
#++(find-all-placements (init-hole 16 16) 5 1)

(defvar *good-seeds* (make-hash-table))
(defvar *bad-seeds* (make-hash-table))

(defparameter *exit* t)

(defun run-binpack2-test (&key (w 21) (h 21) (seed (random (expt 2 64))))
  #++(format t "~&:seed #x~16,'0x~%" seed)
  (let* ((*random-state* (sb-ext:seed-random-state seed))
         (hole (init-hole w h))
         (placed (list `(:init ,w ,h)))
         (ok nil)
         (rw (floor w 4))
         (rh (floor h 4))
         (no-fit 0)
         (max-no-fit 5)
         (tmp (make-hash-table :test 'equalp)))
    (multiple-value-bind (r e)
        (ignore-errors
         (loop for wx = (1+ (random (1- rw)))
               for wy = (1+ (random (1- rh)))
               do (push (list :show wx wy) placed)
                  (let ((bf (brute-force-placements/fast hole wx wy))
                        (px (find-all-placements hole wx wy)))
                    (unless (= (hash-table-count bf) (length px))
                      (error "brute force found ~s placements, binpack found ~s?"
                             (hash-table-count bf) (length px)))
                    (clrhash tmp)
                    (loop for p in px
                          do (assert (valid-placement-p* p)))
                    (loop for p in px
                          for l = (list  (x p) (y p) (w p) (h p))
                          do (assert (gethash l bf)
                                     nil
                                     "~s found by binpack but not brute force"
                                     l)
                             (setf (gethash l tmp)
                                   (intersect-hole-with-quad p)))
                    (loop for k being the hash-keys of bf
                          do (assert (gethash k tmp)
                                     nil
                                     "~s found by brute force but not binpack"
                                     k))

                    (cond
                      ((zerop (hash-table-count bf))
                       (incf no-fit)
                       (when (and (= rw 1) (= rh 1))
                         (error "failed to place 1x1?"))
                       (when (> no-fit max-no-fit)
                         (setf no-fit 0)
                         (setf rw (max 2 (ceiling rw 2)))
                         (setf rh (max 2 (ceiling rh 2)))))
                      (t
                       (let* ((p (alexandria:random-elt px))
                              (l (list (x p) (y p) (w p) (h p))))
                         (push (list* :place l) placed)
                         (setf hole (remove-quad-from-hole
                                     (hole p) p
                                     :overlap (gethash l tmp)))
                         (when (not hole)
                           (setf ok t)
                           (loop-finish))
                         (when (or (not (dll-next hole))
                                   (not (dll-prev hole)))
                           (break "got bad dll links in hole? ~s" hole))
                         (do-dll/next (h hole)
                           (check-hole h))))))))
      (declare (ignore r))
      #++(if ok
             (with-open-file (f1 "/tmp/binpack/good-seeds.txt"
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :append)
               (format f1 "~16,'0x~%" seed))
             (with-open-file (f1 "/tmp/binpack/bad-seeds.txt"
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :append)
               (format f1 "~16,'0x~%" seed)))
      (unless ok
        (with-open-file (f1 "/tmp/binpack/bad-seeds.txt"
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :append)
          (format f1 "~16,'0x~%" seed)))

      #++(if ok
             (incf (gethash seed *good-seeds* 0))
             (incf (gethash seed *bad-seeds* 0)))
      (unless ok
        (with-open-file (f1 (format nil "/tmp/binpack/test.~16,'0x.~a.lisp" seed
                                    (if ok "OK" "BAD"))
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
          (let ((f (make-broadcast-stream f1 *standard-output*)))
            (when e
              (format t "~&~@<;;;~@; ~a~%~:>" e))
            (write (reverse placed) :stream f :readably t))))
      (when (or (not ok)
                (not binpack2-vis::*replays*))
        (#-sbcl push
         #+sbcl sb-ext:atomic-push
         (reverse placed) binpack2-vis::*replays*))
      (when (not OK)
        (setf *exit* t)
        (format t "~&~%~% @@@@@@@ ~a~%" e)
        #++(break "ff ~a" e)))))

(defvar *tests* (cons 0 0))
#++
(incf (cdr *tests*) (shiftf (car *tests*) 0))
(setf *exit* t)
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
#++
(run-binpack2-test :seed 14472710023339531888)
#++
(run-binpack2-test :seed  #x17576D7DB9B9E873)
#++
(loop for i in '(#x6CBD6641E618AB1F
                 #x0570161EDE120180
                 #xA6432A16F481ED99
                 #x17576D7DB9B9E873
                 #x5472A53E30FA3B64
                 #x4373A0760D5DF80E
                 #xA6432A16F481ED99
                 #x8C787122F6524DAA
                 #x32FAE81F332EF2B1
                 #xDABE0F1F28A8EEA8)
      do (run-binpack2-test :seed i))

#++(alexandria:hash-table-keys *bad-seeds*)
#++
'(
  831046288634743980
  17055507203831357804
  14154731064795601613
  3017332201790830780
  3913720303626237533
  2477458706962062508
  9799081945024727010
  7924065659042252561
  17224828980458174732
  6759562583343026413
  13331914532251852307
  10137225725825241658
  190676904605619078
  5422845697868918910
  1000541890507753404
  10312330359039212967
  5558115861008568655
  15844624481434766675
  10422033531626872018
  913714912618076019
  590130149089506818
  15041423711168915455
  13583076918816467943
  11793316356006354707
  16047579473308594548
  10082863248321994818
  6292638295932804558
  15576285998597020715
  3297654077572988203
  6343411172282833006
  12932989368861382956
  17390427894665691475
  6262548162620425495
  3083712972247263665
  11584063820759865392
  14819457401519905154
  7976215471899523936
  3374323980364780765
  8464266912148868969
  8095722066905342270)


#+:seed #x547EBB86A05737F0

#++
(let ((w 3)
      (h 2))
  (loop for x from 0 below 16
        do (loop for y from 0 below 16
                 for v = (valid-placement-p (binpack2-vis::hole binpack2-vis::*w*)
                                            w h x y)
                 when v do (format t "~&~%~s,~s @ ~s,~s?~%"
                                   w h x y))))
