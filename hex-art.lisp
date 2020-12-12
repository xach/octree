;;;; hex-art.lisp

(in-package #:octree)

(defun hex-path (centerpoint radius)
  "Create a hexagon path centered at CENTERPOINT."
  (let ((step (* pi 1/3))
        (angle 0))
    (move-to (add centerpoint (apoint angle radius)))
    (dotimes (i 5)
      (incf angle step)
      (line-to (add centerpoint (apoint angle radius))))
    (close-subpath)))

(defun containsp (point box)
  (let ((min (minpoint box))
        (max (maxpoint box)))
    (and (<= (x min) (x point) (x max))
         (<= (y min) (y point) (y max)))))

(defun call-for-hex-centerpoints (fun canvas radius)
  (let* ((3r (* radius 3))
         (margin-box (expand canvas 3r))
         (min (minpoint margin-box))
         (max (maxpoint margin-box))
         (min-x (x min))
         (min-y (y min))
         (max-x (x max))
         (max-y (y max))
         (centerpoint (centerpoint canvas))
         (center-delta (add (apoint 0 radius)
                            (apoint (/ pi 3) radius))))
    (flet ((do-row (point)
             (let* ((start-x (- (x point)
                                (* 3r (ceiling (abs (- (x point) min-x))
                                               3r))))
                    (steps (ceiling (vectometry:width margin-box) 3r)))
               (loop for x from start-x by 3r
                     for p = (point x (y point))
                     repeat steps
                     do (funcall fun p)))))
      (loop for p = centerpoint then (add p center-delta)
              while (< (y p) max-y)
            do (do-row p))
      (setf center-delta (point (x center-delta)
                                (- (y center-delta))))
      (loop for p = (add centerpoint center-delta) then (add p center-delta)
              while (< min-y (y p))
            do (do-row p)))))

(defun ilerper (min max)
  "Return a function that maps values between min and max to a
normalized value between 0 and 1.0."
  (let ((magnitude (- max min)))
    (lambda (value)
      (cond ((<= value min)
             0.0)
            ((<= max value)
             1.0)
            (t
             (float (/ (- value min) magnitude)))))))

(defun range-mapper (input-min input-max output-min output-max)
  (let ((in-magnitude (- input-max input-min))
        (out-magnitude (- output-max output-min)))
    (lambda (value)
      (let ((norm
              (cond ((<= value input-min)
                     0.0)
                    ((<= input-max value)
                     1.0)
                    (t
                     (float (/ (- value input-min) in-magnitude))))))
        (+ output-min (* norm out-magnitude))))))

(defun remap (input-min input-max output-min output-max value)
  (let ((in-magnitude (- input-max input-min))
        (out-magnitude (- output-max output-min)))
    (let ((norm
            (cond ((<= value input-min)
                   0.0)
                  ((<= input-max value)
                   1.0)
                  (t
                   (float (/ (- value input-min) in-magnitude))))))
      (+ output-min (* norm out-magnitude)))))

(defun remap-sine (input-min input-max output-min output-max value)
  (let ((in-magnitude (- input-max input-min))
        (out-magnitude (- output-max output-min)))
    (let ((norm
            (cond ((<= value input-min)
                   0.0)
                  ((<= input-max value)
                   1.0)
                  (t
                   (float (/ (- value input-min) in-magnitude))))))
      (+ output-min (* (sin (* pi norm)) norm out-magnitude)))) )

(defun remapper (ilerp-fun min max)
  (let ((magnitude (- max min)))
    (lambda (value)
      (+ min (* magnitude (funcall ilerp-fun value))))))

(defun revmapper (ilerp-fun min max)
  (let ((magnitude (- max min)))
    (lambda (value)
      (- max (* magnitude (funcall ilerp-fun value))))))

(defun test-hex (file)
  (let* ((canvas (box 0 0 512 512))
         (ds (make-data-stream :width (vectometry:width canvas)
                               :height (vectometry:height canvas)
                               :loopingp t))
         (p (centerpoint canvas))
         (radius 25))
    (declare (ignore ds))
    (with-box-canvas canvas
      (set-fill-color (hsv-color 45 1.0 1.0))
      (clear-canvas)
      (set-line-width 3)
      (set-line-join :round)
      (let* ((ilerp (ilerper 0 (distance p (minpoint canvas))))
             (revmap (remapper ilerp 0.5 1.0))
             (thickmap (revmapper ilerp 2 4)))
        (call-for-hex-centerpoints (lambda (p)
                                     (let ((mag (distance p (centerpoint canvas))))
                                       (set-line-width (funcall thickmap mag))
                                       (hex-path p
                                                 (* radius
                                                    0.9
                                                    (funcall revmap mag))))
                                     (stroke))
                                   canvas
                                   radius))
      (save-png file))
    #+nil
    (output-data-stream ds file)))


(defun test-hex-animation (file)
  (let* ((canvas (box 0 0 512 512))
         (ds (make-data-stream :width (vectometry:width canvas)
                               :height (vectometry:height canvas)
                               :loopingp t))
         (p (centerpoint canvas))
         (radius 15)
         (frames 48)
         (anglestep (/ (* pi 2) frames)))
    (with-box-canvas canvas
      (set-fill-color (hsv-color 45 0.0 0.5))
      (dotimes (i frames)
        (clear-canvas)
        (set-line-width 3)
        (set-line-join :round)
        (let* ((ilerp (ilerper 0 256))
               (revmap (remapper ilerp 0.25 1.0))
               (colormap (revmapper ilerp 0 360))
               (thickmap (revmapper ilerp 1 1))
               (refpoint (add p
                              (apoint (* anglestep i) 100))))
          (call-for-hex-centerpoints (lambda (p)
                                       (with-graphics-state
                                         (let* ((mag (distance p refpoint))
                                                (fill-color
                                                  (hsv-color (funcall colormap mag)
                                                       (funcall ilerp mag)
                                                       0.5))
                                                (stroke-color
                                                  (hsv-color (funcall colormap mag)
                                                             (funcall ilerp mag)
                                                             0.25)))
                                           (set-line-width (funcall thickmap mag))
                                           (set-stroke-color stroke-color)
                                           (set-fill-color fill-color)
                                           (hex-path p
                                                     (* radius
                                                        0.9
                                                        (funcall revmap mag)))
                                           (fill-and-stroke))))
                                     canvas
                                     radius))
        (add-image (vimage-gif-image :delay-time 7) ds)))
    (output-data-stream ds file)))


(defmacro do-hexes ((centerpoint canvas radius) &body body)
  `(call-for-hex-centerpoints (lambda (,centerpoint) ,@body)
                              ,canvas ,radius))

(defun hexleg (radius)
  (let ((halfradius (/ radius 2)))
    (sqrt (- (* radius radius)
             (* halfradius halfradius)))))

(defun jitter (value magnitude)
  (+ value (- magnitude (random (* 2 magnitude)))))

(defun test-hex-overlap-animation (file)
  (let* ((canvas (box 0 0 512 512))
         (ds (make-data-stream :width (vectometry:width canvas)
                               :height (vectometry:height canvas)
                               :loopingp t))
         (radius 48)
         (r/2 (/ radius 2))
         (j (/ radius 20.0))
         (frames 48)
         (red (hsv-color 0 1 1))
         (green (hsv-color 120 1 1))
         (blue (hsv-color 240 1 1)))
    (with-box-canvas canvas
      (setf (vecto::blend-style vecto::*graphics-state*) :add)
      (dotimes (i frames)
        (let ((horizontal-displacement (remap-sine 0 frames
                                                   0 (* 3 radius)
                                                   i))
              (vertical-displacement (remap-sine 0 frames
                                                 0 (* 2 (hexleg radius))
                                                 i)))
          (set-fill-color *black*)
          (clear-canvas)
          (set-line-width 2)
          (do-hexes (hex canvas radius)
            (set-fill-color red)
            (centered-circle-path (add hex
                                       (point (jitter 0 j)
                                              (jitter vertical-displacement j)))
                                  r/2)
            (fill-path))
          (do-hexes (hex canvas radius)
            (set-fill-color green)
            (centered-circle-path (add hex
                                       (point (jitter 0 j)
                                              (jitter (- vertical-displacement) j)))
                                  r/2)
            (fill-path))
          (do-hexes (hex canvas radius)
            (set-fill-color blue)
            (centered-circle-path (add hex
                                       (point (jitter horizontal-displacement j)
                                              (jitter 0 j)))
                                  r/2)
            (fill-path)))
        (add-image (vimage-gif-image :delay-time (+ 7 (if (zerop i) 25 0))) ds)))
    (output-data-stream ds file)))
