;;;; text-art.lisp

(in-package #:octree)

(defun text-bounding-box (font text size)
  (zpb-ttf:with-font-loader (loader font)
    (string-bounding-box text size loader)))

(defvar *current-gif*)
(defmacro with-animated-gif ((canvas &key to (loopingp t)) &body body)
  (let ((c (gensym)))
    `(let* ((,c ,canvas)
            (*current-gif*
              (make-data-stream :width (ceiling (vectometry:width ,c))
                                :height (ceiling (vectometry:height ,c))
                                :loopingp ,loopingp)))
       (progn ,@body)
       ,@ (when to
            `((output-data-stream *current-gif* ,to))))))

(defun save-frame (&key (delay 5))
  (add-image (vimage-gif-image :delay-time delay)
             *current-gif*))


(defun jitter-text (text file &key (size 60) (font "~/futuran.ttf"))
  (let* ((box (text-bounding-box font text size))
         (canvas (expand box (/ size 4)))
         (frames 48))
    (with-box-canvas canvas
      (let ((*font-loader* (get-font font)))
        (with-animated-gif (canvas :to file)
          (set-font (get-font font) size)
          (dotimes (i frames)
            (set-fill-color *black*)
            (clear-canvas)
            (dolist (color (list (rgba-color 1 0 0 1)
                                 (rgba-color 0 1 0 1)
                                 (rgba-color 0 0 1 1)))
              (set-fill-color color)
              (draw-string (point (jitter 0 (remap-sine 0 frames
                                                        1 5
                                                        i))
                                  (jitter 0 (remap-sine 0 frames
                                                        1 5
                                                        i)))
                           text))
            (save-frame)))))))

(defun jitter-stack (text file &key (width 400) (font "~/futuran.ttf"))
  (zpb-ttf:with-font-loader (*font-loader* font)
    (let* ((stack (stringstack width text))
           (box (bounding-box stack))
           (canvas (expand box (/ width 8)))
           (frames 48))
      (with-box-canvas canvas
        (setf (vecto::blend-style vecto::*graphics-state*) :add)
        (with-animated-gif (canvas :to file)
          (dotimes (i frames)
            (flet ((jitterpoint ()
                     (point (jitter 0 (remap-sine 0 frames
                                                  (/ width 400.0) (/ width 40.0)
                                                  i))
                            (jitter 0 (remap-sine 0 frames
                                                  (/ width 400.0) (/ width 40.0)
                                                  i)))))
              (set-fill-color *black*)
              (clear-canvas)
              (dolist (color (list (rgba-color 1 0 0 1)
                                   (rgba-color 0 1 0 1)
                                   (rgba-color 0 0 1 1)))
                (set-fill-color color)
                (dolist (stringbox (elements stack))
                  (set-font (font stack) (font-size stringbox))
                  (draw-string (add (jitterpoint) (displacement stringbox))
                               (text stringbox)))))
            (save-frame)))))))


