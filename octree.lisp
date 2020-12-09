;;;; octree.lisp



(in-package #:octree)

(defconstant +max-depth+ 8)

(defstruct (color (:conc-name ""))
  (red 0)
  (green 0)
  (blue 0))

(defstruct octree-node
  (color-count 0)
  color-index
  rgb
  children)

(defun xbit (index integer)
  (ldb (byte 1 index) integer))

(defun branch (color depth)
  (let ((bit (- +max-depth+ depth)))
    (+ (ash (xbit bit (red color))   2)
       (ash (xbit bit (green color)) 1)
       (ash (xbit bit (blue color))  0))))

(defun add-colors (c1 c2)
  "Destructively modify C1 by adding the components of C2 to its
components."
  (incf (red c1) (red c2))
  (incf (green c1) (green c2))
  (incf (blue c1) (blue c2))
  c1 )

(defun node-add-color (node color level)
  (cond ((= level +max-depth+)
         (let ((node-color (or (octree-node-rgb node)
                               (setf (octree-node-rgb node)
                                     (make-color :red 0 :green 0 :blue 0)))))
           (add-colors node-color
                       color))
         (incf (octree-node-color-count node)))
        (t
         (let* ((index (branch color level))
               (children (or (octree-node-children node)
                             (setf (octree-node-children node)
                                   (make-array 8 :initial-element nil))))
               (child (or (aref children index)
                          (setf (aref children index)
                                (make-octree-node)))))
           (node-add-color child color (1+ level))))))

(defun test-image-data ()
  (let ((canvas (box 0 0 256 256)))
    (with-box-canvas canvas
      (set-fill-color *white*)
      (clear-canvas)
      (move-to *origin*)
      (line-to (centerpoint canvas))
      (stroke)
      (set-line-width 5)
      (loop for hue from 0 below 360 by 30
            for angle from 0.0 by (/ pi 10)
            do
               (set-stroke-color (hsv-color hue 1 1))
               (move-to (centerpoint canvas))
               (line-to (add (centerpoint canvas)
                             (apoint angle 64)))
               (stroke))
      (vecto::image vecto::*graphics-state*))))


(defun call-for-image-pixels (fun image)
  (let ((samples-per-pixel (zpng:samples-per-pixel image))
        (data (zpng:image-data image)))
    (assert (<= 3 samples-per-pixel))
    (loop for i from 0 below (length data) by samples-per-pixel
          do
             (funcall fun
                      (aref data (+ i 0))
                      (aref data (+ i 1))
                      (aref data (+ i 2))))))

(defmacro do-image-pixels ((r g b) image &body body)
  `(call-for-image-pixels (lambda (,r ,g ,b) (block nil (locally ,@body))) ,image))

(defun traverse (fun node)
  (if (zerop (octree-node-color-count node))
      (map nil (lambda (node)
                 (when node
                   (traverse fun node)))
           (octree-node-children node))
      (funcall fun node)))

(defun leaves (node)
  (let ((result '()))
    (traverse (lambda (n) (push n result)) node)
    result))

(defun leaf-count (node)
  (if (null node)
      0
      (let ((count 0))
        (traverse (lambda (n) (declare (ignore n)) (incf count)) node)
        count)))

(defun leaf-pixel-count (node)
  (if (null node)
      0
      (let ((count 0))
        (traverse (lambda (n) (incf count (octree-node-color-count n)))
                  node)
        count)))

(defun leafp (node)
  (plusp (octree-node-color-count node)))

(defun absorb-children (node)
  "Combine a node into the values of its children"
  (when (leafp node)
    (error "Can't absorb the children of a leaf."))
  (let ((absorbed 0))
    (map nil
         (lambda (child)
           (when child
             (incf absorbed)
             (incf (octree-node-color-count node)
                   (octree-node-color-count child))
             (if (octree-node-rgb node)
                 (add-colors (octree-node-rgb node)
                             (octree-node-rgb child))
                 (setf (octree-node-rgb node)
                       (octree-node-rgb child)))))
         (children node))
    (setf (octree-node-children node) nil)
    (1- absorbed)))

(defun node-average-color (node)
  (let ((count (octree-node-color-count node))
        (color (octree-node-rgb node)))
    (make-color :red (truncate (red color) count)
                :green (truncate (green color) count)
                :blue (truncate (blue color) count))))

(defun child (node index)
  (aref (octree-node-children node) index))

(defun children (node)
  (octree-node-children node))

(defun make-image-quantizer (image)
  (let ((quantizer (make-octree-node)))
    (do-image-pixels (r g b) image
      (node-add-color quantizer (make-color :red r :green g :blue b) 0))
    quantizer))

(defmacro do-children ((child node) &body body)
  `(block nil
     (map nil (lambda (,child) (when ,child (locally ,@body)))
          (children ,node))))

(defun make-levels-array (quantizer)
  (let ((levels (make-array +max-depth+ :initial-element nil)))
    (labels ((traverse (node level)
               (when (< level +max-depth+)
                 (push node (aref levels level))
                 (do-children (child node)
                   (traverse child (1+ level))))))
      (traverse quantizer 0))
    levels))

(defun make-palette (quantizer color-count)
  (let* ((palette '())
         (palette-index 0)
         (levels (make-levels-array quantizer))
         (root quantizer)
         (leaf-count (leaf-count root)))
    (block palette
      (loop for level downfrom (1- +max-depth+) to 0
            do
               (dolist (node (aref levels level))
                 (let ((absorbed (absorb-children node)))
                   (decf leaf-count absorbed)
                   (when (<= leaf-count color-count)
                     (return-from palette))))))
    (block nil
      (traverse (lambda (node)
                  (when (<= color-count palette-index)
                    (return))
                  (setf (octree-node-color-index node) palette-index)
                  (push node palette)
                  (incf palette-index))
                root))
    (reverse palette)))

(defun palette-index (node color)
  (labels ((lookup (node level)
             (if (leafp node)
                 (octree-node-color-index node)
                 (let ((index (branch color level)))
                   (lookup (child node index) (1+ level))))))
    (lookup node 0)))

(defun test-all-pixels-in-palette (img)
  (let* ((q (make-image-quantizer img))
         (p (make-palette q 256)))
    (declare (ignore p))
    (do-image-pixels (r g b) img
      (palette-index q (make-color :red r :green g :blue b)))))

(defun test-write-a-gif (img file)
  (let* ((q (make-image-quantizer img))
         (width (zpng:width img))
         (height (zpng:height img))
         (color-table (make-color-table ))
         (gif-image-data
           (make-array (* width height)
                       :element-type '(unsigned-byte 8)))
         (p (make-palette q 256))
         (i 0))
    (dolist (node p)
      (let ((color (node-average-color node)))
        (add-color (logior (ash (red color) 16)
                           (ash (green color) 8)
                           (ash (blue color) 0))
                   color-table)))
    (do-image-pixels (r g b) img
      (setf (aref gif-image-data i)
            (palette-index q (make-color :red r :green g :blue b)))
      (incf i))
    (let* ((gif-image (make-image :width width :height height
                                  :image-data gif-image-data))
           (ds (make-data-stream :width width :height height
                                 :color-table color-table
                                 :initial-images (list gif-image))))
      (output-data-stream ds file ))))

(defun vimage-gif-image (&key (delay-time 5))
  (let* ((img (vecto::image vecto::*graphics-state*))
         (q (make-image-quantizer img))
         (width (zpng:width img))
         (height (zpng:height img))
         (color-table (make-color-table ))
         (gif-image-data
           (make-array (* width height)
                       :element-type '(unsigned-byte 8)))
         (p (make-palette q 256))
         (i 0))
    (dolist (node p)
      (let ((color (node-average-color node)))
        (add-color (logior (ash (red color) 16)
                           (ash (green color) 8)
                           (ash (blue color) 0))
                   color-table)))
    (do-image-pixels (r g b) img
      (setf (aref gif-image-data i)
            (palette-index q (make-color :red r :green g :blue b)))
      (incf i))
    (make-image :width width :height height
                :delay-time delay-time
                :image-data gif-image-data
                :color-table color-table)))

(defun test-animation (file)
  (let* ((canvas (box 0 0 256 256))
         (ds (make-data-stream :width (vectometry:width canvas)
                               :height (vectometry:height canvas)
                               :loopingp t)))
    (with-box-canvas canvas
      (loop for divisor downfrom 100
            repeat 94 do
              (set-fill-color *white*)
              (clear-canvas)
              (set-line-cap :round)
              (set-line-width 5)
              (loop for hue from 0 below 360 by 30
                    for angle from 0.0 by (/ pi divisor)
                    do
                       (set-stroke-color (hsv-color hue 1 1))
                       (move-to (centerpoint canvas))
                       (line-to (add (centerpoint canvas)
                                     (apoint angle 128)))
                       (stroke))
              (add-image (vimage-gif-image) ds)))
    (output-data-stream ds file)))
