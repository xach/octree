;;;; octree.lisp
;;;;
;;;; Based (mostly) on https://observablehq.com/@tmcw/octree-color-quantization
;;;;

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

(defun branch (color depth)
  "Return the index, from 0 to 7, of the branch that COLOR takes in an
octree node at level DEPTH."
  (let ((bit (- +max-depth+ depth)))
    (+ (ash (ldb (byte 1 bit) (red color))   2)
       (ash (ldb (byte 1 bit) (green color)) 1)
       (ash (ldb (byte 1 bit) (blue color))  0))))

(defun add-colors (c1 c2)
  "Destructively modify C1 by adding the components of C2 to its
components."
  (incf (red c1) (red c2))
  (incf (green c1) (green c2))
  (incf (blue c1) (blue c2))
  c1 )

(defun node-add-color (node color level)
  "Add the components of COLOR to NODE (or a suitable child)"
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


(defun leafp (node)
  (plusp (octree-node-color-count node)))

(defmacro do-child-nodes ((child node) &body body)
  `(block nil
     (map nil (lambda (,child) (when ,child (locally ,@body)))
          (children ,node))))

(defun call-for-leaf-nodes (node fun)
  (if (leafp node)
      (funcall fun node)
      (do-child-nodes (child node)
        (call-for-leaf-nodes child fun))))

(defmacro do-leaf-nodes ((leaf node) &body body)
  `(call-for-leaf-nodes ,node (lambda (,leaf) ,@body)))

(defun leaves (node)
  (let ((result '()))
    (do-leaf-nodes (leaf node)
      (push leaf result))
    result))

(defun leaf-count (node)
  (let ((count 0))
    (do-leaf-nodes (leaf node)
      (declare (ignore leaf))
      (incf count))
    count))

(defun absorb-children (node)
  "Destructively merge the color values of NODE's children into NODE."
  (when (leafp node)
    (error "Can't absorb the children of a leaf."))
  (let ((absorbed 0))
    (do-child-nodes (child node)
      (incf absorbed)
      (incf (octree-node-color-count node)
            (octree-node-color-count child))
      (if (octree-node-rgb node)
          (add-colors (octree-node-rgb node)
                      (octree-node-rgb child))
          (setf (octree-node-rgb node)
                (octree-node-rgb child))))
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


;;; Create a limited set of colors to represent each color in a ZPNG
;;; image (the lower-level format used by Vecto)

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

(defun make-image-quantizer (image)
  "Create a quantizer with the pixel color information of IMAGE."
  (let ((quantizer (make-octree-node)))
    (do-image-pixels (r g b) image
      (node-add-color quantizer (make-color :red r :green g :blue b) 0))
    quantizer))

(defun make-levels-array (quantizer)
  (let ((levels (make-array +max-depth+ :initial-element nil)))
    (labels ((traverse (node level)
               (when (< level +max-depth+)
                 (push node (aref levels level))
                 (do-child-nodes (child node)
                   (traverse child (1+ level))))))
      (traverse quantizer 0))
    levels))

(defun make-palette (quantizer color-count)
  "Destructively reduce the number of nodes in QUANTIZER until at most
COLOR-COUNT remain. Assign each remaining node an integer palette
index. Returns a list of nodes in palette order."
  (let* ((palette '())
         (palette-index 0)
         (levels (make-levels-array quantizer))
         (root quantizer)
         (leaf-count (leaf-count root)))
    ;; Coalesce similar color nodes until only COLOR-COUNT nodes
    ;; remain
    (block palette
      (loop for level downfrom (1- +max-depth+) to 0
            do
               (dolist (node (aref levels level))
                 (let ((absorbed (absorb-children node)))
                   (decf leaf-count absorbed)
                   (when (<= leaf-count color-count)
                     (return-from palette))))))
    ;; Assign each remaining node a palette index, and compute its
    ;; final color
    (block nil
      (do-leaf-nodes (node root)
        (when (<= color-count palette-index)
          (return))
        (setf (octree-node-color-index node) palette-index)
        (setf (octree-node-rgb node)
              (node-average-color node))
        (setf (octree-node-color-count node) 1)
        (push node palette)
        (incf palette-index)
       root))
    (reverse palette)))

(defun palette-index (quantizer color)
  "Return the integer index of COLOR in the octree-node QUANTIZER."
  (labels ((lookup (node level)
             (if (leafp node)
                 (octree-node-color-index node)
                 (let ((index (branch color level)))
                   (lookup (child node index) (1+ level))))))
    (lookup quantizer 0)))

(defun vimage-gif-image (&key (delay-time 5))
  "Convert the current Vecto canvas to a Skippy GIF image, suitable
for adding to a data stream with SKIPPY:ADD-IMAGE."
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
