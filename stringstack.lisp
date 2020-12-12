;;;; stringstack.lisp

(in-package #:octree)

(defvar *reference-font-size* 100)
(defvar *font-loader*)
(defparameter *minimum-text-pixels* 6.5)


(defclass stringbox ()
  ((text
    :initarg :text
    :accessor text)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box)
   (displacement
    :initarg :displacement
    :accessor displacement)
   (scaling
    :initarg :scaling
    :accessor scaling))
  (:documentation "A single string in a stringstack."))

(defmethod print-object ((stringbox stringbox) stream)
  (print-unreadable-object (stringbox stream :type t)
    (let ((p (displacement stringbox)))
      (format stream "~A,~A [~A]"
              (x p)
              (y p)
              (scaling stringbox)))))

(defgeneric font-size (object)
  (:method (stringbox)
    (* *reference-font-size* (scaling stringbox))))

(defclass stringstack ()
  ((font
    :initarg :font
    :accessor font)
   (max-width
    :initarg :max-width
    :accessor max-width)
   (elements
    :initarg :elements
    :accessor elements)
   (origin
    :initarg :origin
    :accessor origin)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box))
  (:default-initargs
   :elements nil
   :font *font-loader*
   :origin *origin*)
  (:documentation
   "A stack of stringboxes of equal horizontal size."))

(defun line-gap (stringstack)
  (/ (max-width stringstack) *reference-font-size*))

(defun split-add-string (string stringstack)
  (when (< (length string) 2)
    (error "String too small to split"))
  (flet ((trim (s)
           (string-trim " " s)))
    (let* ((split (truncate (length string) 2))
           (left (trim (subseq string 0 split)))
           (right (trim (subseq string split))))
      (add-string left stringstack)
      (add-string right stringstack))))
      
(defun too-small-p (scale)
  (< scale (/ *minimum-text-pixels* *reference-font-size*)))

(defun add-string (string stringstack)
  (when (string= string "")
    (setf string " "))
  (let ((box (string-bounding-box string *reference-font-size* (font stringstack))))
    (unless (zerop (vectometry:width box))
      (let* ((scale (/ (max-width stringstack) (vectometry:width box))))
        (when (too-small-p scale)
          (return-from add-string (split-add-string string stringstack)))
        (setf box (vectometry:scale box scale))
        (let ((displacement (sub (origin stringstack) (top-left box))))
          (push (make-instance 'stringbox
                               :text string
                               :bounding-box (displace box displacement)
                               :displacement displacement
                               :scaling scale)
                (elements stringstack)))
        (setf (origin stringstack) (sub (origin stringstack)
                                        (ypoint (+ (vectometry:height box)
                                                   (line-gap stringstack)))))
        t))))

(defmethod (setf elements) :after (new-value (stack stringstack))
  (slot-makunbound stack 'bounding-box))

(defun prune-to-height (stack max-height)
  (flet ((max-y (obj)
           (y (maxpoint (bounding-box obj))))
         (min-y (obj)
           (y (minpoint (bounding-box obj)))))
    (let ((newstack (make-instance 'stringstack
                                   :max-width (max-width stack)
                                   :font (font stack)))
          (limit (- (max-y stack) max-height)))
      (dolist (stringbox (reverse (elements stack)))
        (when (< (min-y stringbox) limit)
          (return))
        (add-string (text stringbox) newstack))
      newstack)))

(defun clear (stack)
  (setf (elements stack) nil)
  (setf (origin stack) *origin*)
  stack)

(define-condition empty-stack () ())

(defmethod slot-unbound (class (stack stringstack) (slot (eql 'bounding-box)))
  (let* ((first (first (elements stack)))
         (box (and first (bounding-box first))))
    (cond (box
           (dolist (stringbox (rest (elements stack)))
             (setf box (combine (bounding-box stringbox) box))))
          (t (setf box (box 0 0 (max-width stack) 5))))
    (setf (bounding-box stack) box)))

(defgeneric adjust (object adjustment)
  (:method ((stack stringstack) point)
    (setf (origin stack) (add point (origin stack)))
    (setf (elements stack)
          (mapcar (lambda (element)
                    (setf (bounding-box element)
                          (displace (bounding-box element) point))
                    (setf (displacement element)
                          (add (displacement element) point))
                    element)
                  (elements stack)))
    stack))

(defun quadrant-i (stack)
  (adjust stack (neg (minpoint (bounding-box stack)))))

#+nil
(defun split (string)
  (cl-ppcre:split ":" string))

(defun stringstack (width strings)
  (let ((stack (make-instance 'stringstack :max-width width)))
    (dolist (string strings stack)
      (add-string string stack))))

(defun stringstack* (width &rest strings)
  (stringstack width strings))

(defun draw-stringstack (stack file &key (color *white*))
  (with-box-canvas (bounding-box stack)
    (set-fill-color color)
    (dolist (stringbox (elements stack))
      (set-font (font stack) (font-size stringbox))
      (draw-string (displacement stringbox) (text stringbox)))
    (save-png file)))

