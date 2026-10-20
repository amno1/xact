;;; layout.lisp — rows, columns and boxes, laid out by measure and place.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;; License: GPLv3+

;;; Commentary:

;; A small layout engine after Eve, the layout half of Adobe's Adam & Eve
;; (see design.md): the geometry of a window's parts is declared as
;; nested containers instead of computed by hand, and solved in two
;; passes, with no linear solver.
;;
;; A layout is a tree of nodes:
;;
;;   BOX     a leaf: one rectangle, such as a child window.  It has a
;;           minimum size, and stretch factors saying how much of any
;;           extra space it takes in each direction (0: none).
;;   ROW     children side by side, left to right.
;;   COLUMN  children one above the other, top to bottom.
;;
;; Rows and columns have a MARGIN around their children and SPACING
;; between them.  Along their axis, extra space goes to the children in
;; proportion to their stretch; across it, a child is laid out by its
;; ALIGN: :FILL (the default) takes the full width or height, :START,
;; :CENTER and :END keep the child's minimum size and place it there.  A
;; container stretches as much as its most stretchy child, so stretch
;; passes up the tree.
;;
;;   (column (:margin 12 :spacing 10)
;;     (row (:spacing 8 :stretch-y 1)
;;       (box :name :sv  :min-width 60 :stretch-x 1 :stretch-y 1)
;;       (box :name :hue :min-width 24))
;;     (row (:spacing 8)
;;       (box :name :swatch :min-width 56 :min-height 36)
;;       (box :name :hex    :stretch-x 1)))
;;
;; LAYOUT-MEASURE computes each node's minimum size, bottom up;
;; LAYOUT-PLACE gives the root a rectangle and divides it, top down; and
;; LAYOUT-SOLVE does both.  Each node then has its X Y WIDTH HEIGHT, and
;; a box with a PLACE function has it called as (box) -- to configure its
;; window, say.  LAYOUT-FIND finds a box by name.
;;
;; A widget's WIDGET-LAYOUT, when set, is laid out over the widget's
;; size by RELAYOUT, which RESIZE-INTERACTOR calls when the window
;; changes size.

;;; Code:

(in-package #:xact)

(defclass layout-node ()
  ((stretch-x :initarg :stretch-x :initform 0 :accessor node-stretch-x)
   (stretch-y :initarg :stretch-y :initform 0 :accessor node-stretch-y)
   (align     :initarg :align     :initform :fill :accessor node-align
              :documentation "Across its container's axis: :FILL, :START,
              :CENTER or :END.")
   (min-w     :initform 0 :accessor node-min-width
              :documentation "The minimum width, from LAYOUT-MEASURE.")
   (min-h     :initform 0 :accessor node-min-height)
   (x         :initform 0 :accessor node-x)
   (y         :initform 0 :accessor node-y)
   (width     :initform 0 :accessor node-width)
   (height    :initform 0 :accessor node-height)))

(defclass layout-box (layout-node)
  ((name       :initarg :name       :initform nil :reader node-name)
   (min-width  :initarg :min-width  :initform 0)
   (min-height :initarg :min-height :initform 0)
   (place      :initarg :place      :initform nil :accessor box-place
               :documentation "Called as (box) once the box is placed.")))

(defclass layout-container (layout-node)
  ((children :initarg :children :initform '() :accessor node-children)
   (margin   :initarg :margin  :initform 0 :reader container-margin)
   (spacing  :initarg :spacing :initform 0 :reader container-spacing)))

(defclass layout-row    (layout-container) ())
(defclass layout-column (layout-container) ())

(defun box (&rest initargs &key name min-width min-height stretch-x stretch-y align place)
  "A BOX layout node; see the top of layout.lisp."
  (declare (ignore name min-width min-height stretch-x stretch-y align place))
  (apply #'make-instance 'layout-box initargs))

(defmacro row ((&rest options &key margin spacing stretch-x stretch-y align) &body children)
  "A ROW of CHILDREN, laid out left to right."
  (declare (ignore margin spacing stretch-x stretch-y align))
  `(make-instance 'layout-row :children (list ,@children) ,@options))

(defmacro column ((&rest options &key margin spacing stretch-x stretch-y align) &body children)
  "A COLUMN of CHILDREN, laid out top to bottom."
  (declare (ignore margin spacing stretch-x stretch-y align))
  `(make-instance 'layout-column :children (list ,@children) ,@options))

(defmethod initialize-instance :after ((c layout-container) &key)
  ;; A container stretches as much as its stretchiest child, unless told.
  (with-slots (children stretch-x stretch-y) c
    (when (zerop stretch-x)
      (setf stretch-x (reduce #'max children :key #'node-stretch-x :initial-value 0)))
    (when (zerop stretch-y)
      (setf stretch-y (reduce #'max children :key #'node-stretch-y :initial-value 0)))))

;;; Measure: minimum sizes, bottom up.

(defgeneric layout-measure (node)
  (:documentation "Compute NODE's minimum size, and its children's."))

(defmethod layout-measure ((b layout-box))
  (setf (node-min-width b)  (slot-value b 'min-width)
        (node-min-height b) (slot-value b 'min-height)))

(defun %measure-container (c along across)
  "Along the axis: the sum of the children plus spacing; across: the
   largest child.  ALONG and ACROSS read a child's minimum size."
  (let* ((kids (node-children c))
         (m (container-margin c))
         (gaps (* (container-spacing c) (max 0 (1- (length kids))))))
    (mapc #'layout-measure kids)
    (values (+ (* 2 m) gaps (reduce #'+ kids :key along))
            (+ (* 2 m) (reduce #'max kids :key across :initial-value 0)))))

(defmethod layout-measure ((c layout-row))
  (multiple-value-bind (w h) (%measure-container c #'node-min-width #'node-min-height)
    (setf (node-min-width c) w (node-min-height c) h)))

(defmethod layout-measure ((c layout-column))
  (multiple-value-bind (h w) (%measure-container c #'node-min-height #'node-min-width)
    (setf (node-min-width c) w (node-min-height c) h)))

;;; Place: rectangles, top down.

(defgeneric layout-place (node x y width height)
  (:documentation "Give NODE the rectangle X Y WIDTH HEIGHT and divide it
   among its children."))

(defmethod layout-place :before ((n layout-node) x y width height)
  (setf (node-x n) x (node-y n) y
        (node-width n) (max 0 width) (node-height n) (max 0 height)))

(defmethod layout-place ((b layout-box) x y width height)
  (declare (ignore x y width height))
  (when (box-place b)
    (funcall (box-place b) b)))

(defun %shares (extra weights)
  "EXTRA pixels shared out in proportion to WEIGHTS, as whole pixels that
   add up to EXTRA (when any weight is positive)."
  (let ((total (reduce #'+ weights)))
    (if (or (<= extra 0) (zerop total))
        (make-list (length weights) :initial-element 0)
        (let* ((given 0) (acc 0))
          (mapcar (lambda (wt)
                    (incf acc wt)
                    (let ((upto (floor (* extra acc) total)))
                      (prog1 (- upto given) (setf given upto))))
                  weights)))))

(defun %across (node min start size)
  "(VALUES POSITION EXTENT) of NODE across its container's axis, in the
   SIZE pixels from START, by its ALIGN; MIN is its minimum there."
  (let ((ext (min min size)))
    (ecase (node-align node)
      (:fill   (values start size))
      (:start  (values start ext))
      (:center (values (+ start (floor (- size ext) 2)) ext))
      (:end    (values (+ start (- size ext)) ext)))))

(defun %place-container (c x y width height horizontal)
  (let* ((kids (node-children c))
         (m (container-margin c))
         (sp (container-spacing c))
         (along-size (if horizontal (- width (* 2 m)) (- height (* 2 m))))
         (across-size (if horizontal (- height (* 2 m)) (- width (* 2 m))))
         (mins (mapcar (if horizontal #'node-min-width #'node-min-height) kids))
         (extra (- along-size (reduce #'+ mins) (* sp (max 0 (1- (length kids))))))
         (shares (%shares extra (mapcar (if horizontal #'node-stretch-x #'node-stretch-y) kids)))
         (pos (+ (if horizontal x y) m)))
    (loop for kid in kids
          for min in mins
          for share in shares
          for along = (+ min share)
          do (multiple-value-bind (apos asize)
                 (%across kid (if horizontal (node-min-height kid) (node-min-width kid))
                          (+ (if horizontal y x) m) across-size)
               (if horizontal
                   (layout-place kid pos apos along asize)
                   (layout-place kid apos pos asize along)))
             (incf pos (+ along sp)))))

(defmethod layout-place ((c layout-row) x y width height)
  (%place-container c x y width height t))

(defmethod layout-place ((c layout-column) x y width height)
  (%place-container c x y width height nil))

(defun layout-solve (node x y width height)
  "Measure NODE's tree, then lay it out in X Y WIDTH HEIGHT (never less
   than its minimum size).  Returns NODE."
  (layout-measure node)
  (layout-place node x y (max width (node-min-width node)) (max height (node-min-height node)))
  node)

(defun layout-find (node name)
  "The box named NAME in NODE's tree, or NIL."
  (typecase node
    (layout-box (and (eql (node-name node) name) node))
    (layout-container (some (lambda (k) (layout-find k name)) (node-children node)))))

(defun layout-boxes (node)
  "All the boxes in NODE's tree, in order."
  (typecase node
    (layout-box (list node))
    (layout-container (mapcan #'layout-boxes (node-children node)))))

(defun relayout (widget)
  "Lay WIDGET-LAYOUT out over the widget's size, if it has one."
  (let ((layout (widget-layout widget)))
    (when layout
      (layout-solve layout 0 0 (widget-width widget) (widget-height widget)))))

;;; layout.lisp ends here
