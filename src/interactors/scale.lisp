;;; scale.lisp — scale something by dragging the mouse.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;; License: GPLv3+

;;; Commentary:

;; SCALE-DRAG-INTERACTOR turns a drag into a scale factor: from the press,
;; dragging right or up makes it bigger and left or down smaller, by a
;; factor of 2 every PIXELS-PER-DOUBLING pixels, kept between MIN and
;; MAX.  It reads the scale at the press with GET and hands each new one
;; to SET; what scales -- text, a layout, a whole window -- is SET's
;; business.  The start event is Alt- or Super-click by default: Super
;; as well, because window managers such as openbox move windows on
;; Alt-drag, and then the press never reaches the client.
;;
;;   (make-instance 'scale-drag-interactor
;;     :window win
;;     :get (lambda () (cref state :scale))
;;     :set (lambda (s) (setf (cref state :scale) s)))

;;; Code:

(in-package #:xact)

(defclass scale-drag-interactor (continuous-mixin interactor)
  ((get    :initarg :get :reader scale-drag-get
           :documentation "Called with no arguments: the scale now.")
   (set    :initarg :set :reader scale-drag-set
           :documentation "Called with the new scale, a rational.")
   (pixels-per-doubling :initarg :pixels-per-doubling :initform 200)
   (min    :initarg :min :initform 1/2)
   (max    :initarg :max :initform 4)
   (anchor :initform nil :documentation "(root-x root-y scale) at the press."))
  (:default-initargs
   :start-event '(:or (:and :leftdown :alt) (:and :leftdown :super))))

(defmethod start-interaction ((i scale-drag-interactor) ev)
  (setf (slot-value i 'anchor)
        (list (event-root-x ev) (event-root-y ev) (funcall (scale-drag-get i)))))

(defmethod handle-event ((i scale-drag-interactor) (ev motion-event))
  ;; Reached through CONTINUOUS-MIXIN's :AROUND method while :ACTIVE.
  (when (eq (interactor-state i) :active)
    (destructuring-bind (x0 y0 s0) (slot-value i 'anchor)
      (with-slots (pixels-per-doubling min max) i
        (let* ((d (+ (- (event-root-x ev) x0) (- y0 (event-root-y ev))))
               (scale (* s0 (expt 2 (/ d pixels-per-doubling)))))
          (funcall (scale-drag-set i)
                   (rational (max min (min max scale)))))))
    t))

;;; scale.lisp ends here
