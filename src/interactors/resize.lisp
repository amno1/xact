;;; resize.lisp — keep a widget's size in step with its window.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;; License: GPLv3+

;;; Commentary:

;; RESIZE-INTERACTOR handles the ConfigureNotify a window gets when it
;; is resized, by the user or the WM: it sets WIDGET-WIDTH and
;; WIDGET-HEIGHT to the new size, lays its WIDGET-LAYOUT out again if
;; it has one (RELAYOUT), calls ON-RESIZE as (widget width height) -- to
;; rebuild a backing pixmap, say -- and marks the widget dirty.  Moves alone change nothing.
;;
;; A widget whose size lives elsewhere (a model, a state cell) can
;; define methods on WIDGET-WIDTH and (SETF WIDGET-WIDTH), and the same
;; for height, to keep it there.

;;; Code:

(in-package #:xact)

(defclass resize-interactor (interactor)
  ((widget    :initarg :widget    :reader resize-widget :reader interactor-widget)
   (on-resize :initarg :on-resize :initform nil :reader resize-on-resize)))

(defmethod handle-event ((i resize-interactor) (ev configure-event))
  (let ((w  (resize-widget i))
        (nw (event-width ev))
        (nh (event-height ev)))
    (when (and (plusp nw) (plusp nh)
               (or (/= nw (widget-width w))
                   (/= nh (widget-height w))))
      (setf (widget-width w)  nw
            (widget-height w) nh)
      (relayout w)
      (when (resize-on-resize i)
        (funcall (resize-on-resize i) w nw nh))
      (setf (widget-dirty-p w) t))
    t))

;;; resize.lisp ends here
