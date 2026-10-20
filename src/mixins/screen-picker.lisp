;;; screen-picker.lisp — pick a colour from anywhere on the screen.
;;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;;; License: GPLv3+

;;; Commentary:

;; SCREEN-PICKER-MIXIN turns any interactor into a screen colour picker:
;; when its interaction completes (by default, button 1 pressed and
;; released), it runs PICK-COLOR-FROM-SCREEN (backend/screen-picker.lisp)
;; and calls ON-SAMPLE as (interactor r g b) with the colour picked.
;; Cancelling the pick (Escape, another button) calls nothing.
;;
;; Mix it into an interactor whose HIT-TEST-P is the picker button:
;;
;;   (defclass pick-button (screen-picker-mixin continuous-mixin interactor)
;;     ())
;;
;;   (defmethod hit-test-p ((i pick-button) ev)
;;     (inside-pick-button-p ev))
;;
;;   (widget-add-interactor
;;    w (make-instance 'pick-button
;;        :window window :connection conn
;;        :on-sample (lambda (i r g b)
;;                     (declare (ignore i))
;;                     (setf (cref (widget-state w) :r) r
;;                           (cref (widget-state w) :g) g
;;                           (cref (widget-state w) :b) b))))
;;
;; The pick runs its own event loop with the pointer and keyboard
;; grabbed; the widget's loop resumes when it returns.

;;; Code:

(in-package #:xact)

(defclass screen-picker-mixin ()
  ((connection :initarg :connection :reader screen-picker-connection)
   (on-sample  :initarg :on-sample :initform nil
               :documentation "Called as (interactor r g b) after a pick.")))

(defmethod stop-interaction :after ((i screen-picker-mixin) ev)
  (declare (ignore ev))
  (multiple-value-bind (r g b)
      (pick-color-from-screen (screen-picker-connection i))
    (when (and r (slot-value i 'on-sample))
      (funcall (slot-value i 'on-sample) i r g b))))

;;; screen-picker.lisp ends here
