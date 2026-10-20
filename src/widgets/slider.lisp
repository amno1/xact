;; slider.lisp
;; Copyright (C) 2026  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple slider widget to show how pieces assemble. Built entirely
;; from drag-interactor plus a draw method.

;; draw is supplied by the application or by a backend-specific renderer. The
;; widget knows its geometry; the renderer knows how to paint.

;;; Code:

(in-package #:xact)

(defclass slider (widget)
  ((left   :initarg :left   :reader slider-left)
   (top    :initarg :top    :reader slider-top)
   (width  :initarg :width  :reader slider-width)
   (height :initarg :height :reader slider-height)
   (on-change :initarg :on-change :initform nil :reader slider-on-change)))

(defun make-slider (window state &key connection left top width height on-change)
  (let ((w (make-instance 'slider
             :connection connection
             :window window :state state
             :left left :top top :width width :height height
             :on-change on-change)))
    (widget-add-interactor
     w
     (make-instance 'drag-interactor
       :window window
       :hit-test (lambda (i ev)
                   (declare (ignore i))
                   (and (<= left (event-x ev) (+ left width))
                        (<= top  (event-y ev) (+ top height))))
       :on-drag (lambda (i ev)
                  (declare (ignore i))
                  (let ((frac (max 0.0 (min 1.0
                                            (/ (- (event-x ev) left)
                                               (float width))))))
                    (when on-change (funcall on-change frac))
                    (setf (widget-dirty-p w) t)))))
    w))

;;; slider.lisp ends here
