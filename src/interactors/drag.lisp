;;; drag.lisp — an interactor that follows the pointer while a button is held.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>

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

;; DRAG-INTERACTOR is CONTINUOUS-MIXIN plus two functions: HIT-TEST says
;; where a drag may start, ON-DRAG is called with each pointer position
;; while it lasts -- on the press that starts it and on every motion
;; while :ACTIVE.  Moving outside the hit region pauses the drag
;; (:OUTSIDE); coming back resumes it; releasing outside, or Escape,
;; aborts it, as Garnet does.
;;
;;   (make-instance 'drag-interactor
;;     :window window
;;     :hit-test (lambda (i ev) (inside-p ev))
;;     :on-drag  (lambda (i ev) (setf (cref state :x) (event-x ev))))

;;; Code:

(in-package #:xact)

(defclass drag-interactor (continuous-mixin interactor)
  ((hit-test :initarg :hit-test :initform nil :reader drag-hit-test
             :documentation "Called as (interactor event); NIL means anywhere.")
   (on-drag  :initarg :on-drag  :initform nil :reader drag-on-drag
             :documentation "Called as (interactor event) with each position.")))

(defmethod hit-test-p ((i drag-interactor) event)
  (let ((test (drag-hit-test i)))
    (or (null test) (funcall test i event))))

(defun %drag-to (i event)
  (when (drag-on-drag i)
    (funcall (drag-on-drag i) i event)))

(defmethod start-interaction ((i drag-interactor) event)
  (%drag-to i event))

(defmethod return-interaction ((i drag-interactor) event)
  ;; Back inside the hit region: follow the pointer from here.
  (%drag-to i event))

(defmethod handle-event ((i drag-interactor) (event motion-event))
  ;; Reached through CONTINUOUS-MIXIN's :around method, which handles the
  ;; transitions; here only a motion while :ACTIVE moves the drag.
  (when (eq (interactor-state i) :active)
    (%drag-to i event)
    t))

;;; drag.lisp ends here
