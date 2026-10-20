;; continuous.lisp
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

;; The three-state machine. This is the part Garnet called general-go,
;; decomposed into method combination so that mixins can layer on top.

;; The :around method decides when transitions happen; the primary
;; methods decide what happens in each state. A mixin like
;; screen-picker-mixin can add :before and :after methods on
;; start-interaction and stop-interaction and never touch the state
;; machine.

;;; Code:

(in-package #:xact)

(defclass continuous-mixin ()
  ((start-event :initarg :start-event :initform :leftdown)
   (stop-event  :initarg :stop-event  :initform :leftup)
   (abort-event :initarg :abort-event :initform :escape)
   (continuous  :initarg :continuous  :initform t
                :documentation
                "When NIL, the interaction is a tap:
                 start is followed immediately by tap-interaction.")))

(defmethod handle-event :around ((i continuous-mixin) ev)
  (case (interactor-state i)
    (:idle
     (cond ((event-matches-p ev (slot-value i 'start-event))
            (unless (hit-test-p i ev)
              (return-from handle-event nil))
            (setf (interactor-state i) :active)
            (start-interaction i ev)
            (unless (slot-value i 'continuous)
              (setf (interactor-state i) :idle)
              (tap-interaction i ev))
            t)
           (t
            ;; Not starting; give primary methods a chance to handle
            ;; (hover, cursor changes). They return NIL if uninterested.
            (call-next-method))))
    (:active
     (cond ((event-matches-p ev (slot-value i 'abort-event))
            (setf (interactor-state i) :idle)
            (abort-interaction i ev)
            t)
           ((event-matches-p ev (slot-value i 'stop-event))
            (setf (interactor-state i) :idle)
            (stop-interaction i ev)
            t)
           ((and (typep ev 'motion-event)
                 (not (hit-test-p i ev)))
            (setf (interactor-state i) :outside)
            (leave-interaction i ev)
            t)
           (t (call-next-method))))
    (:outside
     (cond ((event-matches-p ev (slot-value i 'abort-event))
            (setf (interactor-state i) :idle)
            (abort-interaction i ev)
            t)
           ((event-matches-p ev (slot-value i 'stop-event))
            (setf (interactor-state i) :idle)
            (abort-interaction i ev)
            t)
           ((and (typep ev 'motion-event) (hit-test-p i ev))
            (setf (interactor-state i) :active)
            (return-interaction i ev)
            t)
           (t (call-next-method))))))

;;; continuous.lisp ends here
