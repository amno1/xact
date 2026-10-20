;; events.lisp
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

;; Events are a class hierarchy so multiple dispatch can specialize on the
;; event type.

;;; Code:

(in-package #:xact)

(defclass event ()
  ((window :initarg :window :initform nil :reader event-window
           :documentation "The window the event is about: for input and
           crossing events the window it was reported to, for structure
           events (configure, map, create, ...) the window that changed.
           A widget that selects SubstructureNotify on a parent sees its
           children's events, and tells them apart by this."))
  (:documentation "Base class of every xact event."))

(defclass window-event (event)
  ((x :initarg :x :reader event-x)
   (y :initarg :y :reader event-y)))

(defclass crossing-event (window-event)
  ((mode :initarg :mode :initform 0 :reader event-mode
         :documentation "X's crossing mode: 0 Normal, 1 Grab (the pointer
         left because of a grab, as during a drag), 2 Ungrab."))
  (:documentation "The pointer entered or left the window."))

(defclass enter-event  (crossing-event) ())
(defclass leave-event  (crossing-event) ())

(defclass expose-event (event)
  ((x      :initarg :x      :reader event-x)
   (y      :initarg :y      :reader event-y)
   (width  :initarg :width  :reader event-width)
   (height :initarg :height :reader event-height)))

(defclass configure-event (event)
  ((x      :initarg :x      :reader event-x)
   (y      :initarg :y      :reader event-y)
   (width  :initarg :width  :reader event-width)
   (height :initarg :height :reader event-height)))

(defclass create-event  (event) ()
  (:documentation "A window was created (seen with SubstructureNotify on its parent)."))
(defclass reparent-event (event)
  ((parent :initarg :parent :initform nil :reader event-parent)))
(defclass map-event     (event) ())
(defclass unmap-event   (event) ())
(defclass destroy-event (event) ())
(defclass close-event   (event) ())

(defclass motion-event (event)
  ((x       :initarg :x       :reader event-x)
   (y       :initarg :y       :reader event-y)
   (root-x  :initarg :root-x  :reader event-root-x)
   (root-y  :initarg :root-y  :reader event-root-y)))

(defclass button-event (event)
  ((x         :initarg :x         :reader event-x)
   (y         :initarg :y         :reader event-y)
   (root-x    :initarg :root-x    :reader event-root-x)
   (root-y    :initarg :root-y    :reader event-root-y)
   (button    :initarg :button    :reader event-button)
   (modifiers :initarg :modifiers :initform 0 :reader event-modifiers)))

(defclass button-press   (button-event) ())
(defclass button-release (button-event) ())

(defclass key-event (event)
  ((keysym    :initarg :keysym    :reader event-keysym)
   (keycode   :initarg :keycode   :reader event-keycode)
   (modifiers :initarg :modifiers :initform 0 :reader event-modifiers)))

(defclass key-press   (key-event) ())
(defclass key-release (key-event) ())

(defgeneric event-matches-p (event spec)
  (:documentation
   "Does EVENT satisfy SPEC?  A spec is T, a keyword, (:OR spec...),
   (:AND spec...), or a predicate function of one argument.  The
   keywords :CONTROL :ALT :SHIFT and :SUPER match a button or key event
   with that modifier held, so (:AND :LEFTDOWN :CONTROL) is a Ctrl-click.")
  (:method (event (spec (eql t)))
    (declare (ignore event))
    t)
  (:method (event spec)
    (cond ((keywordp spec)
           (ecase spec
             (:leftdown    (and (typep event 'button-press)
                                (eql (event-button event) 1)))
             (:leftup      (and (typep event 'button-release)
                                (eql (event-button event) 1)))
             (:any-down    (typep event 'button-press))
             (:any-up      (typep event 'button-release))
             (:motion      (typep event 'motion-event))
             (:any-key     (typep event 'key-press))
             (:escape      (and (typep event 'key-press)
                                (eql (event-keysym event) :escape)))
             (:close       (typep event 'close-event))
             ((:control :alt :shift :super :hyper)
              (and (typep event '(or button-event key-event))
                   (logtest (ecase spec
                              (:control #x4)
                              (:alt #x8)
                              (:shift #x1)
                              ((:super :hyper) #x40))
                            (event-modifiers event))))))
          ((and (consp spec) (eq (car spec) :or))
           (some (lambda (s) (event-matches-p event s)) (cdr spec)))
          ((and (consp spec) (eq (car spec) :and))
           (every (lambda (s) (event-matches-p event s)) (cdr spec)))
          ((functionp spec) (funcall spec event))
          (t (error "Unknown event spec: ~S" spec)))))

