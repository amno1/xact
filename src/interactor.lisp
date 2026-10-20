;; interactor.lisp -- The base class for input-behavior objects.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>

;; An interactor watches a slice of input and mutates some target when the
;; interaction happens. Concrete interactors specialize the transition generics;
;; the three-state machine lives in continuous-mixin, not here.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(in-package #:xact)

(defclass interactor ()
  ((window :initarg :window :reader interactor-window)
   (state  :initform :idle :accessor interactor-state))
  (:documentation "Base class for all input-behavior objects.
                   Subclasses specialize the transition generics."))

(defgeneric handle-event (interactor event)
  (:documentation "Handle EVENT. Return T if the interactor consumed it,
                   NIL to let other interactors try. The default methods
                   on concrete interactors are reached through the state
                   machine's :around methods in the mixins.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric start-interaction   (interactor event)
  (:documentation "Called when the interactor transitions :idle -> :active.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric stop-interaction    (interactor event)
  (:documentation "Called on the transition :active -> :idle via the stop event.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric abort-interaction   (interactor event)
  (:documentation "Called on the transition :active -> :idle via the abort event.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric leave-interaction   (interactor event)
  (:documentation "Called on :active -> :outside.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric return-interaction  (interactor event)
  (:documentation "Called on :outside -> :active.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric tap-interaction     (interactor event)
  (:documentation "Called for one-shot interactions, immediately after start.")
  (:method ((i interactor) event) (declare (ignore i event))))

(defgeneric hit-test-p (interactor event)
  (:documentation "Does EVENT's pointer position land on this interactor's region?")
  (:method ((i interactor) event) (declare (ignore i event)) t))

(defgeneric handle-tick (interactor)
  (:documentation "Called periodically by the event loop while no events
   are pending.  Used by interactors that act on the passage of time
   rather than on input events.")
  (:method ((i interactor)) (declare (ignore i)) nil))
