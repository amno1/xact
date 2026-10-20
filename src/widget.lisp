;; widget.lisp -- 
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>

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

(defclass widget ()
  ((connection  :initarg :connection  :reader widget-connection)
   (window      :initarg :window      :initform nil :accessor widget-window)
   (state       :initarg :state       :initform (make-instance 'state) :reader widget-state)
   (width       :initarg :width       :initform 0 :accessor widget-width)
   (height      :initarg :height      :initform 0 :accessor widget-height)
   (interactors :initform '()         :accessor widget-interactors)
   (layout      :initarg :layout      :initform nil :accessor widget-layout
                :documentation "A layout tree (see layout.lisp) laid out over
                the widget's size by RELAYOUT, or NIL.")
   (keymaps     :initarg :keymaps     :initform '() :accessor widget-keymaps
                :documentation "The keymaps RUN-WIDGET looks keys up in, top
                first (see keymap.lisp).")
   (dirty       :initform t           :accessor widget-dirty-p)))

(defun widget-add-interactor (widget interactor)
  "Interactors are tried in reverse order of addition, so the most
   recently added has first crack at an event."
  (push interactor (widget-interactors widget))
  interactor)

(defun widget-remove-interactor (widget interactor)
  (setf (widget-interactors widget)
        (remove interactor (widget-interactors widget)))
  interactor)

(defun widget-handle-event (widget event)
  "Offer EVENT to each interactor in order. The first one that returns
   T claims the event; the widget is marked dirty."
  (dolist (i (widget-interactors widget))
    (when (handle-event i event)
      (setf (widget-dirty-p widget) t)
      (return-from widget-handle-event t)))
  nil)

;; The surface argument is deliberately opaque. For an XCB app, it is a struct
;; holding the connection, drawable, and graphics context. Could be a
;; a Cairo context, OpenGL context, Skia surface ...
(defgeneric draw (widget surface)
  (:documentation "Render WIDGET into SURFACE. SURFACE is backend-specific.
                   Called by the event loop when the widget is dirty.")
  (:method ((w widget) surface) (declare (ignore w surface))))

(defun widget-redraw (widget surface)
  (when (widget-dirty-p widget)
    (draw widget surface)
    (setf (widget-dirty-p widget) nil)))

(defun widget-tick (widget)
  (dolist (i (widget-interactors widget))
    (handle-tick i)))
