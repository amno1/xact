;;; window.lisp — interactors that move and resize the widget's window.
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

;; When the window manager draws no frame -- override-redirect and
;; undecorated windows -- it gives no way to move or resize the window
;; either.  These interactors give it back, client-side.
;;
;; WINDOW-DRAG-INTERACTOR: press button 1 anywhere in the window, move,
;; and the window follows the pointer.
;;
;; WINDOW-RESIZE-INTERACTOR: the same, except that a press in the grip
;; zone, a GRIP-SIZE square in the bottom-right corner, resizes the
;; window instead, never below MIN-WIDTH x MIN-HEIGHT.  With :MOVE NIL
;; it only resizes: a press outside the grip is left to the widget's
;; other interactors, which is what a WM-framed window wants.
;; DRAW-RESIZE-GRIP draws the usual three diagonal strokes for the grip.
;;
;; KEY-RESIZE-INTERACTOR and KEY-MOVE-INTERACTOR do the same from the
;; keyboard, modally.  Their KEY (default Ctrl+R to resize, Ctrl+M to
;; move) enters the mode, where the arrow keys move the bottom-right
;; corner, or the whole window, by STEP pixels (Shift: 1); Return keeps
;; the result and Escape puts the window back.  While in the mode the
;; interactor takes every key, so nothing else sees the arrows or Escape;
;; add it after the widget's quit interactor so it sees them first.
;;
;; Both work in root coordinates, so the window can move or grow by more
;; than its own size in one drag, and Escape puts it back where the drag
;; started.  With :EWMH T the move or resize is handed to the window
;; manager instead (_NET_WM_MOVERESIZE), which then does the dragging;
;; that only works where a WM supporting it runs.
;;
;; ON-START is called as (interactor) when a drag or resize begins, and
;; ON-MOVE as (interactor dx dy) after each move, with the pointer's
;; offset from where the drag started -- to move a second window along
;; with the first, say.  WIDGET-DRAGGING-P tells whether one of a
;; widget's window interactors is moving or resizing it, for code that
;; must ignore the Leave event the drag's pointer grab causes.

;;; Code:

(in-package #:xact)

(defclass window-drag-interactor (continuous-mixin interactor)
  ((widget   :initarg :widget :reader interactor-widget)
   (ewmh     :initarg :ewmh :initform nil
             :documentation "Hand the move or resize to the window
             manager with _NET_WM_MOVERESIZE.")
   (on-start :initarg :on-start :initform nil)
   (on-move  :initarg :on-move  :initform nil)
   (mode     :initform nil :reader window-drag-mode
             :documentation ":DRAG or :RESIZE while active, else NIL.")
   (anchor   :initform nil
             :documentation "(root-x root-y window-x window-y width height)
             when the drag started.")
   (frame    :initform '(0 0 0 0)
             :documentation "The WM frame's (left right top bottom) when the
             drag started; moves ask for the frame's position.")))

(defclass window-resize-interactor (window-drag-interactor)
  ((grip-size  :initarg :grip-size  :initform 16
               :accessor resize-grip-size
               :documentation "Side of the grip zone; 0 for none.")
   (move       :initarg :move       :initform t
               :documentation "Also move the window by a press outside the grip.")
   (min-width  :initarg :min-width  :initform 80)
   (min-height :initarg :min-height :initform 80)))

(defgeneric %in-grip-p (interactor event)
  (:method ((i window-drag-interactor) event)
    (declare (ignore event))
    nil)
  (:method ((i window-resize-interactor) event)
    (let ((grip (slot-value i 'grip-size))
          (w (interactor-widget i)))
      (and grip (plusp grip)
           (>= (event-x event) (- (widget-width w) grip))
           (>= (event-y event) (- (widget-height w) grip))))))

(defmethod handle-event :around ((i window-resize-interactor) (event button-press))
  ;; Resize-only: a press outside the grip does not start anything.
  (if (and (eq (interactor-state i) :idle)
           (not (slot-value i 'move))
           (not (%in-grip-p i event)))
      nil
      (call-next-method)))

(defun draw-resize-grip (conn drawable gc width height &key (size 16) (strokes 3))
  "Draw a resize grip, STROKES diagonal lines in GC's foreground, in the
   SIZE square at the bottom-right corner of a WIDTH x HEIGHT DRAWABLE."
  (let ((step (truncate size (1+ strokes)))
        (x0 (- width 2))
        (y0 (- height 2)))
    (xcb::poly-segment
     conn drawable gc
     (loop for i from 1 to strokes
           for off = (* step i)
           collect (make-instance 'xcb::segment
                                  :x1 (- x0 off) :y1 y0
                                  :x2 x0         :y2 (- y0 off))))))

(defun %request-wm-moveresize (i event resize)
  "Ask the window manager to move or resize I's window (EWMH)."
  (let* ((w    (interactor-widget i))
         (conn (widget-connection w))
         (root (xcb:root (first (xcb:conn-roots conn)))))
    ;; The WM needs the pointer: give up the implicit grab of the press.
    (xcb:ungrab-pointer conn 0)
    (xcb:send-client-message conn root
                             (logior #x80000 #x100000) ; SubstructureRedirect | SubstructureNotify
                             (widget-window w)
                             (xcb:intern-atom-id conn "_NET_WM_MOVERESIZE")
                             ;; x_root y_root direction button source
                             ;; (direction 4 = size bottom-right, 8 = move)
                             (list (event-root-x event) (event-root-y event)
                                   (if resize 4 8) 1 1))
    (xcb:flush conn)))

(defmethod start-interaction ((i window-drag-interactor) (event button-press))
  (let* ((w (interactor-widget i))
         (conn (widget-connection w))
         (resize (%in-grip-p i event)))
    (if (slot-value i 'ewmh)
        (progn (%request-wm-moveresize i event resize)
               ;; The WM does the dragging and takes the pointer, so the
               ;; release may never reach us: the interaction is over.
               (setf (interactor-state i) :idle))
        (multiple-value-bind (wx wy) (xcb:window-root-position conn (widget-window w))
          (setf (slot-value i 'frame) (%frame-extents conn (widget-window w)))
          (setf (slot-value i 'anchor)
                (list (event-root-x event) (event-root-y event) wx wy
                      (widget-width w) (widget-height w))
                (slot-value i 'mode) (if resize :resize :drag))))
    (when (slot-value i 'on-start)
      (funcall (slot-value i 'on-start) i))))

(defun %move-to (i x y)
  "Move I's window so that the window itself is at X, Y on the root: a WM
   that frames it places the frame at the position asked for."
  (destructuring-bind (left right top bottom) (slot-value i 'frame)
    (declare (ignore right bottom))
    (%configure i :x (- x left) :y (- y top))))

(defun %configure (i &key x y width height)
  (let* ((w (interactor-widget i))
         (mask (logior (if x 1 0) (if y 2 0) (if width 4 0) (if height 8 0))))
    (when (plusp mask)
      (apply #'xcb:configure-window (widget-connection w) (widget-window w) mask
             (append (and x (list :x x)) (and y (list :y y))
                     (and width (list :width width)) (and height (list :height height)))))))

(defmethod handle-event ((i window-drag-interactor) (event motion-event))
  ;; Reached through CONTINUOUS-MIXIN's :around method while :ACTIVE.
  (when (eq (interactor-state i) :active)
    (destructuring-bind (&optional rx ry wx wy width height) (slot-value i 'anchor)
      (let ((dx (and rx (- (event-root-x event) rx)))
            (dy (and ry (- (event-root-y event) ry))))
        (case (slot-value i 'mode)
          (:drag
           (%move-to i (+ wx dx) (+ wy dy))
           (when (slot-value i 'on-move)
             (funcall (slot-value i 'on-move) i dx dy)))
          (:resize
           (let ((nw (max (slot-value i 'min-width) (+ width dx)))
                 (nh (max (slot-value i 'min-height) (+ height dy))))
             (%configure i :width nw :height nh))))))
    t))

(defun %end-drag (i)
  (setf (slot-value i 'mode) nil
        (slot-value i 'anchor) nil))

(defmethod stop-interaction ((i window-drag-interactor) event)
  (declare (ignore event))
  (%end-drag i))

(defmethod abort-interaction ((i window-drag-interactor) event)
  ;; Escape: put the window back where the drag started.
  (declare (ignore event))
  (destructuring-bind (&optional rx ry wx wy width height) (slot-value i 'anchor)
    (declare (ignore rx ry))
    (case (slot-value i 'mode)
      (:drag   (%move-to i wx wy)
               (when (slot-value i 'on-move)
                 (funcall (slot-value i 'on-move) i 0 0)))
      (:resize (%configure i :width width :height height))))
  (%end-drag i))

;;; Keyboard moving and resizing

(defclass key-window-interactor (interactor)
  ((widget     :initarg :widget :reader interactor-widget)
   (key        :initarg :key
               :documentation "Enters the mode; a key as QUIT-INTERACTOR takes them.")
   (step       :initarg :step :initform 10
               :documentation "Pixels per arrow press; Shift moves by 1.")
   (on-start   :initarg :on-start :initform nil
               :documentation "Called as (interactor) on entering the mode.")
   (on-end     :initarg :on-end :initform nil
               :documentation "Called as (interactor confirmed-p) on leaving it.")
   (original   :initform nil
               :documentation "The window's (A B) on entering the mode: its
               size for resizing, its position for moving.")
   (current    :initform nil
               :documentation "(A B) asked for so far."))
  (:documentation "The modal keyboard part KEY-RESIZE-INTERACTOR and
   KEY-MOVE-INTERACTOR share; they say what (A B) is and how to apply it."))

(defclass key-resize-interactor (key-window-interactor)
  ((min-width  :initarg :min-width  :initform 80)
   (min-height :initarg :min-height :initform 80))
  (:default-initargs :key '(:control #\r)))

(defclass key-move-interactor (key-window-interactor)
  ((frame :initform '(0 0 0 0)
          :documentation "The WM frame's (left right top bottom), read on
          entering the mode; see %MOVE-TO."))
  (:default-initargs :key '(:control #\m)))

(defun %frame-extents (conn window)
  "WINDOW's frame as _NET_FRAME_EXTENTS gives it, (LEFT RIGHT TOP BOTTOM),
   or zeros where no WM frames it."
  (handler-case
      (let ((atom (xcb:intern-atom-id conn "_NET_FRAME_EXTENTS" t)))
        (if (zerop atom)
            (list 0 0 0 0)
            (let* ((reply (xcb::get-property-reply
                           conn (xcb::get-property conn nil window atom
                                                   (xcb:intern-atom-id conn "CARDINAL") 0 4)))
                   (value (coerce (slot-value reply 'xcb::value)
                                  '(simple-array (unsigned-byte 8) (*))))
                   (order (xcb:conn-byte-order conn)))
              (if (>= (length value) 16)
                  (loop for k below 4 collect (xcb:read-card32-at value (* 4 k) order))
                  (list 0 0 0 0)))))
    (error () (list 0 0 0 0))))

(defgeneric %key-window-get (interactor)
  (:documentation "The window's (A B) now.")
  (:method ((i key-resize-interactor))
    (let ((w (interactor-widget i)))
      (list (widget-width w) (widget-height w))))
  (:method ((i key-move-interactor))
    (let* ((w    (interactor-widget i))
           (conn (widget-connection w)))
      (setf (slot-value i 'frame) (%frame-extents conn (widget-window w)))
      (multiple-value-list (xcb:window-root-position conn (widget-window w))))))

(defgeneric %key-window-set (interactor a b)
  (:documentation "Ask for the window's (A B), clamped as the interactor
   needs; returns what was asked for.")
  (:method ((i key-resize-interactor) a b)
    (let ((a (max (slot-value i 'min-width) a))
          (b (max (slot-value i 'min-height) b)))
      (%configure i :width a :height b)
      (list a b)))
  (:method ((i key-move-interactor) a b)
    (%move-to i a b)
    (list a b)))

(defun key-window-active-p (i)
  "True while I is in its mode."
  (and (slot-value i 'original) t))

(defun key-resizing-p (i)
  "True while I, a KEY-RESIZE-INTERACTOR, is in resize mode."
  (key-window-active-p i))

(defun %key-window-end (i confirmed)
  (unless confirmed
    (apply #'%key-window-set i (slot-value i 'original)))
  (setf (slot-value i 'original) nil
        (slot-value i 'current) nil)
  (when (slot-value i 'on-end)
    (funcall (slot-value i 'on-end) i confirmed)))

(defmethod handle-event ((i key-window-interactor) (ev key-press))
  (cond
    ((not (key-window-active-p i))
     (when (key-matches-p ev (slot-value i 'key))
       (let ((now (%key-window-get i)))
         (setf (slot-value i 'original) now
               (slot-value i 'current)  (copy-list now)))
       (when (slot-value i 'on-start)
         (funcall (slot-value i 'on-start) i))
       t))
    (t
     (destructuring-bind (a b) (slot-value i 'current)
       (let ((step (if (logtest 1 (event-modifiers ev)) 1 (slot-value i 'step))))
         (flet ((to (na nb) (setf (slot-value i 'current) (%key-window-set i na nb))))
           (case (event-keysym ev)
             (:right  (to (+ a step) b))
             (:left   (to (- a step) b))
             (:down   (to a (+ b step)))
             (:up     (to a (- b step)))
             (:return (%key-window-end i t))
             (:escape (%key-window-end i nil))))))
     ;; Modal: every key is ours until Return or Escape.
     t)))

(defmethod handle-event ((i key-window-interactor) (ev key-release))
  (key-window-active-p i))

(defun widget-dragging-p (widget)
  "True while one of WIDGET's window interactors is moving or resizing
   its window."
  (some (lambda (i)
          (and (typep i 'window-drag-interactor)
               (member (window-drag-mode i) '(:drag :resize))))
        (widget-interactors widget)))

;;; window.lisp ends here
