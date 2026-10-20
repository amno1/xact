;;; toplevel.lisp — window-manager conventions for a widget's toplevel.
;;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;;; License: GPLv3+

;;; Commentary:

;; SETUP-TOPLEVEL gives a freshly created toplevel the ICCCM and EWMH
;; properties its kind calls for.  Call it once, after creating the
;; window and before mapping it.
;;
;;   :MANAGED            a normal window the WM decorates: WM_NAME,
;;                       WM_CLASS, _NET_WM_PID, the window type, and
;;                       WM_DELETE_WINDOW, so the close button sends a
;;                       CLOSE-EVENT instead of killing the client.
;;   :UNDECORATED        the same, without the WM's frame (Motif hints);
;;                       pair it with a WINDOW-DRAG-INTERACTOR or
;;                       WINDOW-RESIZE-INTERACTOR to move it.
;;   :OVERRIDE-REDIRECT  a window the WM never sees: only WM_NAME, so
;;                       that xprop and the like show what it is.
;;
;; TOPLEVEL is a widget that owns such a window and knows its kind,
;; title and class.  REALIZE creates the window, unless the widget
;; already has one, gives it its properties, and SHOW maps it:
;;
;;   (let ((tl (make-instance 'toplevel :connection conn :kind :undecorated
;;                            :title "Clock" :wm-class "clock"
;;                            :width 164 :height 164)))
;;     (realize tl :x 100 :y 100 :event-mask ...)
;;     (show tl)
;;     (run-widget tl (widget-window tl)))
;;
;; A widget with an unusual window (a visual or colormap of its own)
;; creates it itself, sets WIDGET-WINDOW, then calls REALIZE.  A fourth
;; kind, :ROOT, is for a widget that draws on a window it does not own,
;; the root or a virtual root, given as its WIDGET-WINDOW: REALIZE and
;; SHOW leave that window alone.
;;
;; REQUEST-CLOSE asks a running toplevel to end its loop, from any
;; thread: it sets TOPLEVEL-CLOSING-P, which RUN-WIDGET checks, and
;; sends the window a WM_DELETE_WINDOW message on a connection of its
;; own, so a loop waiting for events wakes up and the quit interactor
;; sees a CLOSE-EVENT.  A :ROOT toplevel gets a hidden InputOnly window
;; to receive that message, since the root window is not its own.
;; Loops of their own (a menu, a colour picker) poll TOPLEVEL-CLOSING-P.

;;; Code:

(in-package #:xact)

(defun setup-toplevel (conn window &key (kind :managed) title class resource-class)
  "Give WINDOW the window-manager properties for KIND, one of :MANAGED,
   :UNDECORATED or :OVERRIDE-REDIRECT.  TITLE is its WM_NAME; CLASS the
   instance part of WM_CLASS, and RESOURCE-CLASS its class part (default:
   CLASS capitalised).  Returns WINDOW."
  (ecase kind
    (:override-redirect
     (when title (xcb:set-wm-name conn window title)))
    ((:managed :undecorated)
     (when title (xcb:set-wm-name conn window title))
     (when class (xcb:set-wm-class conn window class (or resource-class (string-capitalize class))))
     (xcb:set-net-wm-pid         conn window)
     (xcb:set-net-wm-window-type conn window :normal)
     (xcb:set-wm-delete-protocol conn window)
     (when (eq kind :undecorated)
       (xcb:set-motif-wm-hints conn window :decorations nil))))
  window)

;;; The toplevel widget

(defclass toplevel (widget)
  ((kind     :initarg :kind :initform :managed :accessor toplevel-kind
             :documentation ":MANAGED, :UNDECORATED, :OVERRIDE-REDIRECT or :ROOT.
             A widget whose kind is an option sets it before REALIZE.")
   (title    :initarg :title :initform nil :accessor toplevel-title
             :documentation "WM_NAME.")
   (wm-class :initarg :wm-class :initform nil :accessor toplevel-wm-class
             :documentation "The instance part of WM_CLASS.")
   (resource-class :initarg :resource-class :initform nil
             :accessor toplevel-resource-class
             :documentation "The class part of WM_CLASS; NIL for WM-CLASS
             capitalised.")
   (control-window :initform nil :reader toplevel-control-window
             :documentation "For a :ROOT toplevel, the hidden window that
             receives REQUEST-CLOSE's message; NIL otherwise.")
   (closing  :initform nil :accessor toplevel-closing-p
             :documentation "Set by REQUEST-CLOSE, from any thread.")))

(defun toplevel-message-window (tl)
  "The window REQUEST-CLOSE sends to."
  (or (toplevel-control-window tl) (widget-window tl)))

(defgeneric realize (toplevel &key x y border-width background-pixel border-pixel event-mask)
  (:documentation "Make TOPLEVEL's window ready to map and return it.
   Creates it, unless TOPLEVEL already has one, as an InputOutput child
   of the root at X, Y with TOPLEVEL's size, BORDER-WIDTH,
   BACKGROUND-PIXEL, BORDER-PIXEL and EVENT-MASK (override-redirect for
   that kind); then applies SETUP-TOPLEVEL.  A :ROOT toplevel must
   already have its window, which is left as it is.")
  (:method ((tl toplevel) &key (x 0) (y 0) (border-width 0) background-pixel border-pixel
                              (event-mask 0))
    (let ((conn (widget-connection tl))
          (kind (toplevel-kind tl)))
      (when (eq kind :root)
        (unless (widget-window tl)
          (error "A :ROOT toplevel needs its window (the root) before REALIZE."))
        (let ((cw (xcb:generate-id conn)))
          ;; InputOnly, never mapped: only REQUEST-CLOSE's message reaches it.
          (xcb::create-window conn 0 cw (xcb:root (first (xcb:conn-roots conn)))
                              0 0 1 1 0 2 0 0)
          (setf (slot-value tl 'control-window) cw))
        (return-from realize (widget-window tl)))
      (unless (widget-window tl)
        (let ((win (xcb:generate-id conn))
              (attrs '()))
          ;; Value-list order is by mask bit: back-pixel #x2, border-pixel
          ;; #x8, override-redirect #x200, event-mask #x800.
          (when background-pixel (setf (getf attrs :background-pixel) background-pixel))
          (when border-pixel     (setf (getf attrs :border-pixel) border-pixel))
          (when (eq kind :override-redirect) (setf (getf attrs :override-redirect) 1))
          (setf (getf attrs :event-mask) event-mask)
          (apply #'xcb::create-window conn 0 win (xcb:root (first (xcb:conn-roots conn)))
                 x y (max 1 (widget-width tl)) (max 1 (widget-height tl)) border-width 1 0
                 (logior (if background-pixel #x2 0) (if border-pixel #x8 0)
                         (if (eq kind :override-redirect) #x200 0) #x800)
                 attrs)
          (setf (widget-window tl) win)))
      (setup-toplevel conn (widget-window tl) :kind kind
                      :title (toplevel-title tl) :class (toplevel-wm-class tl)
                      :resource-class (toplevel-resource-class tl))
      (widget-window tl))))

(defun show (tl)
  "Map TL's window; a :ROOT toplevel's is left alone."
  (unless (eq (toplevel-kind tl) :root)
    (xcb::map-window (widget-connection tl) (widget-window tl)))
  tl)

(defgeneric unrealize (toplevel)
  (:documentation "Destroy the windows REALIZE made, or was given, for
   TOPLEVEL; a :ROOT toplevel's window is not its own and stays.  Widgets
   mixed into a toplevel free their own windows in :BEFORE methods."))

(defmethod unrealize ((tl toplevel))
  (let ((conn (widget-connection tl)))
    (when (toplevel-control-window tl)
      (ignore-errors (xcb::destroy-window conn (toplevel-control-window tl)))
      (setf (slot-value tl 'control-window) nil))
    (when (and (widget-window tl) (not (eq (toplevel-kind tl) :root)))
      (ignore-errors (xcb::destroy-window conn (widget-window tl))))
    (setf (widget-window tl) nil)))

(defun request-close (tl &key display)
  "Ask TL to close, from any thread: set TOPLEVEL-CLOSING-P and wake its
   event loop with a WM_DELETE_WINDOW message, sent on a connection of
   its own to DISPLAY (default $DISPLAY).  Before TL has a window, only
   the flag is set; RUN-WIDGET sees it when it starts."
  (setf (toplevel-closing-p tl) t)
  (let ((target (toplevel-message-window tl)))
    (when target
      (ignore-errors
       (xcb:call-with-x-connection
        (lambda (c)
          ;; Event mask 0: the event goes to the client that created TARGET.
          (xcb:send-client-message c target 0 target
                                   (xcb:intern-atom-id c "WM_PROTOCOLS")
                                   (list (xcb:intern-atom-id c "WM_DELETE_WINDOW") 0))
          (xcb:flush c))
        :display display))))
  tl)

;;; toplevel.lisp ends here
