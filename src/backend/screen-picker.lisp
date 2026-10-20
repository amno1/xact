;;; screen-picker.lisp — interactive screen colour picker with drag-zoom.
;;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;;; License: GPLv3+
;;;
;;; PICK-COLOR-FROM-SCREEN grabs the pointer and keyboard and lets the
;;; user pick a colour anywhere on the screen: a click samples the pixel
;;; under the pointer; a drag selects a rectangle, shown as a rubber
;;; band, which then opens 4x magnified in a window to click a pixel in.
;;; Escape, or any button but the first, cancels.  It returns when the
;;; pick is done, so call it from an interactor's action (see
;;; SCREEN-PICKER-MIXIN in mixins/screen-picker.lisp), not from a draw.
;;;
;;; It is an xact widget of its own, on the root window, which the grab
;;; sends every pointer and key event to:
;;;
;;;   PICKER-CANCEL   Escape or another button: give up.
;;;   PICKER-SELECT   press, drag (rubber band), release: sample the
;;;                   pixel, or after a drag open the zoom window.
;;;   PICKER-ZOOM     replaces PICKER-SELECT once the zoom window is
;;;                   open: a click in it samples a magnified pixel.
;;;
;;; The widget's DRAW paints the zoom window, so an Expose of it redraws
;;; it through RUN-WIDGET as usual.

(in-package #:xact)

(defun %unpack-pixel (conn visual pixel)
  "Extract R G B from a packed pixel by masking against VISUAL."
  (declare (ignore conn))
  (let* ((rm (xcb::red-mask   visual))
         (gm (xcb::green-mask visual))
         (bm (xcb::blue-mask  visual)))
    (flet ((chan (mask)
             (if (zerop mask)
                 0
                 (let* ((shift (loop for i from 0
                                     until (logbitp 0 (ash mask (- i)))
                                     finally (return i)))
                        (bits  (logcount mask))
                        (val   (ldb (byte bits shift) pixel)))
                   (if (= bits 8)
                       val
                       (round (* val 255) (1- (ash 1 bits))))))))
      (values (chan rm) (chan gm) (chan bm)))))

(defun %create-crosshair-cursor (conn)
  "Create and return an XC_crosshair cursor id from the standard cursor font."
  (let ((fid (xcb:generate-id conn))
        (cid (xcb:generate-id conn)))
    (handler-case
        (progn
          (xcb::open-font conn fid "cursor")
          ;; XC_crosshair is glyph 34, mask is 35
          (xcb::create-glyph-cursor conn cid fid fid 34 35 #xFFFF #xFFFF #xFFFF 0 0 0)
          (xcb::close-font conn fid)
          cid)
      (error () nil))))

(defun %create-rubber-band-gc (conn root screen)
  "Create an XOR GC on ROOT with dashed line style and IncludeInferiors."
  (let ((gc (xcb:generate-id conn)))
    (xcb::create-gc conn gc root
                    (logior xcb-const:+gc-function+
                            xcb-const:+gc-foreground+
                            xcb-const:+gc-subwindow-mode+
                            xcb-const:+gc-line-style+)
                    :function 6                          ; GXxor
                    :foreground (xcb:white-pixel screen)
                    :subwindow-mode 1                    ; IncludeInferiors
                    :line-style 1)                       ; LineOnOffDash
    gc))

(defun %draw-rect (conn root gc x y w h)
  "Draw rectangle outline with GC (XOR toggles line pixels)."
  (xcb::poly-rectangle conn root gc
                       (list (make-instance 'xcb::rectangle
                                            :x x :y y
                                            :width w :height h)))
  (xcb:flush conn))

(defun %grab-screen-rect (conn root depth screen-w screen-h x y w h)
  "Grab a rectangle (X Y W H) from ROOT into a pixel octet buffer.
   Clamps to screen bounds. Returns (VALUES PIXELS CLAMPED-W CLAMPED-H CLAMPED-X CLAMPED-Y)."
  (let* ((cx (max 0 (min (1- screen-w) x)))
         (cy (max 0 (min (1- screen-h) y)))
         (cw (max 1 (min (- screen-w cx) w)))
         (ch (max 1 (min (- screen-h cy) h)))
         (pm (xcb:generate-id conn))
         (gc (xcb:generate-id conn)))
    (xcb::create-pixmap conn depth pm root cw ch)
    (xcb::create-gc conn gc pm
                    (logior xcb-const:+gc-foreground+ xcb-const:+gc-subwindow-mode+)
                    :foreground 0
                    :subwindow-mode 1)
    (xcb::copy-area conn root pm gc cx cy 0 0 cw ch)
    (let* ((cookie (xcb::get-image conn 2 pm 0 0 cw ch #xFFFFFFFF))
           (reply  (xcb::get-image-reply conn cookie))
           (data   (slot-value reply 'xcb::data)))
      (xcb::free-gc conn gc)
      (xcb::free-pixmap conn pm)
      (values data cw ch cx cy))))

(defun %scale-pixels-4x (src-data src-w src-h)
  "Scale SRC-DATA 4 times nearest-neighbour.
   Returns (VALUES DST-DATA (* SRC-W 4) (* SRC-H 4))."
  (let* ((dst-w (* src-w 4))
         (dst-h (* src-h 4))
         (dst-data (make-array (* dst-w dst-h 4) :element-type '(unsigned-byte 8))))
    (dotimes (y src-h)
      (dotimes (x src-w)
        (let ((so (* 4 (+ (* y src-w) x))))
          (dotimes (dy 4)
            (dotimes (dx 4)
              (let ((do (* 4 (+ (* (+ (* y 4) dy) dst-w) (+ (* x 4) dx)))))
                (setf (aref dst-data do)       (aref src-data so)
                      (aref dst-data (+ do 1)) (aref src-data (+ so 1))
                      (aref dst-data (+ do 2)) (aref src-data (+ so 2))
                      (aref dst-data (+ do 3)) (aref src-data (+ so 3)))))))))
    (values dst-data dst-w dst-h)))

(defun %pixel-at (data w x y lsb)
  "Get packed pixel at (X, Y) from DATA."
  (let ((offset (* 4 (+ (* y w) x))))
    (if lsb
        (logior (aref data offset)
                (ash (aref data (+ offset 1))  8)
                (ash (aref data (+ offset 2)) 16)
                (ash (aref data (+ offset 3)) 24))
        (logior (aref data (+ offset 3))
                (ash (aref data (+ offset 2))  8)
                (ash (aref data (+ offset 1)) 16)
                (ash (aref data offset)       24)))))

;;; The widget

(defstruct zoom
  "The magnifier: WINDOW and its GC, the captured SOURCE pixels (CW x CH)
   from the screen at SOURCE-X SOURCE-Y, and their 4x magnification
   PIXELS (W x H), shown at WIN-X WIN-Y."
  window gc source cw ch source-x source-y pixels w h win-x win-y)

(defclass screen-picker (widget)
  ((screen  :initarg :screen)
   (visual  :initarg :visual)
   (depth   :initarg :depth)
   (lsb     :initarg :lsb :documentation "Image data is least significant byte first.")
   (cursor  :initform nil)
   (xor-gc  :initform nil :documentation "Draws the rubber band (XOR).")
   (band    :initform nil :documentation "(X Y W H) of the rubber band shown, or NIL.")
   (zoom    :initform nil :documentation "The open ZOOM, or NIL.")
   (result  :initform nil :documentation "(R G B) once picked.")
   (done    :initform nil)))

(defun make-screen-picker (conn)
  (let* ((screen (first (xcb:conn-roots conn)))
         (p (make-instance 'screen-picker
                           :connection conn
                           :window (xcb:root-window conn)
                           :state (make-instance 'state)
                           :width (xcb:width-in-pixels screen)
                           :height (xcb:height-in-pixels screen)
                           :screen screen
                           :visual (xcb:find-root-visual screen)
                           :depth (xcb:root-depth screen)
                           :lsb (eql (xcb:conn-byte-order conn) xcb:+lsb+))))
    (setf (slot-value p 'cursor) (%create-crosshair-cursor conn)
          (slot-value p 'xor-gc) (%create-rubber-band-gc conn (widget-window p) screen))
    p))

(defun %finish (p rgb)
  "End the pick, with RGB (a list R G B) picked, or NIL for none."
  (with-slots (result done) p
    (setf result rgb done t)))

(defun %sample-screen (p x y)
  "The colour (R G B) of the screen pixel at root X Y."
  (with-slots (visual depth lsb) p
    (let ((conn (widget-connection p)))
      (multiple-value-call #'list
        (%unpack-pixel conn visual
                       (%pixel-at (%grab-screen-rect conn (widget-window p) depth
                                                     (widget-width p) (widget-height p)
                                                     x y 1 1)
                                  1 0 0 lsb))))))

;;; The rubber band

(defun %toggle-band (p rect)
  (destructuring-bind (x y w h) rect
    (%draw-rect (widget-connection p) (widget-window p) (slot-value p 'xor-gc) x y w h)))

(defun %show-band (p rect)
  "Show the rubber band at RECT, (X Y W H), instead of where it was."
  (with-slots (band) p
    (when band (%toggle-band p band))      ; XOR: drawing again erases
    (%toggle-band p rect)
    (setf band rect)))

(defun %hide-band (p)
  (with-slots (band) p
    (when band
      (%toggle-band p band)
      (setf band nil))))

;;; The zoom window

(defun %open-zoom (p rect)
  "Capture RECT, (X Y W H), at most 400x400, and show it 4x in a window."
  (with-slots (screen depth zoom) p
    (let* ((conn (widget-connection p))
           (root (widget-window p))
           (sw (widget-width p)) (sh (widget-height p))
           (w (max 4 (min 400 (third rect))))
           (h (max 4 (min 400 (fourth rect))))
           (x (max 0 (min (- sw w) (first rect))))
           (y (max 0 (min (- sh h) (second rect)))))
      (multiple-value-bind (source cw ch cx cy) (%grab-screen-rect conn root depth sw sh x y w h)
        (multiple-value-bind (pixels zw zh) (%scale-pixels-4x source cw ch)
          (let ((z (make-zoom :window (xcb:generate-id conn) :gc (xcb:generate-id conn)
                              :source source :cw cw :ch ch :source-x cx :source-y cy
                              :pixels pixels :w zw :h zh
                              :win-x (max 10 (min (- sw zw 10) cx))
                              :win-y (max 10 (min (- sh zh 10) cy)))))
            (xcb::create-window conn 0 (zoom-window z) root (zoom-win-x z) (zoom-win-y z) zw zh 2
                                xcb-const:+window-class-input-output+ 0
                                (logior xcb-const:+cw-back-pixel+ xcb-const:+cw-border-pixel+
                                        xcb-const:+cw-override-redirect+ xcb-const:+cw-event-mask+)
                                :background-pixel 0
                                :border-pixel (xcb:white-pixel screen)
                                :override-redirect 1
                                :event-mask xcb-const:+event-mask-exposure+)
            (xcb::create-gc conn (zoom-gc z) (zoom-window z) xcb-const:+gc-foreground+ :foreground 0)
            (xcb::map-window conn (zoom-window z))
            (setf zoom z
                  (widget-dirty-p p) t)))))))

(defun %close-zoom (p)
  (with-slots (zoom) p
    (when zoom
      (let ((conn (widget-connection p)))
        (ignore-errors (xcb::destroy-window conn (zoom-window zoom)))
        (ignore-errors (xcb::free-gc conn (zoom-gc zoom))))
      (setf zoom nil))))

(defun %zoom-colour (p root-x root-y)
  "The colour (R G B) of the source pixel shown at root ROOT-X ROOT-Y in
   the zoom window, or NIL when that is outside it."
  (with-slots (zoom visual lsb) p
    (let ((x (- root-x (zoom-win-x zoom)))
          (y (- root-y (zoom-win-y zoom))))
      (when (and (<= 0 x (1- (zoom-w zoom))) (<= 0 y (1- (zoom-h zoom))))
        (let ((sx (max 0 (min (1- (zoom-cw zoom)) (floor x 4))))
              (sy (max 0 (min (1- (zoom-ch zoom)) (floor y 4)))))
          (multiple-value-call #'list
            (%unpack-pixel (widget-connection p) visual
                           (%pixel-at (zoom-source zoom) (zoom-cw zoom) sx sy lsb))))))))

(defmethod draw ((p screen-picker) surface)
  (declare (ignore surface))
  (with-slots (zoom depth) p
    (when zoom
      (xcb:put-image-chunked (widget-connection p) 2 (zoom-window zoom) (zoom-gc zoom)
                             (zoom-w zoom) (zoom-h zoom) 0 0 0 depth (zoom-pixels zoom)))))

;;; Interactors

(defclass picker-interactor (interactor)
  ((picker :initarg :picker :reader picker)))

(defclass picker-cancel (picker-interactor) ()
  (:documentation "Escape, or any button but the first, gives up."))

(defmethod handle-event ((i picker-cancel) (ev key-press))
  (when (eql (event-keysym ev) :escape)
    (%finish (picker i) nil)
    t))

(defmethod handle-event ((i picker-cancel) (ev button-press))
  (unless (eql (event-button ev) 1)
    (%finish (picker i) nil)
    t))

(defclass picker-select (continuous-mixin picker-interactor)
  ((anchor   :initform nil :documentation "Root (X Y) of the press.")
   (dragging :initform nil))
  (:documentation "Press and release samples the pixel; press, drag and
   release selects a rectangle and opens it in the zoom window."))

(defconstant +drag-threshold+ 4
  "Pixels the pointer must move before a press becomes a drag.")

(defmethod start-interaction ((i picker-select) (ev button-press))
  (setf (slot-value i 'anchor) (list (event-root-x ev) (event-root-y ev))
        (slot-value i 'dragging) nil))

(defun %drag-rect (anchor x y)
  "The rectangle (X Y W H) between ANCHOR, (X Y), and X Y."
  (destructuring-bind (ax ay) anchor
    (list (min ax x) (min ay y) (max 1 (abs (- x ax))) (max 1 (abs (- y ay))))))

(defmethod handle-event ((i picker-select) (ev motion-event))
  (with-slots (anchor dragging) i
    (when (eq (interactor-state i) :active)
      (destructuring-bind (ax ay) anchor
        (let ((x (event-root-x ev)) (y (event-root-y ev)))
          (when (or dragging
                    (> (abs (- x ax)) +drag-threshold+)
                    (> (abs (- y ay)) +drag-threshold+))
            (setf dragging t)
            (%show-band (picker i) (%drag-rect anchor x y)))))
      t)))

(defmethod stop-interaction ((i picker-select) (ev button-release))
  (let ((p (picker i)))
    (with-slots (anchor dragging) i
      (if dragging
          ;; Hand over to the zoom window.
          (let ((rect (slot-value p 'band)))
            (%hide-band p)
            (%open-zoom p rect)
            (widget-remove-interactor p i)
            (widget-add-interactor p (make-instance 'picker-zoom :window (widget-window p)
                                                                 :picker p)))
          (%finish p (%sample-screen p (event-root-x ev) (event-root-y ev)))))))

(defclass picker-zoom (picker-interactor) ()
  (:documentation "A click in the zoom window samples the pixel under it;
   a click elsewhere gives up."))

(defmethod handle-event ((i picker-zoom) (ev button-press))
  (when (eql (event-button ev) 1)
    (let ((p (picker i)))
      (%finish p (%zoom-colour p (event-root-x ev) (event-root-y ev))))
    t))

;;; Running a pick

(defun %grab (p)
  "Grab the pointer, with the crosshair, and the keyboard; true if the
   pointer grab succeeded."
  (let ((conn (widget-connection p))
        (root (widget-window p)))
    (prog1 (zerop (slot-value (xcb::grab-pointer-reply
                               conn (xcb::grab-pointer
                                     conn 0 root
                                     (logior xcb-const:+event-mask-button-press+
                                             xcb-const:+event-mask-button-release+
                                             xcb-const:+event-mask-pointer-motion+)
                                     xcb-const:+grab-mode-async+ xcb-const:+grab-mode-async+
                                     0 (or (slot-value p 'cursor) 0) 0))
                              'xcb::status))
      (ignore-errors
       (xcb::grab-keyboard-reply
        conn (xcb::grab-keyboard conn 0 root 0
                                 xcb-const:+grab-mode-async+ xcb-const:+grab-mode-async+))))))

(defun %release (p)
  "Undo everything the pick did to the screen, and free what it made."
  (let ((conn (widget-connection p)))
    (ignore-errors (%hide-band p))
    (%close-zoom p)
    (ignore-errors (xcb::ungrab-pointer conn 0))
    (ignore-errors (xcb::ungrab-keyboard conn 0))
    (with-slots (xor-gc cursor) p
      (when xor-gc (ignore-errors (xcb::free-gc conn xor-gc)))
      (when cursor (ignore-errors (xcb::free-cursor conn cursor))))
    (xcb:flush conn)))

(defun pick-color-from-screen (conn &key stop-fn)
  "Let the user pick a colour from anywhere on the screen (see the top of
   this file).  Returns (VALUES R G B), or NIL when the pick is cancelled
   or STOP-FN, called now and then, returns true."
  (let ((p (make-screen-picker conn)))
    (unwind-protect
         (when (%grab p)
           (dolist (i (list (make-instance 'picker-select :window (widget-window p) :picker p)
                            ;; Added last, so it sees Escape and other
                            ;; buttons before the selection does.
                            (make-instance 'picker-cancel :window (widget-window p) :picker p)))
             (widget-add-interactor p i))
           (run-widget p nil
                       :stop-when (lambda ()
                                    (or (slot-value p 'done)
                                        (and stop-fn (funcall stop-fn))))
                       :idle-sleep 0.01))
      (%release p))
    (let ((rgb (slot-value p 'result)))
      (if rgb (values-list rgb) nil))))

;;; screen-picker.lisp ends here
