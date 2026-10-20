;;; color-picker.lisp — a colour picker widget
;;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;;; License: GPLv3+

;;; Commentary:

;; COLOR-PICKER is a widget that fills its window with a colour picker:
;; a saturation/value square and a hue strip, a swatch with the colour's
;; hex value, a Pick button that samples the screen (PICK-COLOR-FROM-SCREEN),
;; and three sliders, R G B or H S V by its MODE.  Text is antialiased
;; TrueType, through xcb-truetype, which is why this file is the
;; separate system xact/color-picker.
;;
;; The picker builds its child windows on its WIDGET-WINDOW, the parent
;; it fills.  Mixed into a TOPLEVEL, REALIZE builds them after the window
;; and UNREALIZE frees them; otherwise call PICKER-OPEN and PICKER-CLOSE
;; around the run.  PICKER-OPEN also adds its interactors: PICKER-INPUT,
;; which redraws the parts that are exposed and drags in the square, the
;; strip and the sliders, and a RESIZE-INTERACTOR that lays it out again
;; when the window changes size.
;;
;;   (defclass my-picker (color-picker toplevel) ()
;;     (:default-initargs :width 360 :height 500 :title "Colours"))
;;   (let ((p (make-instance 'my-picker :connection conn
;;                           :on-change (lambda (r g b) ...))))
;;     (realize p :x 100 :y 100 :event-mask ...)
;;     (show p)
;;     (run-widget p (widget-window p) :stop-when ...)
;;     (unrealize p))
;;
;; The colour lives in cells of the widget's STATE (see state.lisp):
;; :R :G :B (0..255) and :H (0..359), :S :V (0..255), all writable, tied by
;; a relation (RELATE) with an RGB->HSV and an HSV->RGB method.  Each
;; input writes the representation it works in -- the square and the hue
;; strip HSV, the sliders RGB or HSV by mode, the screen sampler RGB --
;; and the relation computes the other.  RGB->HSV keeps the current hue
;; of a grey and saturation of black, which RGB leaves undefined, so the
;; hue strip does not jump to red as the square reaches its grey edge.
;;
;; PICKER-COLOR and SET-PICKER-COLOR speak R G B.  ON-CHANGE is called
;; with R G B whenever the user changes it.

;;; Code:

(in-package #:xact)

(defconstant +picker-indicator-size+ 11)   ; SV square marker size

(defclass color-picker (widget)
  ((mode          :initarg :mode          :initform :rgb :accessor picker-mode
                  :type (member :rgb :hsv)
                  :documentation "Initial slider mode (:rgb or :hsv).")
   ;; Computed layout dimensions
   (sv-w          :initform 260           :accessor picker-sv-w)
   (sv-h          :initform 260           :accessor picker-sv-h)
   (hue-w         :initform 24            :accessor picker-hue-w)
   (swatch-w      :initform 56            :accessor picker-swatch-w)
   (echo-w        :initform 160           :accessor picker-echo-w)
   (pick-w        :initform 70            :accessor picker-pick-w)
   (bar-w         :initform 240           :accessor picker-bar-w)
   (bar-h         :initform 22            :accessor picker-bar-h)
   ;; Sub-windows
   (sv-window     :initform nil           :accessor picker-sv-window)
   (sv-indicator  :initform nil           :accessor picker-sv-indicator)
   (hue-window    :initform nil           :accessor picker-hue-window)
   (hue-indicator :initform nil           :accessor picker-hue-indicator)
   (swatch-window :initform nil           :accessor picker-swatch-window)
   (hex-window    :initform nil           :accessor picker-hex-window)
   (hex-drawable  :initform nil           :accessor picker-hex-drawable)
   (pick-button   :initform nil           :accessor picker-pick-button)
   (pick-drawable :initform nil           :accessor picker-pick-drawable)
   (bars          :initform nil           :accessor picker-bars)
   ;; TrueType fonts
   (font-family   :initarg :font-family   :initform nil :accessor picker-font-family
                  :type (or null string)
                  :documentation "TrueType font family.")
   (font-subfamily :initarg :font-subfamily :initform nil :accessor picker-font-subfamily
                  :type (or null string)
                  :documentation "TrueType font subfamily.")
   (font-size     :initarg :font-size     :initform 16  :accessor picker-font-size
                  :type (integer 8)
                  :documentation "TrueType font size in pixels.")
   (tt-font       :initform nil           :accessor picker-tt-font)
   (tt-small-font :initform nil           :accessor picker-tt-small-font)
   ;; GCs / theme colours
   (draw-gc       :initform nil           :accessor picker-draw-gc)
   (pixel-bg      :initarg :pixel-bg      :initform nil :accessor picker-pixel-bg
                  :documentation "Background pixel; NIL for a dark grey.")
   (pixel-fg      :initarg :pixel-fg      :initform nil :accessor picker-pixel-fg
                  :documentation "Text pixel; NIL for an off-white.")
   (pixel-card    :initform nil           :accessor picker-pixel-card)
   (pixel-border  :initform nil           :accessor picker-pixel-border)
   (pixel-btn     :initform nil           :accessor picker-pixel-btn)
   (pixel-btn-border :initform nil        :accessor picker-pixel-btn-border)
   ;; Caches
   (sv-buffer     :initform nil           :accessor picker-sv-buffer)
   (sv-buffer-hue :initform -1            :accessor picker-sv-buffer-hue)
   (hue-buffer    :initform nil           :accessor picker-hue-buffer)
   ;; Callbacks
   (on-change     :initarg :on-change     :initform nil :accessor picker-on-change
                  :documentation "Called with R G B when the user changes the colour.")
   (on-pick       :initarg :on-pick       :initform nil :accessor picker-on-pick
                  :documentation "Called with the picker when Pick is clicked; NIL
                  samples the screen with PICK-COLOR-FROM-SCREEN.")
   ;; Drag
   (drag-target   :initform nil           :accessor picker-drag-target)
   (drag-bar      :initform nil           :accessor picker-drag-bar)
   (interactors-added :initform nil)))

;;; The colour cells

(defmethod initialize-instance :after ((w color-picker) &key)
  (let ((st (widget-state w)))
    (setf (cref st :r) 0 (cref st :g) 0 (cref st :b) 0
          (cref st :h) 0 (cref st :s) 0 (cref st :v) 0)
    (relate st
            '(:r :g :b) '(:h :s :v)
            (lambda (r g b)
              ;; Keep the hue of a grey, and the saturation of black, as
              ;; they were: RGB says nothing about them.
              (multiple-value-bind (h s v) (rgb->hsv r g b)
                (values (if (zerop s) (cref st :h) h)
                        (if (zerop v) (cref st :s) s)
                        v)))
            '(:h :s :v) '(:r :g :b)
            #'hsv->rgb)))

(defun picker-hue (w) (cref (widget-state w) :h))
(defun picker-sat (w) (cref (widget-state w) :s))
(defun picker-val (w) (cref (widget-state w) :v))

(defun picker-color (w)
  "Return (VALUES R G B) 0..255."
  (let ((st (widget-state w)))
    (values (cref st :r) (cref st :g) (cref st :b))))

(defun %picker-write-rgb (w r g b)
  (let ((st (widget-state w)))
    (setf (cref st :r) r (cref st :g) g (cref st :b) b)))

(defun %picker-write-hsv (w h s v)
  (let ((st (widget-state w)))
    (setf (cref st :h) h (cref st :s) s (cref st :v) v)))

(defun %picker-changed (w old-hue)
  "Redraw after the user changed W's colour, the hue-dependent gradients
   only when the hue moved, and tell ON-CHANGE."
  (if (eql old-hue (picker-hue w))
      (%picker-redraw-all-dynamic w)
      (%picker-redraw-hue-dependent w))
  (when (picker-on-change w)
    (multiple-value-call (picker-on-change w) (picker-color w))))

(defun picker-screen (w)
  (first (xcb:conn-roots (widget-connection w))))

(defun picker-visual (w)
  "The root visual's VISUALTYPE, for XCB:RGB->PIXEL."
  (xcb:find-root-visual (picker-screen w)))

(defun picker-depth (w)
  (xcb:root-depth (picker-screen w)))

(defparameter *picker-font-candidates*
  '(("DejaVu Sans Mono" "Bold")
    ("Liberation Mono"  "Bold")
    ("Ubuntu Mono"      "Bold")
    ("Noto Sans Mono"   "Bold")
    ("FreeMono"         "Bold")
    ("DejaVu Sans Mono" "Book")
    ("Liberation Mono"  "Regular")
    ("DejaVu Sans"      "Bold")
    ("FreeSans"         "Regular")))

(defun %picker-resolve-font (&key family subfamily (size 16))
  "Find and return an XCB-TRUETYPE:FONT instance."
  (unless (plusp (hash-table-count xcb-truetype:*font-cache*))
    (xcb-truetype:cache-fonts))
  (let* ((fam (or family
                  (loop for (f s) in *picker-font-candidates*
                        when (and (gethash f xcb-truetype:*font-cache*)
                                  (or (null s) (gethash s (gethash f xcb-truetype:*font-cache*))))
                          return f)
                  (first (xcb-truetype:get-font-families))))
         (sub (or subfamily
                  (when (and (null family) (gethash fam xcb-truetype:*font-cache*))
                    (loop for (f s) in *picker-font-candidates*
                          when (and (string-equal f fam)
                                    s
                                    (gethash s (gethash fam xcb-truetype:*font-cache*)))
                            return s)))))
    (make-instance 'xcb-truetype:font
                   :family fam
                   :subfamily sub
                   :size size)))


(defstruct picker-bar
  window              ; gradient drawable
  indicator           ; child indicator overlay
  label-window        ; label window ("R", "G", "B" etc.)
  label-drawable      ; xcb-truetype:window (not owned)
  value-window        ; value window ("255", "180°" etc.)
  value-drawable      ; xcb-truetype:window (not owned)
  kind)               ; :r :g :b :h :s :v


(defun rgb->hsv (r g b)
  "R G B are 0..255. Returns (VALUES H S V) with H 0..359, S V 0..255."
  (let* ((mx (max r g b))
         (mn (min r g b))
         (d  (- mx mn))
         (v  mx)
         (s  (if (zerop mx) 0 (round (* 255 d) mx)))
         (h  (if (zerop d)
                 0
                 (let ((hh (* 60.0d0
                              (cond ((= mx r) (/ (- g b) (float d 1.0d0)))
                                    ((= mx g) (+ 2 (/ (- b r) (float d 1.0d0))))
                                    (t       (+ 4 (/ (- r g) (float d 1.0d0))))))))
                   (mod (round hh) 360)))))
    (values h s v)))

(defun hsv->rgb (h s v)
  "H 0..359; S V 0..255. Returns (VALUES R G B) 0..255."
  (if (zerop s)
      (values v v v)
      (let* ((hf (/ (mod h 360) 60.0d0))
             (i  (floor hf))
             (f  (- hf i))
             (sf (/ s 255.0d0))
             (p  (round (* v (- 1.0d0 sf))))
             (q  (round (* v (- 1.0d0 (* sf f)))))
             (t- (round (* v (- 1.0d0 (* sf (- 1.0d0 f)))))))
        (case i
          (0 (values v t- p))
          (1 (values q v p))
          (2 (values p v t-))
          (3 (values p q v))
          (4 (values t- p v))
          (t (values v p q))))))

(defun %picker-bar-name (part i)
  (intern (format nil "~A-~D" part i) :keyword))

(defun %picker-layout (w)
  "W's layout (see layout.lisp): the SV square and hue strip, which take
   the extra height, over the swatch, hex and Pick row, over three slider
   rows of label, bar and value.  Each box's PLACE records the size the
   drawing code reads and moves its child window, when it has one."
  (flet ((b (name &rest initargs)
           (apply #'box :name name
                        :place (lambda (box) (%picker-place w name box))
                        initargs)))
    (column (:margin 12 :spacing 10)
      (row (:spacing 8 :stretch-y 1)
        (b :sv  :min-width 62 :min-height 82 :stretch-x 1 :stretch-y 1)
        (b :hue :min-width 26 :min-height 82))
      (row (:spacing 8)
        (b :swatch :min-width 58 :min-height 38)
        (b :hex    :min-width 62 :min-height 38 :stretch-x 1)
        (b :pick   :min-width 72 :min-height 38))
      (column (:spacing 8)
        (row (:spacing 8)
          (b :label-0 :min-width 24 :min-height 24)
          (b :bar-0   :min-width 62 :min-height 24 :stretch-x 1)
          (b :value-0 :min-width 46 :min-height 24))
        (row (:spacing 8)
          (b :label-1 :min-width 24 :min-height 24)
          (b :bar-1   :min-width 62 :min-height 24 :stretch-x 1)
          (b :value-1 :min-width 46 :min-height 24))
        (row (:spacing 8)
          (b :label-2 :min-width 24 :min-height 24)
          (b :bar-2   :min-width 62 :min-height 24 :stretch-x 1)
          (b :value-2 :min-width 46 :min-height 24))))))

(defun %picker-place (w name box)
  "Record BOX's size for the part NAME of W, and move its window there.
   Bordered windows get the box less their 1-pixel border."
  (let* ((bars (picker-bars w))
         (part (string name))
         (i (and (find #\- part) (parse-integer part :start (1+ (position #\- part)))))
         (bar (and i (nth i bars)))
         (bordered (not (member name '(:label-0 :label-1 :label-2 :value-0 :value-1 :value-2))))
         (inset (if bordered 1 0))
         (iw (max 1 (- (node-width box) (* 2 inset))))
         (ih (max 1 (- (node-height box) (* 2 inset))))
         (window (case name
                   (:sv (setf (picker-sv-w w) iw (picker-sv-h w) ih) (picker-sv-window w))
                   (:hue (setf (picker-hue-w w) iw) (picker-hue-window w))
                   (:swatch (setf (picker-swatch-w w) iw) (picker-swatch-window w))
                   (:hex (setf (picker-echo-w w) iw) (picker-hex-window w))
                   (:pick (setf (picker-pick-w w) iw) (picker-pick-button w))
                   ((:bar-0 :bar-1 :bar-2)
                    (setf (picker-bar-w w) iw (picker-bar-h w) ih)
                    (and bar (picker-bar-window bar)))
                   ((:label-0 :label-1 :label-2) (and bar (picker-bar-label-window bar)))
                   ((:value-0 :value-1 :value-2) (and bar (picker-bar-value-window bar))))))
    (when window
      (xcb::configure-window (widget-connection w) window
                             (logior xcb-const:+config-window-x+ xcb-const:+config-window-y+
                                     xcb-const:+config-window-width+ xcb-const:+config-window-height+)
                             :x (node-x box) :y (node-y box) :width iw :height ih))))

(defun %picker-make-child (conn parent x y w h &key bg (border 0) (border-pixel 0) event-mask)
  (let ((wid  (xcb:generate-id conn))
        (mask (logior (if bg         xcb-const:+cw-back-pixel+   0)
                      (if (plusp border) xcb-const:+cw-border-pixel+ 0)
                      (if event-mask xcb-const:+cw-event-mask+   0))))
    (apply #'xcb::create-window conn 0 wid parent x y w h border
           xcb-const:+window-class-input-output+ 0 mask
           (append (when bg     (list :background-pixel bg))
                   (when (plusp border) (list :border-pixel border-pixel))
                   (when event-mask (list :event-mask event-mask))))
    wid))

(defconstant +picker-interactive-mask+
  (logior xcb-const:+event-mask-button-press+
          xcb-const:+event-mask-button1-motion+
          xcb-const:+event-mask-button-release+
          xcb-const:+event-mask-exposure+))

(defun %picker-create-windows (w)
  ;; The layout gives the sizes; the windows are made, then put in place
  ;; by a second RELAYOUT once they exist.
  (setf (widget-layout w) (%picker-layout w))
  (relayout w)
  (let* ((conn (widget-connection w))
         (parent (widget-window w))
         (ox 0)
         (oy 0)
         (screen (picker-screen w))
         (visual (picker-visual w))
         (margin 12)
         (gap 8)
         (section-gap 10)
         (sv-w (picker-sv-w w))
         (sv-h (picker-sv-h w))
         (hue-w (picker-hue-w w))
         (echo-h 36)
         (bar-w (picker-bar-w w))
         (bar-h (picker-bar-h w))
         (label-w 24)
         (value-w 46)
         (fsize (picker-font-size w)))
    ;; Setup colors & theme
    (unless (picker-pixel-bg w)
      (setf (picker-pixel-bg w) (xcb:rgb->pixel visual 24 24 28)))
    (unless (picker-pixel-fg w)
      (setf (picker-pixel-fg w) (xcb:rgb->pixel visual 240 240 245)))
    (setf (picker-pixel-card w) (xcb:rgb->pixel visual 34 34 40)
          (picker-pixel-border w) (xcb:rgb->pixel visual 58 58 68)
          (picker-pixel-btn w) (xcb:rgb->pixel visual 44 44 54)
          (picker-pixel-btn-border w) (xcb:rgb->pixel visual 75 75 90))

    ;; Initialize TrueType fonts
    (setf (picker-tt-font w)
          (%picker-resolve-font :family (picker-font-family w)
                               :subfamily (picker-font-subfamily w)
                               :size fsize))
    (setf (picker-tt-small-font w)
          (%picker-resolve-font :family (picker-font-family w)
                               :subfamily (picker-font-subfamily w)
                               :size (max 11 (- fsize 3))))

    ;; SV square (gradient drawn by PUT-IMAGE)
    (setf (picker-sv-window w)
          (%picker-make-child conn parent (+ ox margin) (+ oy margin)
                       sv-w sv-h
                       :bg (picker-pixel-bg w) :border 1 :border-pixel (picker-pixel-border w)
                       :event-mask +picker-interactive-mask+))
    ;; Marker inside SV square
    (setf (picker-sv-indicator w)
          (%picker-make-child conn (picker-sv-window w) 0 0
                       +picker-indicator-size+ +picker-indicator-size+
                       :bg (xcb:black-pixel screen) :border 1 :border-pixel (xcb:white-pixel screen)))

    ;; Hue strip
    (setf (picker-hue-window w)
          (%picker-make-child conn parent (+ ox margin sv-w gap) (+ oy margin)
                       hue-w sv-h
                       :bg (picker-pixel-bg w) :border 1 :border-pixel (picker-pixel-border w)
                       :event-mask +picker-interactive-mask+))
    ;; Marker inside Hue strip
    (setf (picker-hue-indicator w)
          (%picker-make-child conn (picker-hue-window w) 0 0
                       hue-w 4
                       :bg (xcb:black-pixel screen) :border 1 :border-pixel (xcb:white-pixel screen)))

    ;; Middle row: Swatch, Echo window, Pick button
    (let* ((row-y (+ oy margin sv-h section-gap))
           (swatch-w (picker-swatch-w w))
           (pick-w (picker-pick-w w))
           (echo-w (picker-echo-w w))
           (echo-x (+ ox margin swatch-w gap))
           (pick-x (+ echo-x echo-w gap)))
      (setf (picker-swatch-window w)
            (%picker-make-child conn parent (+ ox margin) row-y
                         swatch-w echo-h
                         :bg (picker-pixel-bg w) :border 1 :border-pixel (picker-pixel-border w)
                         :event-mask xcb-const:+event-mask-exposure+))
      (setf (picker-hex-window w)
            (%picker-make-child conn parent echo-x row-y
                         echo-w echo-h
                         :bg (picker-pixel-card w) :border 1 :border-pixel (picker-pixel-border w)
                         :event-mask xcb-const:+event-mask-exposure+))
      (setf (picker-hex-drawable w)
            (make-instance 'xcb-truetype:window
                           :connection conn
                           :id (picker-hex-window w)
                           :visual (xcb:root-visual screen)
                           :screen screen))
      (trivial-garbage:cancel-finalization (picker-hex-drawable w))

      (setf (picker-pick-button w)
            (%picker-make-child conn parent pick-x row-y
                         pick-w echo-h
                         :bg (picker-pixel-btn w) :border 1 :border-pixel (picker-pixel-btn-border w)
                         :event-mask (logior xcb-const:+event-mask-exposure+
                                             xcb-const:+event-mask-button-press+
                                             xcb-const:+event-mask-button-release+)))
      (setf (picker-pick-drawable w)
            (make-instance 'xcb-truetype:window
                           :connection conn
                           :id (picker-pick-button w)
                           :visual (xcb:root-visual screen)
                           :screen screen))
      (trivial-garbage:cancel-finalization (picker-pick-drawable w)))

    ;; Bottom section: 3 Sliders
    (let* ((sliders-start-y (+ oy margin sv-h section-gap echo-h section-gap))
           (slider-gap 8)
           (kinds (if (eq (picker-mode w) :rgb) '(:r :g :b) '(:h :s :v)))
           (bars nil))
      (dotimes (i 3)
        (let* ((kind (nth i kinds))
               (sy (+ sliders-start-y (* i (+ bar-h slider-gap))))
               (lx (+ ox margin))
               (bx (+ lx label-w gap))
               (vx (+ bx bar-w gap))
               ;; Label window
               (l-win (%picker-make-child conn parent lx sy label-w bar-h
                                   :bg (picker-pixel-bg w)
                                   :event-mask xcb-const:+event-mask-exposure+))
               (l-draw (make-instance 'xcb-truetype:window
                                      :connection conn
                                      :id l-win
                                      :visual (xcb:root-visual screen)
                                      :screen screen))
               ;; Bar window
               (b-win (%picker-make-child conn parent bx sy bar-w bar-h
                                          :bg (picker-pixel-bg w)
                                          :border 1
                                          :border-pixel (picker-pixel-border w)
                                          :event-mask +picker-interactive-mask+))
               (ind-win (%picker-make-child conn b-win 0 0 4 bar-h
                                            :bg (xcb:black-pixel screen)
                                            :border 1
                                            :border-pixel (xcb:white-pixel screen)))
               ;; Value window
               (v-win (%picker-make-child conn parent vx sy value-w bar-h
                                   :bg (picker-pixel-bg w)
                                   :event-mask xcb-const:+event-mask-exposure+))
               (v-draw (make-instance 'xcb-truetype:window
                                      :connection conn
                                      :id v-win
                                      :visual (xcb:root-visual screen)
                                      :screen screen)))
          (trivial-garbage:cancel-finalization l-draw)
          (trivial-garbage:cancel-finalization v-draw)
          (push (make-picker-bar :window b-win
                               :indicator ind-win
                               :label-window l-win
                               :label-drawable l-draw
                               :value-window v-win
                               :value-drawable v-draw
                               :kind kind)
                bars)))
      (setf (picker-bars w) (nreverse bars)))

    ;; Draw GC for put-image
    (setf (picker-draw-gc w) (xcb:generate-id conn))
    (xcb::create-gc conn (picker-draw-gc w) parent
                    xcb-const:+gc-foreground+ :foreground (xcb:white-pixel screen))
    (relayout w)))


;;; Buffer generation

;;; The gradient images are built as R G B bytes in typed loops and
;;; packed for PutImage with XCB:PACK-RGB-IMAGE.  The HSV conversion here is
;;; in integers, for speed; it may differ from HSV->RGB by a rounding step,
;;; which a gradient does not show.  The colour the picker reports is
;;; computed exactly, from its cells.

(declaim (inline %picker-hsv8))
(defun %picker-hsv8 (h s v)
  "R G B (0..255) for H (0..359), S and V (0..255), in integers."
  (declare (type (integer 0 359) h) (type (integer 0 255) s v)
           (optimize speed (safety 0)))
  (if (zerop s)
      (values v v v)
      (let* ((region (floor h 60))
             (rem (floor (* (- h (* region 60)) 255) 60))
             (p (floor (* v (- 255 s)) 255))
             (q (floor (* v (- 255 (floor (* s rem) 255))) 255))
             (tt (floor (* v (- 255 (floor (* s (- 255 rem)) 255))) 255)))
        (declare (type (integer 0 5) region) (type (integer 0 255) rem p q tt))
        (case region
          (0 (values v tt p))
          (1 (values q v p))
          (2 (values p v tt))
          (3 (values p q v))
          (4 (values tt p v))
          (t (values v p q))))))

(defmacro %picker-with-rgb ((rgb w h) &body body)
  "Run BODY with RGB bound to a fresh W x H x 3 octet buffer, and return
   it packed for PutImage on CONN's screen (CONN and VISUAL must be bound)."
  `(let ((,rgb (make-array (* ,w ,h 3) :element-type '(unsigned-byte 8))))
     (declare (type (simple-array (unsigned-byte 8) (*)) ,rgb))
     ,@body
     (xcb:pack-rgb-image (xcb:pixel-format visual (xcb:conn-byte-order conn)) ,rgb ,w ,h)))

(defun %picker-build-sv-buffer (conn visual w h hue)
  "A W x H image of the SV square for HUE (0..359): saturation across,
   value up."
  (declare (type (unsigned-byte 16) w h) (type (integer 0 359) hue))
  (%picker-with-rgb (rgb w h)
    (locally (declare (optimize speed (safety 0)))
      (let ((o 0))
        (declare (type fixnum o))
        (dotimes (yy h)
          (let ((v (floor (* 255 (- (1- h) yy)) (max 1 (1- h)))))
            (declare (type (integer 0 255) v))
            (dotimes (xx w)
              (multiple-value-bind (r g b)
                  (%picker-hsv8 hue (floor (* 255 xx) (max 1 (1- w))) v)
                (setf (aref rgb o) r (aref rgb (+ o 1)) g (aref rgb (+ o 2)) b)
                (incf o 3)))))))))

(defun %picker-build-hgradient-buffer (conn visual w h c0 c1)
  "A W x H image going from colour C0 to C1 across the width. Each is (R G B)."
  (declare (type (unsigned-byte 16) w h))
  (destructuring-bind (r0 g0 b0) c0
    (destructuring-bind (r1 g1 b1) c1
      (%picker-with-rgb (rgb w h)
        (dotimes (xx w)
          (let ((f (if (= w 1) 0 (/ xx (1- w)))))
            (setf (aref rgb (* 3 xx))       (round (+ (* r0 (- 1 f)) (* r1 f)))
                  (aref rgb (+ 1 (* 3 xx))) (round (+ (* g0 (- 1 f)) (* g1 f)))
                  (aref rgb (+ 2 (* 3 xx))) (round (+ (* b0 (- 1 f)) (* b1 f))))))
        ;; Every row is the first.
        (loop for yy from 1 below h
              do (replace rgb rgb :start1 (* yy w 3) :end2 (* w 3)))))))

(defun %picker-build-rainbow-buffer (conn visual w h)
  "A vertical rainbow 0->359 in HUE. Each row is a solid colour."
  (declare (type (unsigned-byte 16) w h))
  (%picker-with-rgb (rgb w h)
    (dotimes (yy h)
      (multiple-value-bind (r g b) (%picker-hsv8 (floor (* 359 yy) (max 1 (1- h))) 255 255)
        (loop for o from (* yy w 3) below (* (1+ yy) w 3) by 3
              do (setf (aref rgb o) r (aref rgb (+ o 1)) g (aref rgb (+ o 2)) b))))))

(defun %picker-build-hrainbow-buffer (conn visual w h)
  "A horizontal rainbow 0->359 in HUE across width W."
  (declare (type (unsigned-byte 16) w h))
  (%picker-with-rgb (rgb w h)
    (dotimes (xx w)
      (multiple-value-bind (r g b) (%picker-hsv8 (floor (* 359 xx) (max 1 (1- w))) 255 255)
        (setf (aref rgb (* 3 xx)) r (aref rgb (+ 1 (* 3 xx))) g (aref rgb (+ 2 (* 3 xx))) b)))
    (loop for yy from 1 below h
          do (replace rgb rgb :start1 (* yy w 3) :end2 (* w 3)))))

(defun %picker-put-image-buffer (conn drawable gc w h x y depth buffer)
  (xcb:put-image-chunked conn 2 drawable gc w h x y 0 depth buffer))


;;; Drawing Routines

(defun %picker-redraw-sv-square (w)
  (let* ((conn (widget-connection w))
         (hue  (picker-hue w))
         (sw   (picker-sv-w w))
         (sh   (picker-sv-h w)))
    (unless (and (picker-sv-buffer w)
                 (= (picker-sv-buffer-hue w) hue))
      (setf (picker-sv-buffer w)
            (%picker-build-sv-buffer conn (picker-visual w) sw sh hue)
            (picker-sv-buffer-hue w) hue))
    (xcb:put-image-chunked conn 2 (picker-sv-window w) (picker-draw-gc w)
                           sw sh 0 0 0 (picker-depth w)
                           (picker-sv-buffer w))))

(defun %picker-redraw-hue-strip (w)
  (let* ((conn (widget-connection w))
         (hw   (picker-hue-w w))
         (sh   (picker-sv-h w)))
    (unless (picker-hue-buffer w)
      (setf (picker-hue-buffer w)
            (%picker-build-rainbow-buffer conn (picker-visual w) hw sh)))
    (%picker-put-image-buffer conn (picker-hue-window w) (picker-draw-gc w)
                      hw sh 0 0 (picker-depth w)
                      (picker-hue-buffer w))))

(defun %picker-bar-endpoints (w kind)
  "Return (VALUES C0 C1) — the two RGB triples at the ends of bar KIND."
  (multiple-value-bind (r g b) (picker-color w)
    (case kind
      (:r (values (list   0 g b) (list 255 g b)))
      (:g (values (list r   0 b) (list r 255 b)))
      (:b (values (list r g   0) (list r g 255)))
      (:h (values '(0 0 0) '(0 0 0)))
      (:s (multiple-value-bind (r0 g0 b0) (hsv->rgb (picker-hue w) 0 (picker-val w))
            (multiple-value-bind (r1 g1 b1) (hsv->rgb (picker-hue w) 255 (picker-val w))
              (values (list r0 g0 b0) (list r1 g1 b1)))))
      (:v (multiple-value-bind (r1 g1 b1) (hsv->rgb (picker-hue w) (picker-sat w) 255)
            (values '(0 0 0) (list r1 g1 b1))))
      (t (values '(0 0 0) '(255 255 255))))))

(defun %picker-redraw-bar-label (w bar)
  (let* ((conn   (widget-connection w))
         (screen (picker-screen w))
         (kind   (picker-bar-kind bar))
         (win    (picker-bar-label-window bar))
         (draw   (picker-bar-label-drawable bar))
         (font   (picker-tt-small-font w))
         (label  (case kind
                   (:r "R") (:g "G") (:b "B")
                   (:h "H") (:s "S") (:v "V")
                   (t "")))
         (color  (case kind
                   (:r #xFF6B6B)
                   (:g #x51CF66)
                   (:b #x339AF0)
                   (:h #xFCC419)
                   (:s #x20C997)
                   (:v #x9775FA)
                   (t #xFFFFFF))))
    (xcb::clear-area conn 0 win 0 0 0 0)
    (let* ((ascent (xcb-truetype:font-ascent screen font))
           (descent (abs (xcb-truetype:font-descent screen font)))
           (th (+ ascent descent))
           (tw (xcb-truetype:text-width screen font label))
           (bh (picker-bar-h w))
           (x (max 2 (floor (- 24 tw) 2)))
           (y (+ (floor (- bh th) 2) ascent)))
      (xcb-truetype:draw-text draw font label x y :colour color))))

(defun %picker-redraw-bar-value (w bar)
  (let* ((conn   (widget-connection w))
         (screen (picker-screen w))
         (kind   (picker-bar-kind bar))
         (win    (picker-bar-value-window bar))
         (draw   (picker-bar-value-drawable bar))
         (font   (picker-tt-small-font w))
         (val-str (multiple-value-bind (r g b)
                      (picker-color w)
                    (case kind
                      (:r (format nil "~3D" r))
                      (:g (format nil "~3D" g))
                      (:b (format nil "~3D" b))
                      (:h (format nil "~3D°" (picker-hue w)))
                      (:s (format nil "~3D%" (round (* (picker-sat w) 100) 255)))
                      (:v (format nil "~3D%" (round (* (picker-val w) 100) 255)))
                      (t "")))))
    (xcb::clear-area conn 0 win 0 0 0 0)
    (let* ((ascent (xcb-truetype:font-ascent screen font))
           (descent (abs (xcb-truetype:font-descent screen font)))
           (th (+ ascent descent))
           (tw (xcb-truetype:text-width screen font val-str))
           (bh (picker-bar-h w))
           (vw 46)
           (x (max 2 (- vw tw 2)))
           (y (+ (floor (- bh th) 2) ascent)))
      (xcb-truetype:draw-text draw font val-str x y :colour #xCCCCCC))))

(defun %picker-redraw-bar (w bar)
  (let* ((conn (widget-connection w))
         (kind (picker-bar-kind bar))
         (bw   (picker-bar-w w))
         (bh   (picker-bar-h w))
         (buffer nil))
    (cond
      ((eq kind :h)
       (setf buffer (%picker-build-hrainbow-buffer conn (picker-visual w) bw bh)))
      (t
       (multiple-value-bind (c0 c1) (%picker-bar-endpoints w kind)
         (setf buffer (%picker-build-hgradient-buffer conn (picker-visual w)
                                              bw bh c0 c1)))))
    (%picker-put-image-buffer conn (picker-bar-window bar) (picker-draw-gc w)
                      bw bh 0 0 (picker-depth w) buffer)
    (%picker-redraw-bar-label w bar)
    (%picker-redraw-bar-value w bar)))

(defun %picker-redraw-swatch (w)
  (multiple-value-bind (r g b) (picker-color w)
    (let ((px (xcb:rgb->pixel (picker-visual w) r g b)))
      (xcb::change-window-attributes (widget-connection w)
                                     (picker-swatch-window w)
                                     xcb-const:+cw-back-pixel+
                                     :background-pixel px)
      (xcb::clear-area (widget-connection w) 0 (picker-swatch-window w)
                       0 0 0 0))))

(defun %picker-redraw-hex-text (w)
  (multiple-value-bind (r g b) (picker-color w)
    (let* ((conn  (widget-connection w))
           (screen (picker-screen w))
           (hex   (format nil "#~2,'0X~2,'0X~2,'0X" r g b))
           (mode-tag (if (eq (picker-mode w) :rgb) "RGB" "HSV"))
           (win   (picker-hex-window w))
           (draw  (picker-hex-drawable w))
           (font  (picker-tt-font w))
           (sfont (picker-tt-small-font w)))
      (xcb::clear-area conn 0 win 0 0 0 0)
      (let* ((ascent  (xcb-truetype:font-ascent screen font))
             (descent (abs (xcb-truetype:font-descent screen font)))
             (th      (+ ascent descent))
             (y       (+ (floor (- 36 th) 2) ascent))
             (sascent (xcb-truetype:font-ascent screen sfont))
             (sdescent (abs (xcb-truetype:font-descent screen sfont)))
             (sth     (+ sascent sdescent))
             (sy      (+ (floor (- 36 sth) 2) sascent))
             (tag-w   (xcb-truetype:text-width screen sfont mode-tag))
             (ew      (picker-echo-w w))
             (tag-x   (max (+ 12 (xcb-truetype:text-width screen font hex) 10)
                           (- ew tag-w 12))))
        (xcb-truetype:draw-text draw font hex 12 y :colour (picker-pixel-fg w))
        (xcb-truetype:draw-text draw sfont mode-tag tag-x sy :colour #x888899)))))

(defun %picker-redraw-pick-button (w)
  (let* ((conn  (widget-connection w))
         (screen (picker-screen w))
         (win   (picker-pick-button w))
         (draw  (picker-pick-drawable w))
         (font  (picker-tt-font w))
         (text  "Pick"))
    (xcb::clear-area conn 0 win 0 0 0 0)
    (let* ((ascent  (xcb-truetype:font-ascent screen font))
           (descent (abs (xcb-truetype:font-descent screen font)))
           (th      (+ ascent descent))
           (tw      (xcb-truetype:text-width screen font text))
           (pw      (picker-pick-w w))
           (x       (max 4 (floor (- pw tw) 2)))
           (y       (+ (floor (- 36 th) 2) ascent)))
      (xcb-truetype:draw-text draw font text x y :colour (picker-pixel-fg w)))))

(defun %picker-redraw-indicators (w)
  (let ((conn (widget-connection w))
        (sv-w (picker-sv-w w))
        (sv-h (picker-sv-h w))
        (bar-w (picker-bar-w w)))
    ;; SV square marker: place its center at (sat/255*(sv-w-1), (255-val)/255*(sv-h-1))
    (let* ((x (round (* (picker-sat w) (1- sv-w)) 255))
           (y (round (* (- 255 (picker-val w)) (1- sv-h)) 255)))
      (xcb::configure-window conn (picker-sv-indicator w)
                             (logior xcb-const:+config-window-x+ xcb-const:+config-window-y+)
                             :x (- x (floor +picker-indicator-size+ 2))
                             :y (- y (floor +picker-indicator-size+ 2))))
    ;; Hue strip marker
    (let ((y (round (* (picker-hue w) (1- sv-h)) 359)))
      (xcb::configure-window conn (picker-hue-indicator w)
                             xcb-const:+config-window-y+ :y (max 0 (- y 2))))
    ;; Bar markers
    (dolist (bar (picker-bars w))
      (let* ((kind  (picker-bar-kind bar))
             (value (case kind
                      (:r (nth-value 0 (picker-color w)))
                      (:g (nth-value 1 (picker-color w)))
                      (:b (nth-value 2 (picker-color w)))
                      (:h (picker-hue w))
                      (:s (picker-sat w))
                      (:v (picker-val w))))
             (vmax  (case kind
                      (:h 359)
                      (t 255)))
             (x     (round (* value (1- bar-w)) vmax)))
        (xcb::configure-window conn (picker-bar-indicator bar)
                               xcb-const:+config-window-x+
                               :x (max 0 (min (- bar-w 4) (- x 2))))))))

(defun %picker-map-windows (w)
  "Map every sub-window the widget created."
  (let ((conn (widget-connection w)))
    (dolist (wid (list (picker-sv-window     w)
                       (picker-sv-indicator  w)
                       (picker-hue-window    w)
                       (picker-hue-indicator w)
                       (picker-swatch-window w)
                       (picker-hex-window    w)
                       (picker-pick-button   w)))
      (xcb::map-window conn wid))
    (dolist (bar (picker-bars w))
      (xcb::map-window conn (picker-bar-label-window bar))
      (xcb::map-window conn (picker-bar-window bar))
      (xcb::map-window conn (picker-bar-indicator bar))
      (xcb::map-window conn (picker-bar-value-window bar)))))

(defun %picker-redraw-all-dynamic (w)
  "Redraw everything that can change on colour or mode change."
  (dolist (bar (picker-bars w)) (%picker-redraw-bar w bar))
  (%picker-redraw-swatch w)
  (%picker-redraw-hex-text w)
  (%picker-redraw-indicators w))

(defun %picker-redraw-hue-dependent (w)
  "Redraw the pieces that change when the hue changes."
  (%picker-redraw-sv-square w)
  (%picker-redraw-hue-strip w)
  (%picker-redraw-all-dynamic w))

(defun %picker-redraw-all (w)
  (%picker-redraw-hue-dependent w)
  (%picker-redraw-pick-button w))

(defun picker-redraw-all (w)
  "Force a complete redraw of the widget (gradients, swatch, hex, indicators)."
  (%picker-redraw-all w))

(defun %picker-sv-set (w x y)
  (let ((sw (max 1 (1- (picker-sv-w w))))
        (sh (max 1 (1- (picker-sv-h w))))
        (h  (picker-hue w)))
    (%picker-write-hsv w h
                       (min 255 (max 0 (round (* x 255) sw)))
                       (min 255 (max 0 (round (* (- sh y) 255) sh))))
    (%picker-changed w h)))

(defun %picker-hue-set (w y)
  (let ((sh (max 1 (1- (picker-sv-h w))))
        (h  (picker-hue w)))
    (%picker-write-hsv w (min 359 (max 0 (round (* y 359) sh)))
                       (picker-sat w) (picker-val w))
    (%picker-changed w h)))

(defun %picker-bar-set (w bar x)
  (let* ((bw   (max 1 (1- (picker-bar-w w))))
         (frac (min 1.0d0 (max 0.0d0 (/ (float x 1.0d0) bw))))
         (h    (picker-hue w))
         (s    (picker-sat w))
         (v    (picker-val w)))
    (multiple-value-bind (r g b) (picker-color w)
      (ecase (picker-bar-kind bar)
        (:r (%picker-write-rgb w (round (* frac 255)) g b))
        (:g (%picker-write-rgb w r (round (* frac 255)) b))
        (:b (%picker-write-rgb w r g (round (* frac 255))))
        (:h (%picker-write-hsv w (round (* frac 359)) s v))
        (:s (%picker-write-hsv w h (round (* frac 255)) v))
        (:v (%picker-write-hsv w h s (round (* frac 255))))))
    (%picker-changed w h)))

(defun set-picker-mode (w mode)
  "Switch slider mode to :RGB or :HSV."
  (unless (eq (picker-mode w) mode)
    (setf (picker-mode w) mode)
    (let ((kinds (if (eq mode :rgb) '(:r :g :b) '(:h :s :v))))
      (loop for bar in (picker-bars w)
            for kind in kinds
            do (setf (picker-bar-kind bar) kind)))
    (%picker-redraw-all-dynamic w)))

(defun %picker-resize (w)
  "After RESIZE-INTERACTOR has laid W out again: the gradients are made
   for the old sizes, so make them anew and repaint."
  (setf (picker-sv-buffer w) nil
        (picker-hue-buffer w) nil)
  (%picker-redraw-all w)
  (xcb:flush (widget-connection w)))

(defun set-picker-color (w r g b)
  "Set the widget's colour, redraw, and call its ON-CHANGE callback."
  (let ((h (picker-hue w)))
    (%picker-write-rgb w r g b)
    (%picker-changed w h)))

;;; Input

(defclass picker-input (interactor)
  ((picker :initarg :picker :reader input-picker))
  (:documentation "Redraws the picker's exposed parts, and drags in its
   SV square, hue strip and sliders; a click on Pick calls ON-PICK."))

(defun %picker-for-window-p (w id window)
  (declare (ignore w))
  (and id (eql id window)))

(defmethod handle-event ((i picker-input) (ev expose-event))
  (let* ((w (input-picker i))
         (win (event-window ev)))
    (flet ((is (id) (%picker-for-window-p w id win)))
      (cond
        ((is (widget-window w))          (%picker-redraw-all w) t)
        ((is (picker-sv-window w))       (%picker-redraw-sv-square w) t)
        ((is (picker-hue-window w))      (%picker-redraw-hue-strip w) t)
        ((is (picker-swatch-window w))   (%picker-redraw-swatch w) t)
        ((is (picker-hex-window w))      (%picker-redraw-hex-text w) t)
        ((is (picker-pick-button w))     (%picker-redraw-pick-button w) t)
        (t (dolist (bar (picker-bars w) nil)
             (cond ((is (picker-bar-window bar))
                    (%picker-redraw-bar w bar) (return t))
                   ((is (picker-bar-label-window bar))
                    (%picker-redraw-bar-label w bar) (return t))
                   ((is (picker-bar-value-window bar))
                    (%picker-redraw-bar-value w bar) (return t)))))))))

(defmethod handle-event ((i picker-input) (ev button-press))
  (let* ((w (input-picker i))
         (win (event-window ev)))
    (cond
      ((eql win (picker-pick-button w)) t)
      ((eql win (picker-sv-window w))
       (setf (picker-drag-target w) :sv)
       (%picker-sv-set w (event-x ev) (event-y ev))
       t)
      ((eql win (picker-hue-window w))
       (setf (picker-drag-target w) :hue)
       (%picker-hue-set w (event-y ev))
       t)
      (t
       (dolist (bar (picker-bars w) nil)
         (when (eql win (picker-bar-window bar))
           (setf (picker-drag-target w) :bar
                 (picker-drag-bar w)    bar)
           (%picker-bar-set w bar (event-x ev))
           (return t)))))))

(defmethod handle-event ((i picker-input) (ev motion-event))
  (let ((w (input-picker i)))
    (case (picker-drag-target w)
      (:sv  (%picker-sv-set w (event-x ev) (event-y ev)) t)
      (:hue (%picker-hue-set w (event-y ev)) t)
      (:bar (%picker-bar-set w (picker-drag-bar w) (event-x ev)) t)
      (t nil))))

(defmethod handle-event ((i picker-input) (ev button-release))
  (let ((w (input-picker i)))
    (cond
      ((eql (event-window ev) (picker-pick-button w))
       (setf (picker-drag-target w) nil
             (picker-drag-bar w)    nil)
       (if (picker-on-pick w)
           (funcall (picker-on-pick w) w)
           (picker-pick-from-screen w))
       t)
      ((picker-drag-target w)
       (setf (picker-drag-target w) nil
             (picker-drag-bar w)    nil)
       t)
      (t nil))))

(defun picker-pick-from-screen (w)
  "Sample a colour from the screen with PICK-COLOR-FROM-SCREEN and make
   it W's.  A TOPLEVEL picker's REQUEST-CLOSE ends the sampling too: its
   loop would otherwise swallow the close message."
  (multiple-value-bind (r g b)
      (pick-color-from-screen (widget-connection w)
                              :stop-fn (lambda ()
                                         (and (typep w 'toplevel) (toplevel-closing-p w))))
    (when r
      (set-picker-color w r g b))
    ;; Repaint after the magnifier closes.
    (%picker-redraw-all w)))

;; The picker draws its parts itself as they change; the dirty-flag
;; redraw RUN-WIDGET does after each event has nothing left to do.
(defmethod draw ((w color-picker) surface)
  (declare (ignore surface))
  nil)


;;; Lifecycle

(defun picker-open (w &key (r 0) (g 0) (b 0))
  "Build W's child windows on its WIDGET-WINDOW, paint and map them, and
   add its interactors, starting with the colour R G B."
  (%picker-write-rgb w r g b)
  (%picker-create-windows w)
  (%picker-redraw-all w)                 ; paint every sub-window first
  (%picker-map-windows w)                ; then make them visible
  (unless (slot-value w 'interactors-added)
    (let ((win (widget-window w)))
      (widget-add-interactor w (make-instance 'resize-interactor
                                              :window win :widget w
                                              :on-resize (lambda (w nw nh)
                                                           (declare (ignore nw nh))
                                                           (%picker-resize w))))
      (widget-add-interactor w (make-instance 'picker-input :window win :picker w)))
    (setf (slot-value w 'interactors-added) t))
  w)

(defun picker-close (w)
  "Free W's child windows, TrueType drawables and GC."
  (let ((conn (widget-connection w)))
    (dolist (d (list* (picker-hex-drawable w) (picker-pick-drawable w)
                      (loop for bar in (picker-bars w)
                            append (list (picker-bar-label-drawable bar)
                                         (picker-bar-value-drawable bar)))))
      ;; Free only each wrapper's picture: its window is destroyed below
      ;; with the rest.
      (when d (xcb-truetype:release-drawable d)))
    (dolist (wid (append (list (picker-sv-window w) (picker-sv-indicator w)
                               (picker-hue-window w) (picker-hue-indicator w)
                               (picker-swatch-window w) (picker-hex-window w)
                               (picker-pick-button w))
                         (loop for bar in (picker-bars w)
                               append (list (picker-bar-window bar)
                                            (picker-bar-indicator bar)
                                            (picker-bar-label-window bar)
                                            (picker-bar-value-window bar)))))
      (when wid (ignore-errors (xcb::destroy-window conn wid))))
    (when (picker-draw-gc w) (ignore-errors (xcb::free-gc conn (picker-draw-gc w))))
    (setf (picker-sv-window w) nil (picker-bars w) nil (picker-draw-gc w) nil
          (picker-hex-drawable w) nil (picker-pick-drawable w) nil
          (picker-sv-buffer w) nil (picker-hue-buffer w) nil)
    (setf (widget-interactors w)
          (remove-if (lambda (i)
                       (or (typep i 'picker-input)
                           (and (typep i 'resize-interactor)
                                (eq (resize-widget i) w))))
                     (widget-interactors w)))
    (setf (slot-value w 'interactors-added) nil)))

(defmethod realize :after ((w color-picker) &key &allow-other-keys)
  (picker-open w))

(defmethod unrealize :before ((w color-picker))
  (when (picker-sv-window w)
    (picker-close w)))

;;; color-picker.lisp ends here
