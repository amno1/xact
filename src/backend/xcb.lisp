;;; xcb.lisp — XCB backend for xact.
;;; License: GPLv3+

(in-package #:xact)

(defvar *keysym-caches* (make-hash-table :test 'eq)
  "Connection → plist :MIN, :MAX, :PER (keysyms per keycode), :KEYSYMS
   (vector).  One per connection: two displays need not share a
   keyboard mapping.")

(defun invalidate-keysym-cache (conn)
  "Forget CONN's keyboard mapping (after a MappingNotify)."
  (remhash conn *keysym-caches*))

(defun ensure-keysym-cache (conn)
  (or (gethash conn *keysym-caches*)
      (let* ((min    (xcb:conn-min-keycode conn))
             (max    (xcb:conn-max-keycode conn))
             (count  (1+ (- max min)))
             (cookie (xcb::get-keyboard-mapping conn min count))
             (reply  (xcb::get-keyboard-mapping-reply conn cookie)))
        (setf (gethash conn *keysym-caches*)
              (list :min     min
                    :max     max
                    :per     (slot-value reply 'xcb::keysyms-per-keycode)
                    :keysyms (slot-value reply 'xcb::keysyms))))))

(defun xcb-keysym-from-code (conn xcb-event &key (column 0))
  (let* ((cache (ensure-keysym-cache conn))
         (min   (getf cache :min))
         (max   (getf cache :max))
         (per   (getf cache :per))
         (ks    (getf cache :keysyms))
         (code  (slot-value xcb-event 'xcb::detail)))
    (when (<= min code max)
      (let ((idx (+ (* (- code min) per) column)))
        (when (< idx (length ks))
          (elt ks idx))))))

(defun xcb-event-keysym (conn xcb-event)
  "The numeric X keysym of the XCB key event XCB-EVENT on CONN, shifted
   when Shift or Lock is held and the key has a shifted keysym: Shift+/
   is ?, not /; with NumLock on, a keypad key gives its digit.  For code
   that handles raw XCB events; xact's own
   KEY-PRESS events carry it, translated, as EVENT-KEYSYM."
  (let* ((state   (slot-value xcb-event 'xcb::state))
         (second  (xcb-keysym-from-code conn xcb-event :column 1))
         (keypad  (and second (<= #xFF80 second #xFFBD))))
    (cond
      ;; NumLock (Mod2) and a keypad key: its second keysym, the digit or
      ;; sign, unless Shift asks for the other -- as Xlib does.
      ((and keypad (logtest state #x10))
       (if (logtest state #b1)
           (xcb-keysym-from-code conn xcb-event)
           second))
      ((and (logtest state #b11) second (plusp second)) ; Shift, Lock
       second)
      (t (xcb-keysym-from-code conn xcb-event)))))

(defun keysym->key (ks)
  "Translate a numeric X11 keysym to a Lisp-friendly key.  Printable
   ASCII becomes a character; special keys become keywords; anything
   else is returned unchanged so the caller can still inspect it."
  (cond
    ((null ks) nil)
    ((<= 32 ks 126) (code-char ks))
    ;; The keypad, with NumLock on, types what the main keys do.
    ((<= #xFFB0 ks #xFFB9) (code-char (+ (char-code #\0) (- ks #xFFB0))))
    ((= ks #xFFAB) #\+)
    ((= ks #xFFAD) #\-)
    ((= ks #xFFAA) #\*)
    ((= ks #xFFAF) #\/)
    ((= ks #xFFAE) #\.)
    ((= ks #xFFAC) #\,)
    ((= ks #xFFBD) #\=)
    ((= ks #xFF8D) :return)
    ((= ks #xFF08) :backspace)
    ((= ks #xFF09) :tab)
    ((= ks #xFF0D) :return)
    ((= ks #xFF1B) :escape)
    ((= ks #xFF50) :home)
    ((= ks #xFF51) :left)
    ((= ks #xFF52) :up)
    ((= ks #xFF53) :right)
    ((= ks #xFF54) :down)
    ((= ks #xFF55) :page-up)
    ((= ks #xFF56) :page-down)
    ((= ks #xFF57) :end)
    ((= ks #xFFFF) :delete)
    (t ks)))

(defun xcb-event->xact (conn xcb-event)
  "Translate an XCB event struct to a xact event object, or NIL."
  (flet ((field (slot) (slot-value xcb-event slot)))
    (typecase xcb-event
      (xcb::motion-notify
       (make-instance 'motion-event
                      :window (field 'xcb::event)
                      :x (field 'xcb::event-x) :y (field 'xcb::event-y)
                      :root-x (field 'xcb::root-x) :root-y (field 'xcb::root-y)))
      ((or xcb::button-press xcb::button-release)
       (make-instance (if (typep xcb-event 'xcb::button-press) 'button-press 'button-release)
                      :window (field 'xcb::event)
                      :x (field 'xcb::event-x) :y (field 'xcb::event-y)
                      :root-x (field 'xcb::root-x) :root-y (field 'xcb::root-y)
                      :button (field 'xcb::detail)
                      :modifiers (field 'xcb::state)))
      ((or xcb::key-press xcb::key-release)
       (make-instance (if (typep xcb-event 'xcb::key-press) 'key-press 'key-release)
                      :window (field 'xcb::event)
                      :keysym (keysym->key (xcb-event-keysym conn xcb-event))
                      :keycode (field 'xcb::detail)
                      :modifiers (field 'xcb::state)))
      ((or xcb::enter-notify xcb::leave-notify)
       (make-instance (if (typep xcb-event 'xcb::enter-notify) 'enter-event 'leave-event)
                      :window (field 'xcb::event)
                      :x (field 'xcb::event-x) :y (field 'xcb::event-y)
                      :mode (field 'xcb::mode)))
      (xcb::expose
       (make-instance 'expose-event
                      :window (field 'xcb::window)
                      :x (field 'xcb::x) :y (field 'xcb::y)
                      :width (field 'xcb::width) :height (field 'xcb::height)))
      (xcb::configure-notify
       (make-instance 'configure-event
                      :window (field 'xcb::window)
                      :x (field 'xcb::x) :y (field 'xcb::y)
                      :width (field 'xcb::width) :height (field 'xcb::height)))
      (xcb::create-notify  (make-instance 'create-event  :window (field 'xcb::window)))
      (xcb::reparent-notify
       (make-instance 'reparent-event :window (field 'xcb::window) :parent (field 'xcb::parent)))
      (xcb::map-notify     (make-instance 'map-event     :window (field 'xcb::window)))
      (xcb::unmap-notify   (make-instance 'unmap-event   :window (field 'xcb::window)))
      (xcb::destroy-notify (make-instance 'destroy-event :window (field 'xcb::window)))
      (xcb::client-message
     (when (xcb:wm-delete-window-message-p conn xcb-event)
       (make-instance 'close-event)))
    (t nil))))

(defun %dispatch-xcb-event (widget conn xcb-event on-other-event)
  (cond
    ((typep xcb-event 'xcb:expose)
     ;; Redraw, and let interactors and ON-OTHER-EVENT see it as well:
     ;; a widget drawing on several windows, or watching the root, needs
     ;; to know which window was exposed.
     (setf (widget-dirty-p widget) t)
     (let ((xact-event (xcb-event->xact conn xcb-event)))
       (unless (widget-handle-event widget xact-event)
         (when on-other-event
           (funcall on-other-event xcb-event)))))
    ((typep xcb-event 'xcb:mapping-notify)
     (invalidate-keysym-cache conn))
    (t
     (let ((xact-event (xcb-event->xact conn xcb-event)))
       (unless (and xact-event
                    (or (widget-handle-event widget xact-event)
                        ;; A key no interactor took goes to the keymaps.
                        (and (typep xact-event 'key-press)
                             (keymap-dispatch widget xact-event))))
         (when on-other-event
           (funcall on-other-event xcb-event)))))))

(defun run-widget (widget surface
                   &key (stop-when (lambda () nil))
                        (on-other-event nil)
                        (idle-sleep 0.02)
                        (keymaps (widget-keymaps widget)))
  "Run WIDGET's event loop until STOP-WHEN returns true.

   Waits for events (it does not poll), handles every queued event, then
   redraws the widget into SURFACE once if any of them made it dirty.
   Every IDLE-SLEEP seconds, events or not, each interactor's HANDLE-TICK
   runs; this is also how often STOP-WHEN is checked while nothing
   happens.  KEYMAPS, top first, are where keys no interactor takes are
   looked up (see keymap.lisp); a nested loop has its own, and its own
   pending key sequence.  A TOPLEVEL's loop also ends once TOPLEVEL-CLOSING-P is
   true (see REQUEST-CLOSE).  ON-OTHER-EVENT is called with each XCB event the widget did
   not handle, including ones xact has no event class for."
  (let* ((*keymaps* keymaps)
         (*pending-keymaps* nil)
         (*pending-keys* '())
         (conn (widget-connection widget))
         (tick (round (* idle-sleep internal-time-units-per-second)))
         (next-tick (+ (get-internal-real-time) tick)))
    (loop until (or (funcall stop-when)
                    (and (typep widget 'toplevel) (toplevel-closing-p widget)))
          do (let ((now (get-internal-real-time)))
               ;; Sleep until an event arrives or the next tick is due.
               (when (< now next-tick)
                 (xcb:wait-for-x-event-or-timeout
                  conn (/ (- next-tick now) internal-time-units-per-second))))
             (loop for xcb-event = (xcb:poll-for-event conn)
                   while xcb-event
                   do (%dispatch-xcb-event widget conn xcb-event on-other-event))
             (when (>= (get-internal-real-time) next-tick)
               (widget-tick widget)
               (setf next-tick (+ (get-internal-real-time) tick)))
             (when (widget-dirty-p widget)
               (widget-redraw widget surface))
             (xcb:flush conn))))
