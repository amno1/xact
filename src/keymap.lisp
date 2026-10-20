;;; keymap.lisp — keymaps, keys and commands, in the manner of Emacs.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;; License: GPLv3+

;;; Commentary:

;; A KEYMAP is a list whose first element is the symbol KEYMAP, followed
;; by key definitions:
;;
;;   (keymap (KEY DEFINITION) (KEY DEFINITION) ...)
;;
;; A KEY is a printable character with or without modifier bits, kept as
;; an integer: the character's code plus any of +CONTROL+, +ALT+, +SHIFT+
;; and +HYPER+, as Emacs does.  MAKE-KEY builds one, KEY-CHAR and
;; KEY-MODIFIERS take it apart, and KBD reads Emacs notation: (kbd "C-x")
;; is one key, (kbd "C-x C-f") a list of two.  A plain character is a key
;; too; DEFINE-KEY and the lookups accept it.  Shift with a character is
;; already in the character (#\R, not S-r), so the Shift bit is only seen
;; on keys whose character Shift does not change, such as S-SPC.
;;
;; A DEFINITION is a command or another keymap.  A command is an
;; interactive function: one whose first form, after its declarations and
;; docstring, is (INTERACTIVE), and whose lambda list is empty.  Common
;; Lisp cannot read the body of a compiled function back, so commands are
;; made with DEFCOMMAND or COMMAND, which check that form and record the
;; function as a command; COMMANDP asks.  A key whose definition is a
;; keymap is a prefix key: after it, the next key is looked up in that
;; keymap, and so on, so keymaps nest as deep as one likes.
;;
;; Each event loop has its keymaps, a list with the one looked up first
;; on top: RUN-WIDGET's :KEYMAPS, by default the widget's WIDGET-KEYMAPS.
;; A key press no interactor takes is looked up in them (KEY-LOOKUP).  The
;; first definition found wins.  If it is a command, it runs, with
;; *WIDGET* bound to the loop's widget and *LAST-KEY* to the key; if it is
;; a prefix key, the next key is looked up in the prefix keymaps of every
;; keymap that has one for it, top first, so a prefix can be shared by
;; several keymaps.  A key undefined after a prefix ends the sequence,
;; and *UNDEFINED-KEY-HOOK* is called with the keys typed; Escape cancels
;; a sequence too.  A key undefined at the top goes on to the loop's
;; ON-OTHER-EVENT, as before.

;;; Code:

(in-package #:xact)

;;; Keys

(defconstant +hyper+   (ash 1 24))
(defconstant +shift+   (ash 1 25))
(defconstant +control+ (ash 1 26))
(defconstant +alt+     (ash 1 27))

(defconstant +modifier-mask+ (logior +hyper+ +shift+ +control+ +alt+))

(defparameter *key-modifier-bits*
  `((:control . ,+control+)
    (:ctrl    . ,+control+)
    (:alt     . ,+alt+)
    (:meta    . ,+alt+)
    (:shift   . ,+shift+)
    (:hyper   . ,+hyper+)
    (:super   . ,+hyper+))
  "Modifier names MAKE-KEY takes, and their bits.")

(defparameter *x-modifier-masks*
  `((#x4 . ,+control+) (#x8 . ,+alt+) (#x1 . ,+shift+) (#x40 . ,+hyper+))
  "X modifier masks (Control, Mod1, Shift, Mod4) and the key bits they
   stand for.  Mod4 represents Hyper (or Super on modern PC keyboards).")

(defun make-key (char &rest modifiers)
  "The key for CHAR, a printable character, with MODIFIERS, each one of
   :CONTROL, :ALT, :SHIFT or :HYPER."
  (check-type char character)
  (logior (char-code char)
          (reduce #'logior modifiers
                  :key (lambda (m)
                         (or (cdr (assoc m *key-modifier-bits*))
                             (error "Unknown modifier ~S" m)))
                  :initial-value 0)))

(defun keyp (object)
  "True if OBJECT is a key: a character, or a character code with
   modifier bits."
  (or (and (characterp object) (graphic-char-p object))
      (and (integerp object) (>= object 0)
           (let ((code (logandc2 object +modifier-mask+)))
             (and (< code char-code-limit) (graphic-char-p (code-char code)))))))

(defun %key (key)
  "KEY as an integer key."
  (cond ((characterp key) (char-code key))
        ((keyp key) key)
        (t (error "~S is not a key: a printable character with or without modifier bits." key))))

(defun key-char (key)
  (code-char (logandc2 (%key key) +modifier-mask+)))

(defun key-modifiers (key)
  "KEY's modifiers, as a list of :CONTROL :ALT :SHIFT :HYPER."
  (let ((key (%key key)))
    (loop for (name . bit) in `((:control . ,+control+) (:alt . ,+alt+)
                                (:shift . ,+shift+) (:hyper . ,+hyper+))
          when (logtest bit key) collect name)))

(defun key-description (keys)
  "KEYS, a key or a list of keys, in Emacs notation: \"C-x C-f\"."
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (key)
                    (let ((c (key-char key)))
                      (format nil "~{~A-~}~A"
                              (mapcar (lambda (m) (ecase m (:control "C") (:alt "M")
                                                    (:shift "S") (:hyper "H")))
                                      (key-modifiers key))
                              (if (char= c #\Space) "SPC" c))))
                  (if (listp keys) keys (list keys)))))

(defun kbd (string)
  "The keys STRING names in Emacs notation, C- M- (or A-) S- H- before a
   character, SPC for space: one key, or a list of them when STRING has
   several, separated by spaces.  (kbd \"C-x\"), (kbd \"C-x C-f\")."
  (let ((keys (loop for word in (remove "" (uiop:split-string string) :test #'string=)
                    collect (let ((mods '()))
                              (loop while (and (> (length word) 2) (char= (char word 1) #\-))
                                    do (push (ecase (char-upcase (char word 0))
                                               (#\C :control) ((#\M #\A) :alt)
                                               (#\S :shift) (#\H :hyper))
                                             mods)
                                       (setf word (subseq word 2)))
                              (apply #'make-key
                                     (cond ((string= word "SPC") #\Space)
                                           ((= (length word) 1) (char word 0))
                                           (t (error "Not a key: ~S" word)))
                                     mods)))))
    (if (rest keys) keys (first keys))))

(defun event-key (event)
  "The key EVENT, a KEY-PRESS, types, or NIL when its keysym is not a
   printable character (an arrow, Escape, a modifier on its own)."
  (let ((ks (event-keysym event)))
    (when (and (characterp ks) (graphic-char-p ks))
      (let ((bits (loop for (mask . bit) in *x-modifier-masks*
                        when (logtest mask (event-modifiers event)) sum bit)))
        ;; Shift is already in the character, unless it changes nothing.
        (when (or (upper-case-p ks) (not (alpha-char-p ks)))
          (unless (char= ks #\Space)
            (setf bits (logandc2 bits +shift+))))
        (logior (char-code ks) bits)))))

;;; Commands

(defvar *commands* (make-hash-table :test 'eq #+sbcl :weakness #+sbcl :key)
  "Function objects made by COMMAND.  Named commands are marked on their
   symbol instead, so a redefinition stays a command.")

(defmacro interactive (&rest spec)
  "Marks the function it begins as a command.  It does nothing when run."
  (declare (ignore spec))
  nil)

(defun %split-body (body)
  "(VALUES DECLARATIONS-AND-DOCSTRING FORMS) of a function BODY."
  (let ((head '()))
    (loop while (or (and (consp (first body)) (eq (car (first body)) 'declare))
                    (and (stringp (first body)) (rest body)))
          do (push (pop body) head))
    (values (nreverse head) body)))

(defun %check-command (name lambda-list body)
  "Check that LAMBDA-LIST and BODY make a command, and return BODY with
   its (INTERACTIVE) form, which may be written in any package, made
   XACT's."
  (unless (null lambda-list)
    (error "Command ~@[~S ~]takes arguments ~S; a command's lambda list is empty."
           name lambda-list))
  (multiple-value-bind (head forms) (%split-body body)
    (unless (and (consp (first forms))
                 (symbolp (car (first forms)))
                 (string= (symbol-name (car (first forms))) "INTERACTIVE"))
      (error "~@[~S: ~]a command's first form, after its declarations, must be (INTERACTIVE)."
             name))
    (append head (list (cons 'interactive (cdr (first forms)))) (rest forms))))

(defmacro defcommand (name lambda-list &body body)
  "DEFUN a command: LAMBDA-LIST must be (), and the first form of BODY,
   after its declarations and docstring, (INTERACTIVE)."
  (setf body (%check-command name lambda-list body))
  `(progn
     (defun ,name () ,@body)
     (setf (get ',name 'command) t)
     ',name))

(defmacro command (lambda-list &body body)
  "A LAMBDA that is a command; see DEFCOMMAND."
  (setf body (%check-command nil lambda-list body))
  `(let ((f (lambda () ,@body)))
     (setf (gethash f *commands*) t)
     f))

(defun commandp (object)
  "True if OBJECT is a command: a function made by COMMAND, or a symbol
   DEFCOMMAND defined."
  (cond ((symbolp object) (and object (get object 'command) (fboundp object) t))
        ((functionp object) (and (gethash object *commands*) t))
        (t nil)))

;;; Keymaps

(defun make-keymap ()
  "A new, empty keymap."
  (list 'keymap))

(defun keymapp (object)
  "True if OBJECT is a keymap: a list whose first element is KEYMAP."
  (and (consp object) (eq (car object) 'keymap)))

(defun %keys (key)
  "KEY, a key or a list of keys, as a list of integer keys."
  (mapcar #'%key (if (listp key) key (list key))))

(defun %binding (keymap key)
  (assoc key (rest keymap)))

(defun define-key (keymap key command)
  "Make COMMAND the definition of KEY in KEYMAP and return it.  KEY is a
   key, or a list of keys: then each but the last is a prefix key, and
   prefix keymaps are made where KEYMAP has none.  COMMAND is a command,
   a keymap (making KEY a prefix key), or NIL to remove the definition."
  (unless (keymapp keymap) (error "~S is not a keymap." keymap))
  (unless (or (null command) (commandp command) (keymapp command))
    (error "~S is neither a command nor a keymap." command))
  (let ((keys (%keys key)))
    (loop for k in (butlast keys)
          for def = (second (%binding keymap k))
          do (setf keymap
                   (cond ((keymapp def) def)
                         (t (let ((sub (make-keymap)))
                              (define-key keymap k sub)
                              sub)))))
    (let* ((k (car (last keys)))
           (binding (%binding keymap k)))
      (cond ((null command)
             (setf (cdr keymap) (remove binding (cdr keymap))))
            (binding (setf (second binding) command))
            (t (push (list k command) (cdr keymap)))))
    command))

(defvar *keymaps* '()
  "The keymaps of the event loop running now, top first.")

(defvar *widget* nil
  "The widget whose event loop runs the current command.")

(defvar *last-key* nil
  "The key that ran the current command.")

(defvar *undefined-key-hook* nil
  "Called with the list of keys typed when a key sequence turns out
   undefined after a prefix key; NIL for nothing.")

(defun %lookup-1 (keymap key)
  (second (%binding keymap key)))

(defun %lookup (keymap keys)
  "KEYS' definition in KEYMAP, following prefix keys, or NIL."
  (loop for (k . more) on keys
        for def = (%lookup-1 keymap k)
        do (cond ((null more) (return def))
                 ((keymapp def) (setf keymap def))
                 (t (return nil)))))

(defun key-lookup (key &optional (keymaps *keymaps*))
  "All the definitions of KEY (a key, or a list of keys following prefix
   keys) in KEYMAPS, a keymap or a list of them, top first.  The first is
   the one an event loop would use."
  (let ((keys (%keys key)))
    (loop for map in (if (keymapp keymaps) (list keymaps) keymaps)
          for def = (%lookup map keys)
          when def collect def)))

(defun keymap-lookup (key &optional (keymaps *keymaps*))
  "The keymaps among KEYMAPS, a keymap or a list of them, that define KEY
   (a key or a list of keys), top first."
  (let ((keys (%keys key)))
    (loop for map in (if (keymapp keymaps) (list keymaps) keymaps)
          when (%lookup map keys) collect map)))

;;; The event loop's side

(defvar *pending-keymaps* nil
  "While a key sequence is being typed, the prefix keymaps its next key
   is looked up in; NIL otherwise.  Bound per event loop.")

(defvar *pending-keys* '()
  "The keys of the sequence typed so far, newest first.")

(defun keymap-dispatch (widget event)
  "Look the key EVENT types up in the loop's keymaps, or in the pending
   prefix keymaps, and act on it.  Returns true if the keymaps took it."
  (let ((key (event-key event)))
    (cond
      ((null key)
       ;; Escape cancels a sequence; other keys without a character
       ;; (arrows, a modifier pressed alone) leave it pending.
       (when (and *pending-keymaps* (eq (event-keysym event) :escape))
         (setf *pending-keymaps* nil *pending-keys* '())
         t))
      (t
       (let* ((maps (or *pending-keymaps* *keymaps*))
              (defs (key-lookup key maps))
              (def  (first defs)))
         (push key *pending-keys*)
         (cond
           ((keymapp def)
            (setf *pending-keymaps* (remove-if-not #'keymapp defs))
            t)
           ((commandp def)
            (setf *pending-keymaps* nil)
            (let ((*widget* widget)
                  (*last-key* key))
              (setf *pending-keys* '())
              (funcall def))
            t)
           (*pending-keymaps*
            (let ((keys (reverse *pending-keys*)))
              (setf *pending-keymaps* nil *pending-keys* '())
              (when *undefined-key-hook*
                (funcall *undefined-key-hook* keys)))
            t)
           (t
            (setf *pending-keys* '())
            nil)))))))

;;; keymap.lisp ends here
