;;; quit.lisp — end a widget's event loop on a key or the close button.
;; Copyright (C) 2026  Arthur Miller <arthur.miller@live.com>
;; License: GPLv3+

;;; Commentary:

;; QUIT-INTERACTOR asks for the event loop to end: QUIT-REQUESTED-P
;; becomes true when one of its KEYS is pressed, or, with CLOSE (the
;; default), when the window manager's close button sends a
;; CLOSE-EVENT.  ON-QUIT, if given, is called as (interactor) then.
;; Pass QUIT-REQUESTED-P to RUN-WIDGET's STOP-WHEN:
;;
;;   (let ((quit (make-instance 'quit-interactor :window win)))
;;     (widget-add-interactor w quit)
;;     (run-widget w win :stop-when (lambda () (quit-requested-p quit))))
;;
;; A key is a keysym as EVENT-KEYSYM gives it (#\q, :escape), or a list
;; of modifiers and a keysym, which then quits only with those modifiers
;; held: (:control #\q).  The modifiers are :shift, :lock, :control,
;; :mod1 (:alt) and :mod4 (:super); others held as well do not matter.
;;
;; A subclass that wants a key to mean something else first -- Escape
;; closing a help window before it quits, say -- specializes
;; HANDLE-EVENT on KEY-PRESS and calls REQUEST-QUIT itself.

;;; Code:

(in-package #:xact)

(defclass quit-interactor (interactor)
  ((keys      :initarg :keys :initform '(#\q :escape) :reader quit-keys)
   (close     :initarg :close :initform t
              :documentation "Also quit on the WM's close button.")
   (on-quit   :initarg :on-quit :initform nil)
   (requested :initform nil :accessor quit-requested-p)))

(defparameter *modifier-bits*
  '((:shift   . 1)
    (:lock    . 2)
    (:control . 4)
    (:mod1    . 8)
    (:alt     . 8)
    (:mod4    . 64)
    (:super   . 64)))

(defun key-matches-p (event key)
  "Does the KEY-PRESS EVENT match KEY: a keysym, or (modifier... keysym)?"
  (if (consp key)
      (and (eql (event-keysym event) (car (last key)))
           (every (lambda (m)
                    (let ((bit (cdr (assoc m *modifier-bits*))))
                      (unless bit (error "Unknown modifier ~S in ~S" m key))
                      (logtest bit (event-modifiers event))))
                  (butlast key)))
      (eql (event-keysym event) key)))

(defgeneric request-quit (interactor)
  (:documentation "End the loop: set QUIT-REQUESTED-P, call ON-QUIT.")
  (:method ((i quit-interactor))
    (setf (quit-requested-p i) t)
    (when (slot-value i 'on-quit)
      (funcall (slot-value i 'on-quit) i))
    t))

(defmethod handle-event ((i quit-interactor) (ev key-press))
  (when (some (lambda (key) (key-matches-p ev key)) (quit-keys i))
    (request-quit i)))

(defmethod handle-event ((i quit-interactor) (ev close-event))
  (when (slot-value i 'close)
    (request-quit i)))

;;; quit.lisp ends here
