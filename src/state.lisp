;; state.lisp
;; Copyright (C) 2026  Arthur Miller

;; A small constraint engine. Cells hold values or formulas; reads inside a
;; formula record a dependency; writes invalidate dependents, which are
;; recomputed lazily on next read.

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

;; Cells can also be tied by multi-way relations (RELATE, at the end).
;;
;; Below is the whole engine. No demons, no priority queue, no circularity sweep
;; marks, no prototype inheritance, no type system. Every one of those is a
;; Garnet feature that exists because KR wanted to be a general-purpose
;; constraint language on 1990s hardware.

(in-package #:xact)

(defstruct (cell (:constructor %make-cell))
  (name       nil)
  (value      nil)
  (formula    nil)  ; a thunk, or NIL for plain cells
  (deps       '())  ; cells this cell's formula read last time
  (dependents '())  ; cells whose formulas read this cell
  (valid        t)
  (busy      nil)
  (stamp       0)   ; when the cell was last written by SETF CREF, 0 for never
  (relations '()))  ; the relations (see RELATE) this cell belongs to

(defclass state ()
  ((cells :initform (make-hash-table :test 'eq)
          :reader state-cells)
   (clock :initform 0 :accessor state-clock
          :documentation "Counts writes, to stamp cells for relations.")))

(defvar *current-formula* nil
  "Bound to the cell being computed during formula evaluation.
   Reads of other cells within this dynamic extent record a dependency.")

(defvar *solving* '()
  "The relations already run for the write being propagated.")

(defun cell (state name)
  (or (gethash name (state-cells state))
      (setf (gethash name (state-cells state)) (%make-cell :name name))))

;; State discipline: One-way formulas compute derived state from canonical
;; state without cycles. Where state has multiple equally valid representations
;; (such as RGB and HSV in a colour picker), multi-way relations (RELATE) tie
;; plain writable cells together so editing any representation updates the
;; others without cycles.
;; The framework provides cref, install-formula and relate.

(defun cref (state name)
  "Read the value of a cell. If a formula is currently being evaluated,
   record a dependency from that formula to this cell."
  (let ((c (cell state name)))
    (unless (cell-valid c)
      (recompute state c))
    (when (and *current-formula* (not (eq *current-formula* c)))
      (pushnew c (cell-deps *current-formula*))
      (pushnew *current-formula* (cell-dependents c)))
    (cell-value c)))

(defun (setf cref) (value state name)
  "Write a cell.

 If the value changes, invalidate all transitive dependents."
  (let ((c (cell state name)))
    (when (cell-formula c)
      (error "Cannot write to formula cell ~S" name))
    (setf (cell-stamp c) (incf (state-clock state)))
    (unless (equal (cell-value c) value)
      (setf (cell-value c) value)
      (invalidate-dependents c))
    (let ((*solving* *solving*))
      (dolist (r (cell-relations c))
        (unless (member r *solving*)
          (solve-relation state r c))))
    value))

(defun install-formula (state name thunk)
  "Install THUNK as the formula for cell NAME.

 The cell is marked invalid and will be recomputed lazily on the next read."
  (let ((c (cell state name)))
    (setf (cell-formula c) thunk
          (cell-valid c) nil)
    (invalidate-dependents c)
    c))

(defun recompute (state c)
  (declare (ignore state))
  (when (cell-busy c)
    (error "Circular constraint detected while evaluating ~S" c))
  (setf (cell-busy c) t)
  (unwind-protect
       (let ((*current-formula* c)
             (old-deps (cell-deps c)))
         ;; Detach from the dependencies recorded during the previous
         ;; evaluation. They will be re-recorded by reads inside the thunk.
         (dolist (d old-deps)
           (setf (cell-dependents d)
                 (remove c (cell-dependents d))))
         (setf (cell-deps c) '())
         ;; Evaluate.  The cell becomes valid only when the formula
         ;; returns: if it signals, the cell stays invalid and the next
         ;; read tries again, rather than serving the stale value.
         (let ((new (funcall (cell-formula c))))
           (unless (equal (cell-value c) new)
             (setf (cell-value c) new))
           (setf (cell-valid c) t)))
    (setf (cell-busy c) nil)))

(defun invalidate-dependents (c)
  (dolist (d (cell-dependents c))
    (when (cell-valid d)
      (setf (cell-valid d) nil)
      (invalidate-dependents d))))

(defmacro defstate (name &body slots)
  "Define a constructor MAKE-<NAME> returning a fresh state.  Each slot
   is (:key value) for a plain cell, which MAKE-<NAME> takes as a keyword
   argument defaulting to VALUE, or (:key (:formula expr)) for a derived
   one.  Inside a formula, SELF refers to the state.

     (defstate color (:r 0) (:g 0) (:b 0)
       (:gray (:formula (/ (+ (cref self :r) (cref self :g) (cref self :b)) 3))))
     (make-color :r 255)"
  (let* ((package (symbol-package name))
         (make-name (intern (concatenate 'string "MAKE-" (symbol-name name)) package))
         ;; SELF in the package the formulas were read in, which is the
         ;; package NAME was read in.
         (self (intern "SELF" package))
         (plain (remove-if (lambda (slot)
                             (let ((value (second slot)))
                               (and (consp value) (eq (car value) :formula))))
                           slots))
         (vars (mapcar (lambda (slot) (make-symbol (symbol-name (first slot)))) plain)))
    `(defun ,make-name (&key ,@(loop for (key value) in plain
                                     for var in vars
                                     collect `((,key ,var) ,value)))
       (let ((,self (make-instance 'state)))
         ,@(loop for (key value) in slots
                 collect (if (and (consp value) (eq (car value) :formula))
                             `(install-formula ,self ,key
                                               (lambda () ,(second value)))
                             `(setf (cref ,self ,key) ,(nth (position key plain :key #'first)
                                                            vars))))
         ,self))))

(defmacro with-crefs (state (&rest bindings) &body body)
  "Bind local variables to cell values for the duration of BODY.
   Each binding is (variable cell-name):

     (with-crefs s ((r :r) (g :g))
       (+ r g))"
  (let ((s (gensym "STATE")))
    `(let* ((,s ,state)
            ,@(loop for (var name) in bindings
                    collect `(,var (cref ,s ,name))))
       ,@body)))

;;; Relations: multi-way constraints, after Adobe's Adam
;;
;; A formula computes one cell from others, in one direction only.  A
;; relation ties a group of plain cells together with several METHODS,
;; each computing some of the cells (its outputs) from the others (its
;; inputs):
;;
;;   (relate st '(:r :g :b) '(:h :s :v) #'rgb->hsv-values
;;              '(:h :s :v) '(:r :g :b) #'hsv->rgb-values)
;;
;; All the cells stay writable.  When one is written, each relation it is
;; in runs a method that has it as an input -- never one that would
;; overwrite it -- and among those, the method whose outputs were written
;; longest ago, so what the user set most recently is kept; between
;; equally old outputs, the method given first.  Values set before RELATE
;; count as never written.  The method's
;; function gets the input values and returns the output values; it may
;; read other cells, the current outputs among them, with CREF.  Outputs
;; are stored without counting as writes, and formulas that read them
;; are invalidated as usual.  An output that is in other relations makes
;; those run too, each at most once per write, so relations cannot loop.

(defstruct (relation (:constructor %make-relation (methods)))
  methods)  ; list of (inputs outputs function)

(defun relate (state &rest methods)
  "Tie cells of STATE together by METHODS, given flat as INPUTS OUTPUTS
   FUNCTION, ...: lists of cell names and a function from the inputs'
   values to the outputs' values (as multiple values).  Returns the
   relation.  The cells must be plain cells, not formulas."
  (let ((r (%make-relation
            (loop for (inputs outputs fn) on methods by #'cdddr
                  collect (list inputs outputs fn)))))
    (dolist (name (remove-duplicates
                   (loop for (inputs outputs) in (relation-methods r)
                         append (append inputs outputs))))
      (let ((c (cell state name)))
        (when (cell-formula c)
          (error "~S is a formula cell; a relation ties plain cells." name))
        ;; Values set before the relation are its starting point, not
        ;; edits: no method is preferred because of them.
        (setf (cell-stamp c) 0)
        (push r (cell-relations c))))
    r))

(defun %relation-store (state name value)
  "Set cell NAME to VALUE for a relation: not a write by the user, so no
   stamp, but dependents and other relations follow."
  (let ((c (cell state name)))
    (unless (equal (cell-value c) value)
      (setf (cell-value c) value)
      (invalidate-dependents c)
      (dolist (r (cell-relations c))
        (unless (member r *solving*)
          (solve-relation state r c))))))

(defun solve-relation (state relation written)
  "Run RELATION's method for a change of the cell WRITTEN."
  (let* ((written-name (cell-name written))
         (stamp (lambda (name) (cell-stamp (cell state name))))
         (candidates (remove-if-not (lambda (m)
                                      (and (member written-name (first m))
                                           (not (member written-name (second m)))))
                                    (relation-methods relation)))
         (method (first (stable-sort (copy-list candidates) #'<
                                     :key (lambda (m)
                                            (reduce #'max (second m) :key stamp
                                                                     :initial-value 0))))))
    (when method
      (destructuring-bind (inputs outputs fn) method
        (push relation *solving*)
        (let ((values (multiple-value-list
                       (apply fn (mapcar (lambda (n) (cref state n)) inputs)))))
          (loop for name in outputs
                for v in values
                do (%relation-store state name v)))))))

