# xact

*Small interactors for XCB. A diminutive for writing tiny widgets on top of
Garnet's central idea(s) but without Garnet's weight.*

## What this is

xact is a small, self-contained Common Lisp library for building interactive
widgets on top of XCB. It steals the main idea (ideas, really) from Garnet:
**input behavior lives in separate objects called interactors**, not in the
widget that gets drawn.

An interactor is an object that watches a slice of input — a drag, a click,
a key sequence — and mutates some target when it happens. The target might be
a slot on a widget, a constraint cell, or a closure. The interactor doesn't
care. It knows how to recognize an interaction, how to transition through it,
and how to report what happened. What the application does with that is up to
the application.

In a world of interactors and cells, it is relatively simple: interactors write
state, one-way formulas compute derived state, and multi-way relations
(`relate`) tie representations together without cycles.

Everything else just follows. In the color picker, RGB and HSV are tied by a
relation: the screen picker and RGB sliders write RGB, the SV square and hue
strip write HSV, and the relation keeps both in sync. Labels, swatches, and
indicators update themselves reactively. There is no cycle because each write
triggers only the complementary direction.

The whole framework exists to make that separation cheap to express: mixins to
compose behavior, method combination to layer it, a cell and relation protocol
to propagate it, and a single dispatch loop to drive it.


## The shape of it

The three-state machine lives in `continuous-mixin`, not in the base class:

```lisp
(defclass continuous-mixin ()
  ((start-event :initarg :start-event :initform :leftdown)
   (stop-event  :initarg :stop-event  :initform :leftup)
   (abort-event :initarg :abort-event :initform :escape)))

(defmethod handle-event :around ((i continuous-mixin) ev)
  (case (interactor-state i)
    (:idle
     (when (event-matches-p ev (slot-value i 'start-event))
       (unless (hit-test-p i ev)
         (return-from handle-event nil))
       (setf (interactor-state i) :active)
       (start-interaction i ev)
       t))
    (:active
     (cond ((event-matches-p ev (slot-value i 'abort-event))
            (setf (interactor-state i) :idle)
            (abort-interaction i ev)
            t)
           ((event-matches-p ev (slot-value i 'stop-event))
            (setf (interactor-state i) :idle)
            (stop-interaction i ev)
            t)
           ((and (typep ev 'motion-event) (not (hit-test-p i ev)))
            (setf (interactor-state i) :outside)
            (leave-interaction i ev)
            t)
           (t (call-next-method))))
    (:outside
     (cond ((or (event-matches-p ev (slot-value i 'abort-event))
                (event-matches-p ev (slot-value i 'stop-event)))
            (setf (interactor-state i) :idle)
            (abort-interaction i ev)
            t)
           ((and (typep ev 'motion-event) (hit-test-p i ev))
            (setf (interactor-state i) :active)
            (return-interaction i ev)
            t)
           (t (call-next-method))))))
```

Concrete interactors specialize the primary methods on both their own class
and the event class:

```lisp
(defclass pick-button-interactor (continuous-mixin interactor)
  ((hit-test :initarg :hit-test :reader interactor-hit-test)
   (on-pick  :initarg :on-pick  :reader interactor-on-pick)))

(defmethod stop-interaction ((i pick-button-interactor) (ev button-release))
  (when (funcall (interactor-hit-test i) ev)
    (funcall (interactor-on-pick i))))
```

Behavior composes through mixins and method combination. The screen picker,
for example, is a mixin that hooks into the interaction lifecycle:

```lisp
(defclass screen-picker-mixin ()
  ((connection :initarg :connection :reader screen-picker-connection)
   (on-sample  :initarg :on-sample :initform nil)))

(defmethod stop-interaction :after ((i screen-picker-mixin) ev)
  (declare (ignore ev))
  (multiple-value-bind (r g b)
      (pick-color-from-screen (screen-picker-connection i))
    (when (and r (slot-value i 'on-sample))
      (funcall (slot-value i 'on-sample) i r g b))))
```

Any button or click interactor can be given screen-picking by adding one name
to its superclass list. This is why xact is built on CLOS: mixins and method
combinations seem to be a really nice idea.

## What this is not

xact is not a complete GUI toolkit, or even a GUI toolkit at all. If you need a
GUI toolkit, Garnet is a complete UI development environment, native for Common
Lisp: KR for constraints, Opal for retained-mode graphics, Gilt for interface
building. xact is barely one layer of what Garnet does; a small middleware for
helping build GUI elements, rebuilt on CLOS. If you need Garnet's full stack,
use Garnet.

xact is **not a Garnet subset** in any meaning. I just borrow some beautiful
ideas from Garnet. So we are building on top of Garnet conceptually but not
code-wise. In other words, xact has its own class hierarchy, its own event
model, its own state machine. It borrows Garnet's *vocabulary* and its
central insights, not its code. A corollary is that xact widgets are not
re-usable as Garnet widgets, even if you can use them both together if you
really want. If your project outgrows xact, you will have to port by hand.
The two libraries occupy different size niches and are meant to coexist. xact
is useful for small, relatively self-contained widgets without heavy
dependencies. Basically, for pure XCB-based widgets when you don't want to
drag in an entire GUI toolkit.

## Why X11 only

To be honest, I needed something to write demos for clxcb, so this grew as I
went along.  X protocol does not provide any provision for working with "window
procedures" or "event handlers", or anything higher than: "here is an event, do
what you want with it". As I worked with demos, and features added to the
complexity, it was obvious to me I need something more structured. At a certain
point, it just became a PITA to scroll through a long list of labels and
event-loop cases, so I looked for some sort of replacement.  I really like the
principles behind Garnet, so I thought I'll try to hack together something
similar at a smaller scale, mostly as an exercise and a learning project and
usable for these demos.  This little library is the result.  In practice, it is
basically for these demos, and thus X via XCB happens to be the primary target
too.  When it comes to the generality, it contains really, only the stuff I have
used in those demos.

So for now, xact is X11-only on purpose. Not a limitation to apologize for, just
a scope decision.  It would be a project on its own to bring all the required
pieces of the puzzle together to write some of my demos on Wayland (via
[cl-wayflan](https://github.com/ak-coram/wayflan-mirror)) in Common Lisp.

If you need portability across X11 and Wayland, use Garnet — I am working on XCB
and Wayland backends for it, to be published once they are ready. Whether they
land in Garnet itself or live as separate ports, the goal is the same: portable
programs across X and Wayland without changes on the user's side.  If you want a
pure Common Lisp stack, to say.  If you are not as picky, there are of course
bindings to SDL, GTK, Qt and all the other ususal suspect in the field.

## The three layers

Garnet's designers separated their system into three major concerns. Naming
them explicitly clarifies what xact does and does not do:

**Management**

This is the data model and its consistency. In Garnet, the KR subsystem. In
xact, whatever you bring: plain CLOS objects, structs, or a small cell system
if you want reactive updates.

**Acting**

Input is handled here. In Garnet it is done by Interactors. In xact, *plain
theft* — interactors too. Think of "interactive", if it helps.

**Rendering**

This is where a state is turned into pixels. In Garnet, it is in the Opal
subsystem. In xact, whatever you bring: a hand-rolled XCB drawing layer, as
in my case, but it could be a Cairo binding, an OpenGL context, a Skia
binding, or anything else really.

xact is the middle layer. It presupposes a rendering library and leaves
drawing to backend-specific code, supplying the interactive behavior layer
along with a lightweight cell and relation system. That is the whole scope.
I think a modern term for what xact is would be "behaviours".

## Scope

xact is aimed at small widgets: color pickers, font pickers, sliders,
buttons, small selection grids, and similar. It is not aimed at application
shells or anything that wants a full retained-mode scene graph. When a widget
grows past a few hundred lines, Garnet is probably the better tool.

## What is not taken from Garnet

Priority levels: Garnet has them because it hosts many independent interactors
per window and needs ordered claiming. xact's widget-handle-event iterates in
reverse order of addition, and the first interactor to return T claims the
event. If you need more structure, add an explicit priority slot and sort on it.

The :where grammar: Garnet's :start-where accepts a small declarative language
for "any element of this aggregate", "leaf element", etc. xact replaces the
whole thing with hit-test-p, an ordinary generic function. This is the single
largest reduction in scope and it is basically for free.

Accelerators and Global key bindings: Add a key-press handler to the widget and
be done.

Animators and timers: Add them as a mixin when your widget needs them. A timer
is a thread plus a synthetic event.

Demons and update hooks. Garnet's :update-slots and pre/post-set demons exist so
Opal knows when to redraw. In xact, every event handler already marks the widget
dirty. If you need finer granularity, add an after-write hook to (setf cref)
later.

Prototype-based inheritance: We will have to make do with CLOS. I believe
classes are enough for simple widgets.

Type checking on slots: Garnet has an elaborate type system; xact uses
check-type in constructors when needed.
