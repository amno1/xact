# xact design notes

## Where xact is now

xact is a small library for interactive widgets on XCB. It takes
Garnet's central idea: input behaviour lives in **interactors**,
separate from the widgets that are drawn. The parts:

**Widgets** (`widget.lisp`). A widget has a connection and a window, a size, a
`state`, a list of interactors and a list of keymaps. A `toplevel`
(`backend/toplevel.lisp`) is a widget that owns a window and knows its kind:

  - `:managed` and `:undecorated` windows are handled by the window manager;
  - an `:override-redirect` window is one the window manager never sees;
  - a `:root` toplevel draws on a window it doesn't own, such as the root.

  `realize`, `show` and `unrealize` cover a toplevel's life.  `request-close`
  asks it to close from any thread: it sets a flag and wakes the loop with a
  WM_DELETE_WINDOW message.

**Interactors** (`interactor.lisp`, `interactors/`). An interactor is an object
that watches a slice of input. `continuous-mixin` holds the
start/running/stop/abort state machine. On top of it sit the drag, window move
and resize, quit and resize-follow interactors. The keyboard interactors for
moving and resizing are modal: while active, they take every key.

**The event loop** (`backend/xcb.lisp`, `run-widget`). It waits for events,
translates them into xact events, and offers each event to the widget's
interactors, newest first. A key press that no interactor takes goes to the
widget's keymaps. The loop runs ticks and redraws a dirty widget.

**Keymaps** (`keymap.lisp`). These work as in Emacs: keys are characters with
modifier bits, and a command is an `(interactive)` function. A prefix key leads
to another keymap, so keymaps nest. Each loop has its own list of keymaps, and
the first one is looked up first. It was a bit unnecessary experiment, in
retrospect.

**State** (`state.lisp`). This is a cell protocol, as in Garnet's KR. A cell
holds either a plain value or a one-way formula. Formulas record the cells they
read and are recomputed lazily when one of those cells changes. A formula cell
cannot be written.

**Widgets built on all this:** `slider`, the screen colour sampler
(`screen-picker`, `pick-color-from-screen`) and `color-picker` (the separate
system `xact/color-picker`).

The rule the README states is: *interactors write canonical state, formulas
write derived state, and nothing else writes anything.*

## What the colour picker taught us

Before the relations below, the colour picker kept its colour in cells.  `:r :g
:b` were canonical, and `:h :s :v` were formulas over them. Every input writes
RGB, so there are no cycles.

But one-way formulas force us to pick a single direction, and here neither
direction is lossless. A grey has no hue and black has no saturation. If HSV is
derived from RGB, the hue strip jumps to red as soon as the square is dragged to
its grey edge. The picker worked around this with two extra canonical cells,
`:hue-hint` and `:sat-hint`, which remember what the user last asked for.

The real problem is that the colour has two equally valid representations. The
user edits one or the other, depending on which control they touch. The hints
were a symptom of encoding a two-way relation with one-way tools.

## Influences

**Garnet KR**: one-way formulas with dynamic dependency tracking.  xact's
`state.lisp` is this model.

**Adobe's Adam & Eve** (Sean Parent and others, in the Adobe Source Libraries).
Adam is a property model of **multi-way dataflow constraints**: a `relate`
clause gives several methods, each computing some cells of a group from the
others.  On each change the solver picks the method whose outputs are the cells
the user edited least recently.  Evaluation is then ordinary one-way dataflow.
Adam knows nothing about geometry. Eve is its layout half: nested containers
(rows, columns) with alignment guides, laid out in a measure pass and a place
pass. It is not a general linear solver.

Both keep state logic separate from presentation. Adam adds multi-directional
relations without giving up predictable evaluation.

## How xact models these

**Multi-way relations** are in `state.lisp`, after Adam. *(Done: `relate`.)* A
relation ties a group of plain cells together with methods. Each method has its
inputs, its outputs and a function. When a cell in a relation is written, the
relation runs a method that keeps the written cell as an input. If several
methods qualify, it picks the one whose outputs were written least
recently. Outputs are stored without counting as user writes, and formulas that
read them are invalidated as usual.  A relation never writes the cell the user
just wrote, so there is no loop.

The colour picker moves onto it. `:r :g :b` and `:h :s :v` all become writable
cells, joined by an RGB→HSV method and an HSV→RGB method.  The square and the
strip write HSV, and the RGB sliders and the screen sampler write RGB.  The
RGB→HSV method keeps the current hue or saturation where RGB leaves it, which
makes the hint cells unnecessary.

One-way formulas stay as they are, for derived values that are never edited.

When `relate` is called, the values set before it count as never written, so
they don't steer the first choice of method. When two methods' outputs are
equally old, the method given first wins.

**An Eve-like box layout** is done in `layout.lisp`. A layout is a tree of
`box` leaves inside `row` and `column` containers, which have margins and
spacing. A box has a minimum size and stretch factors.  Each child of a
container is aligned across the container's axis (`:fill`, `:start`, `:center`
or `:end`). Stretch passes up the tree, so a container stretches as much as its
stretchiest child.

   The layout is solved in two passes, measure then place, with no
   simplex solver.  Each box's `place` function gets the box's rectangle,
   for example to configure a child window.  A widget's `widget-layout`
   is laid out again by the resize interactor whenever the window
   changes size.

   The colour picker uses it. One layout tree replaces
   `%picker-compute-layout` and the geometry half of `%picker-resize`.
   Its hue strip, swatch and Pick button now have fixed widths, where
   the old code scaled them with the window, within limits.

   Not done, compared with Eve: alignment guides across containers (for
   example, lining up the labels of different rows), and widgets having
   their own parent/children slots. The layout tree is separate from the
   widgets for now.

**Not planned:** Cassowary-style linear solving, spatial grids, Morton-ordered
flat geometry arrays or SIMD culling.  They pay off at thousands of elements, and
xact windows have tens. Hit-testing is already done by the X server through
child windows, and the costs that matter are round trips and image uploads.
