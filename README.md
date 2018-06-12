## pixel-spirit-deck

Un-official translation of Patricio Gonzalez's [PixelSpiritDeck](https://github.com/patriciogonzalezvivo/PixelSpiritDeck) into cepl/varjo/lisp.

### How to use directly

Install [quicklisp](https://www.quicklisp.org/beta/)
Put this repo on `~/.quicklisp/local-projects/`
On SLIME repl:
```
(ql:quickload :pixel-spirit-deck)
(in-package :pixel-spirit-deck)
```
On emacs, open and evaluate the `pixel-spirit-deck.lisp` file with C-c C-k
On SLIME repl, `(play :start)`
Open `cards.lisp` and evaluate each card function.

### Why?
To practice lisp, and glsl. To have fun. To have a library of the functions provided there a quickload away.

### TODO
* ~~Shader lisp macro (aka defmacro) to add the boilerplate of each card (st mangling) implicitly.~~
* Have a way to iterate through each fragment.
* ~~Add time~~
* ~~Export functions~~
