# Ball-Z

Game with balls. Quite a few of them.

[Autumn 2016 Lisp Game Jam](https://itch.io/jam/autumn-2016-lisp-game-jam) entry.


## Installation

### macOS
If you happened to be using macOS, it is possible to download blob and follow instructions
at itch.io [page](https://borodust.itch.io/ball-z)


### From sources
The engine this game is based upon is in its very early stages of development. See instructions
on how to set it up at the respective [page](https://github.com/borodust/cl-bodge).

Ensure `ball-z` system can be found by quicklisp too by cloning repo to
`quicklisp/local-projects/` (`~/quicklisp/local-projects/` is the default path),
symlinking project repository there or any other hackish means you like.

When both `cl-bodge` and `ball-z` can be found by your local quicklisp configuration,
you should be able to run the game by loading it with quicklisp and calling start
function with path to game configuration file:
```lisp
(ql:quickload :ball-z)
(bz:start "/path/to/ball-z.conf") ; substitute string for the path of the `ball-z.conf`
                                  ; that could be found in the source tree
```