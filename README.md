ihaskell-notebook
=================

The goal of the project is to enable evaluation of Haskell code in the IPython
notebook and support rich display of output.

`ihaskell-notebook` is implemented as an IPython extention (a "cell
magic") which connects to a Haskell application we're calling `ghcj`.
This application use the GHC API directly to handle compilations, imports,
and evaluation.

Requirements
------------

You will need:

 * IPython (tested with v0.13)

   See the install instructions [here](http://ipython.org/install.html)

 * GHC

   The easiest way to get GHC is to install the [Haskell
   Platform](http://www.haskell.org/platform/).

Usage
-----

 * Checkout this ihaskell-notebook repository
 * chdir to `ghcj` and build, e.g.

    $ cd ghcj
    $ cabal-dev install

 * modify the top-level script ihaskellnb.sh to reflect the location of `ghcj`
   binary

    GHCJ_BINARY = "/home/user/ihaskell-notebook/ghcj/dist/ghcj/ghcj"

 * run `ihaskellnb.sh` in whatever directory you want to use for notebooks

Once in the notebook, create (or load, or upload) a notebook. In
the first cell, evaluate:

    %load_ext ghcjmagic

From this point on, all subsequent cell evaluations are run through the ghcj
cell magic. There is no need to prefix the cells with `%%ghcj` as you would for
a normal IPython cell magic.
