ihaskell-notebook
=================

The goal of the project is to enable evaluation of Haskell code in the IPython
notebook and support rich display of output.

`ihaskell-notebook` is implemented as an IPython extention (a "cell
magic") for interacting with a persistent
[ghci](http://www.haskell.org/haskellwiki/GHC/GHCi) session inside
the IPython notebook.

Requirements
------------

You will need:

 * IPython (tested with v0.13)
 * GHCi

The easiest way to get GHCi (and GHC) is to install the [Haskell
Platform](http://www.haskell.org/platform/).

Usage
-----

Checkout this ihaskell-notebook repository and start an ipython notebook
instance in its directory (which should contain ghcimagic.py).

In the first cell, evaluate:

    %load_ext ghcimagic

From this point on, all subsequent cell evaluations are run through the ghci
cell magic. There is no need to prefix the cells with `%%ghci` as you would for
a normal IPython cell magic.
