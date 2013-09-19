#!/bin/sh

# File: ihaskellnb.sh
# Authors: Benjamin Jones, Rogan Creswick
# Date: 2013-04-25

#
# Edit these paths to suit your needs
#

IHNB_ROOT=~/src/ihaskell-notebook
GHCJ_BINARY=$IHNB_ROOT/ghcj/dist/dist-sandbox-6f4fed69/build/ghcj/ghcj
IPNB_MAGICS=$IHNB_ROOT/python

#
# Leave everything below this line alone
#

export IHNB_ROOT
export GHCJ_BINARY
export PYTHONPATH=$PYTHONPATH:$IPNB_MAGICS
ipython notebook --ext=ghcjmagic
