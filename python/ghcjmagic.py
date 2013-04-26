# -*- coding: utf-8 -*-
"""
===========
ghcjmagic
===========

Magics for interacting with ghcj via ghcj2py
"""

#-----------------------------------------------------------------------------
#  Copyright (C) 2013 Rogan Creswick and Benjamin Jones
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

import ghcj2py

from IPython.core.displaypub import publish_display_data
from IPython.core.magic import (Magics, magics_class, cell_magic)
from IPython.testing.skipdoctest import skip_doctest
from IPython.core.magic_arguments import (argument, magic_arguments,
                                          parse_argstring)


@magics_class
class GhcjMagics(Magics):
    """A set of magics useful for interactive work with Octave via oct2py.
    """
    def __init__(self, shell):
        """
        Parameters
        ----------
        shell : IPython shell

        """
        super(GhcjMagics, self).__init__(shell)
        self._ghcj = ghcj2py.Ghcj2Py()
        self._publish_display_data = publish_display_data

    @skip_doctest
    @magic_arguments()
    @argument(
        '-v', '--verbose', action='store_true',
        help='turn verbosity up')
    @cell_magic
    def ghcj(self, line, cell=None, local_ns=None):
        '''
        Execute code in ghcj and return results including errors if there are
        any.
        '''

        args = parse_argstring(self.ghcj, line)

        if cell is None:
            code = ''
        else:
            code = cell

        try:
            text_output = self._ghcj.run(code, verbose=args.verbose)
        except (ghcj2py.Ghcj2PyError) as exception:
            msg = exception.message
            raise GhcjMagicError('ghcj could not complete execution: %s' % msg)

        key = 'ghcjMagic.ghcj'
        display_data = []

        # Publish text output
        if text_output:
            display_data.append((key, {'text/plain': text_output}))

        for source, data in display_data:
            self._publish_display_data(source, data)


def load_ipython_extension(ip):
    """Load the extension in IPython and overwrite the normal IPython running
    instance's `run_cell` method with one that invokes the ghcj cell magic."""
    ip.register_magics(GhcjMagics)

    def new_run_cell(self, raw_cell, **kwds):
        self.run_cell_magic('ghcj', '', raw_cell)  # ignore **kwds

    from IPython.core.interactiveshell import InteractiveShell
    func_type = type(InteractiveShell.run_cell)
    #ip.run_cell = func_type(new_run_cell, ip, InteractiveShell)


class GhcjMagicError(Exception):
    pass
