# -*- coding utf-8 -*-
"""
Module for controlling a ghci session from python.
"""

#-----------------------------------------------------------------------------
#  Copyright (C) 2013 Rogan Creswick and Benjamin Jones
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

import pexpect
import re

GHCI_CMD = 'ghci'
#SET_PLAIN_PROMPT=':set  prompt "%s>"'
GHCI_PROMPT = re.compile('^[^\r\n]*> ', re.MULTILINE)
#GHCI_PROMPT = re.compile('^Prelude> ', re.MULTILINE)

# The prefix string to remove from ghci results:
RESULT_PFX = '0d0d0a1b5b3f316c1b3e'.decode('hex')

class Ghci2Py:

    def __init__(self):
        self.child = setup_ghci_process()

    def put(self, key, value):
        pass

    def get(self, key):
        pass

    def run(self, code, verbose=False):
        return self.send_command(code, verbose)

    def send_command(self, cmd, verbose):
        if verbose:
            print "sending command '%s'"%cmd

        self.child.sendline(cmd)
        self.child.expect(GHCI_PROMPT)
        # remove the cmd string, and the new line:
        pfx_len = len(RESULT_PFX)
        raw_str = self.child.before[len(cmd):]
        if verbose:
            print "raw result: '%s'"%toHex(raw_str)

        str = raw_str[pfx_len:]

        # remove the trailing new line:
        if len(str) > 0:
            str = str[:-2]
        if verbose:
            print "received result: '%s'"%str
            print "received result: '%s'"%toHex(str)
        return str

def setup_ghci_process():
    """
    Static method that setups up an interactive ghci process.
    """
    child = pexpect.spawn(GHCI_CMD)
#    print "waiting for prompt"
    child.expect(GHCI_PROMPT)
#    send_command(child, SET_PLAIN_PROMPT)
#    print "sending"
#    print "result: '%s'"%res
#    print "result: '%s'"%toHex(res)
    return child



#convert string to hex
def toHex(s):
    return s.encode('hex')

#convert hex repr to string
def toStr(s):
    return s.decode('hex')