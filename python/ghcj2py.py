# -*- coding utf-8 -*-
"""
Module for controlling a ghcj session from python.
"""

#-----------------------------------------------------------------------------
#  Copyright (C) 2013 Rogan Creswick and Benjamin Jones
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

import base64
import json
import os
import pexpect
#import sys


class Ghcj2Py:

    def __init__(self):
        try:
            cmd = os.environ['GHCJ_BINARY']
        except KeyError:
            cmd = 'ghcj'
        self.child = setup_ghcj_process(cmd)
        #self.child.logfile = sys.stdout

    def run(self, code, verbose=False):
        results = self.send_code(code, verbose=verbose)

        # error checking?
        if results == "":
            return "<EMPTY>"

    def send_code(self, code, verbose=False):
        if verbose:
            print "---sending command---\n"
            print code
            print "\n---------------------"

        # encode and send
        js = to_json(code)
        if verbose:
            print 'sending json: %s' % js
        b64js = base64.b64encode(js)
        if verbose:
            print 'sending b64json: %s' % b64js
        self.send(b64js)

        # receive and decode
        b64js = self.recv()
        if verbose:
            print 'received b64json: %s' % b64js
        js = base64.b64decode(b64js)
        if verbose:
            print 'received json: %s' % js

        res = from_json(js)
        if verbose:
            print "received result: '%s'" % res

        return res

    def send(self, txt):
        self.child.sendline(txt + '\n')

    def recv(self):
        self.child.expect('"(.+)"')
        if self.child.match:
            return self.child.match.group(1)
        else:
            return "Error: did not match"


def setup_ghcj_process(cmd):
    """
    Static method that setups up an interactive ghcj process.
    """
    try:
        child = pexpect.spawn(cmd)
    except:
        raise Ghcj2PyError('failed to start ghcj process')

    return child


JSON_INPUT_CELL = 'inputCellNo'
JSON_INPUT_CODE = 'inputSource'


def to_json(code):
    #obj = {'Input': {JSON_INPUT_CELL: 0, JSON_INPUT_CODE: code}}
    obj = {JSON_INPUT_CELL: 0, JSON_INPUT_CODE: code}
    return json.dumps(obj)


JSON_OUTPUT_CELL = 'outputCellNo'
JSON_RESULT_FIELD = 'outputData'


def from_json(js):
    obj = json.loads(js)
    if 'ParseError' in obj:
        return obj['ParseError']
    elif 'CompilerError' in obj:
        return obj['CompilerError']
    elif 'CompilerWarning' in obj:
        return obj['CompilerWarning']
    elif 'Output' in obj:
        return obj['Output'][JSON_RESULT_FIELD]
    else:
        raise Ghcj2PyError('unexpected JSON content from ghcj: %s' % js)


class Ghcj2PyError(Exception):
    pass
