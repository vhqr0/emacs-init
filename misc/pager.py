#!/usr/bin/env python3

import sys
import os
import hashlib

content = sys.stdin.read()

tmpfile = f'/tmp/{hashlib.md5(content.encode()).hexdigest()}.emacs-pager'

with open(tmpfile, 'w') as f:
    f.write(content)

os.system(f'emacsclient {tmpfile}')

os.remove(tmpfile)
