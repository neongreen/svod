#!/usr/bin/env python
#
# This Python 3 script prepares the directory for running of the
# application. It creates some directories if they don't exist and also
# clones some info directories and refreshes some files.
#
# Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
#
# This script is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This script is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

import os
import shutil
import subprocess
import tempfile

MUST_HAVE_DIRS = [
    'info/',
    'user-content/release/',
    'user-content/staging/',
]

EXTERNAL_ASSETS = {
    'https://github.com/svod-music/svod-logo': {
        'favicon.ico':                    'config/favicon.ico',
    },
    'https://github.com/svod-music/svod-concepts': {
        'кодеки.md':                      'info/кодеки.md',
        'контактная-информация.md':       'info/контактная-информация.md',
        'краткий-тур.md':                 'info/краткий-тур.md',
        'лицензии.md':                    'info/лицензии.md',
        'о-проекте.md':                   'info/о-проекте.md',
        'поддержать-свод.md':             'info/поддержать-свод.md',
        'пользовательское-соглашение.md': 'info/пользовательское-соглашение.md',
        'содержимое.md':                  'info/содержимое.md',
        'язык-разметки.md':               'info/язык-разметки.md',
    },
}

# First of all we need to make sure some important directories exist.

for path in MUST_HAVE_DIRS:
    print('Making sure ‘{}’ exists…'.format(path))
    os.makedirs(path, exist_ok=True)

# Now we need to clone some git repositories containing files of our
# application and put them into the right places. We call plain external
# ‘git’ executable here, to avoid dependencies and because it's easy.

for repo, items in EXTERNAL_ASSETS.items():
    with tempfile.TemporaryDirectory(prefix='svod-assets-') as tdir:
        subprocess.check_call(['git', 'clone', repo, tdir])
        for src, dst in items.items():
            print('  Copying ‘{}’ to ‘{}’…'.format(src, dst))
            try:
                shutil.copyfile(os.path.join(tdir, src), dst)
            except (IOError, OSError) as e:
                print('    ', e)
            else:
                print('  OK: ‘{}’'.format(dst))
