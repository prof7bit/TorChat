# -*- mode: python -*-
import os

a = Analysis([os.path.join(HOMEPATH,'support\\_mountzlib.py'), os.path.join(HOMEPATH,'support\\useUnicode.py'), 'torchat.py'],
             pathex=['.'])
pyz = PYZ(a.pure)
exe = EXE( pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name=os.path.join('dist', 'torchat.exe'),
          debug=False,
          strip=False,
          upx=True,
          console=False,
          icon='icons\\torchat.ico'
		  )
