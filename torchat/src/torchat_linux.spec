# -*- mode: python -*-

# this is the spec file for pyinstaller
# which is used in make_debian_package.py
# to freeze torchat.py into a one-file executable

a = Analysis([os.path.join(HOMEPATH,'support/_mountzlib.py'), os.path.join(HOMEPATH,'support/useUnicode.py'), 'torchat.py'],
             pathex=['.'])
pyz = PYZ(a.pure)
exe = EXE( pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name=os.path.join('dist', 'torchat'),
          debug=False,
          strip=True,
          upx=True,
          console=1 )
