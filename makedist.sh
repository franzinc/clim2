
set -x

make clean
make RECORD_XREF_INFO=nil RECORD_SOURCE_FILE_INFO=nil

rm -fr dist temp.zip temp.exe
mkdir dist dist/code dist/src dist/src/clim dist/src/clim/demo
cp -p climnt.fasl climps.fasl climdemo.fasl climg.fasl dist/code
cp -p readme.win32 dist/clim221.txt
cp -p demo/*.lisp dist/src/clim/demo

/winzip/winzip32 -min -a -r -sclim.me -ex temp.zip dist
/winzipse/wzipse32 temp.zip -y -3 -win32 -le \
	-d "c:\Program Files\acl50b" -c notepad clim221.txt

mv temp.zip clim221.zip
mv temp.exe clim221.exe
