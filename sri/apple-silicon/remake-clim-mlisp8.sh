make clean
make
cp clim*fasl /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/code/
cp *dylib    /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/code/
rm -f /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/clim8
rm -f /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/clim8.dxl
/homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/alisp8 -e '(progn (load "buildclim" ) (exit))'
mv /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/clim8.dxl /homedir/brg/aic/allegro-cl-arm64-darwin/11.0-test/alisp8.dxl 



