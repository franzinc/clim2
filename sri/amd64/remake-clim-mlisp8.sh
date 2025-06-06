make clean
make
cp clim*fasl /homedir/brg/aic/allegro-cl-amd64-linux/11.0/code/
cp *dylib    /homedir/brg/aic/allegro-cl-amd64-linux/11.0/code/
rm -f /homedir/brg/aic/allegro-cl-amd64-linux/11.0/clim8
rm -f /homedir/brg/aic/allegro-cl-amd64-linux/11.0/clim8.dxl
/homedir/brg/aic/allegro-cl-amd64-linux/11.0/alisp8 -e '(progn (load "buildclim" ) (exit))'
mv /homedir/brg/aic/allegro-cl-amd64-linux/11.0/clim8.dxl /homedir/brg/aic/allegro-cl-amd64-linux/11.0/alisp8.dxl 



