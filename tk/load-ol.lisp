(in-package :tk)

#|
make ucl_xtras='/usr/tech/cer/stuff/clim-2.0/ol-classes.o /usr/tech/cer/stuff/clim-2.0/lib/libXol.a /usr/tech/cer/stuff/clim-2.0/lib/libXt.a /usr/tech/cer/stuff/clim-2.0/lib/libX11.a' ucl
|#

(flet ((foundp (entry-point)
	       (let ((x (make-array 1 :initial-contents
				    (list (ff:convert-to-lang
					   entry-point))))
		     (y 
		 
		      (make-array 1 :element-type '(unsigned-byte 32))))
		 (zerop (ff:get-entry-points x y)))))
  (unless (foundp "insert_classes")
    #+ingnore
	(mapc #'foreign-functions:remove-entry-point 
	      '("__unpack_quadruple" 
		"__prod_b10000" 
		"__carry_out_b10000" 
		"__prod_65536_b10000"))
	(load "classes.o" 
	      :foreign-files 
	      '("/vapor/usr/tech/cer/stuff/clim-2.0/lib/libXol.a"
		"/vapor/usr/tech/cer/stuff/clim-2.0/lib/libXt.a"
		"/vapor/usr/tech/cer/stuff/clim-2.0/lib/libX11.a") 
	      :print t)))
