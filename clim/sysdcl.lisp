(defsys::defsystem :clim
;; 
;; copyright (c) 1985, 1986 Franz Inc, Alameda, Ca.  All rights reserved.
;; copyright (c) 1986-1991 Franz Inc, Berkeley, Ca.  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Suppplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;;
    (:default-pathname (frob-pathname "clim"))
  ("pkg")
  ("gestures")
  ("defprotocol")
  ("stream-defprotocols")
  ("defresource")
  ("temp-strings")
  ("clim-defs")
  ("stipples")
  ("stream-class-defs")
  ("interactive-defs")
  ("cursor")
  ("input-defs")
  ("view-defs")
  ("formatted-output-defs")
  
  ("input-protocol")
  ("output-protocol")

  ;; 

  ("output-recording-protocol")
  ("output-recording-defs")
  ("standard-tree")
  ("text-output-recording")
  ("standard-sequence")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ("interactive-protocol")
  ("input-editor-commands")
  ("completer")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ("ptypes1")
  ("presentations")
  ("translators")
  ("histories")
  ("ptypes2")
  ("standard-types")
  ("present")
  ("accept")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ("command")
  ("command-processor")
  ("basic-translators")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ("window-stream")
  

  ("frames")
  ("layout")

  ;; dashboard: need the macros from there
  
  ("db-layout")
  ("db-box")
  ("db-table")
  
  ;; db-border?????

  ("gadgets")
  ("db-scroll")
  
  ("db-stream")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ("incremental-redisplay"
   :load-before-compile ("clim-defs" "output-recording-protocol"))
  
  ("table-formatting"
   :load-before-compile ("clim-defs" "incremental-redisplay"))
  
  ("accept-values"
   :load-before-compile ("clim-defs" "incremental-redisplay"))

  ("menus")
  ("text-formatting")
  ("stream-trampolines"
   :load-before-compile ("defprotocol" "stream-defprotocols")
   ))


