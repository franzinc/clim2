(in-package :tk)

(def-c-typedef :cardinal :unsigned-int)

(def-c-type (xtk-class :in-foreign-space) :struct
  (superclass :long)
  (name * :char)
  (widget-size :cardinal)
  (class-part-initialize * :unsigned-long)
  (inited :unsigned-long))

(def-c-type (x-resource :in-foreign-space) :struct
  (name * :char)
  (class * :char)
  (type * :char)
  (size :cardinal)
  (offset :cardinal)
  (default-type * :char)
  (default-addr * :char)
  )

(def-c-type (xtk-widget :in-foreign-space) :struct
  (self :unsigned-long)
  (widget-class :unsigned-long)
  )

(def-c-type (x-resource-list :in-foreign-space) 1 x-resource)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defforeign 'get-resource-list-1 :entry-point "_XtGetResourceList")
(defforeign 'get-constraint-resource-list-1 :entry-point "_XtGetConstraintResourceList")

(defforeign 'initialize-widget-class :entry-point "_XtInitializeWidgetClass")

(defforeign 'x-free :entry-point "_XtFree")

(defforeign 'toolkit-initialize
    :entry-point "_XtToolkitInitialize")

(defforeign 'create_application_context
    :entry-point "_XtCreateApplicationContext")


(defforeign 'app_set_error_handler
    :entry-point "_XtAppSetErrorHandler")

(defforeign 'app_set_warning_handler
    :entry-point "_XtAppSetWarningHandler")

(defforeign 'open_display
    :entry-point "_XtOpenDisplay")

(defforeign 'app_create_shell 
    :entry-point "_XtAppCreateShell")
