INTERFACE yif_addict_class_visitor
  PUBLIC .

  METHODS visit
    IMPORTING
      !class TYPE REF TO ycl_addict_class
    RAISING
      ycx_addict_class_method.

ENDINTERFACE.
