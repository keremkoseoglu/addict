CLASS ycl_addict_dol_obj_intf DEFINITION
  INHERITING FROM ycl_addict_dol_obj_clas
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS yif_addict_dol_obj~get_object_txt REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_addict_dol_obj_intf IMPLEMENTATION.
  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.
ENDCLASS.
