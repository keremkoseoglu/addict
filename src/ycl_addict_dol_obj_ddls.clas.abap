CLASS ycl_addict_dol_obj_ddls DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_DDLS IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE ddtext FROM DDDDLSRCT                     "#EC CI_NOORDER
           WHERE ddlname    = @obj_name
             AND ddlanguage = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE ddlname FROM DDDDLSRC
           WHERE ddlname = @obj_name
           INTO @data(dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
