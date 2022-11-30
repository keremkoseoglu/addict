CLASS ycl_addict_dol_obj_ttyp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_TTYP IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE ddtext FROM dd40t                     "#EC CI_NOORDER
           WHERE typename   = @obj_name AND
                 ddlanguage = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE typename FROM dd40l
           WHERE typename = @obj_name
           INTO @data(dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
