CLASS ycl_addict_dol_obj_fugr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_FUGR IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE areat FROM tlibt
           WHERE area  = @obj_name AND
                 spras = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE area FROM tlibg
           WHERE area = @obj_name
           INTO @data(dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
