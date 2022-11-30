CLASS ycl_addict_dol_obj_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_PROG IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE text FROM trdirt
           WHERE name  = @obj_name AND
                 sprsl = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE name FROM trdir
           WHERE name = @obj_name
           INTO @data(dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
