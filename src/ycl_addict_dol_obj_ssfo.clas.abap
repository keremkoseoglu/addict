CLASS ycl_addict_dol_obj_ssfo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_addict_dol_obj_ssfo IMPLEMENTATION.
  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE caption FROM stxfadmt
           WHERE formname   = @obj_name AND
                 langu      = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE formname FROM stxfadm
           WHERE formname = @obj_name
           INTO @DATA(lv_dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
