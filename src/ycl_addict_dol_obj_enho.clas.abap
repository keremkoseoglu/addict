CLASS ycl_addict_dol_obj_enho DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_ENHO IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE shorttext_id FROM enhheader           "#EC CI_NOORDER
           WHERE enhname = @obj_name
           INTO @DATA(stid).

    IF stid IS INITIAL.
      RETURN.
    ENDIF.

    DATA(trep) = NEW cl_enh_text_repository( VALUE #( ) ).

    TRY.
        output = trep->if_wb_text_repository~get_text( CONV #( stid ) ).
      CATCH cx_root ##no_Handler .
    ENDTRY.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE enhname FROM enhheader
           WHERE enhname = @obj_name
           INTO @data(dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
