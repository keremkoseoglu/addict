CLASS ycl_addict_dol_obj_doma DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_DOMA IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    TRY.
        output = ycl_addict_domain=>get_instance( CONV #( obj_name ) )->get_text( ).
      CATCH cx_root ##no_Handler.
    ENDTRY.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    TRY.
        ycl_addict_domain=>get_instance( CONV #( obj_name ) ).
        output = abap_false.
      CATCH cx_root.
        output = abap_true.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
