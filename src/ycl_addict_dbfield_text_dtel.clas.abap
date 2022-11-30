CLASS ycl_addict_dbfield_text_dtel DEFINITION
  PUBLIC
  INHERITING FROM ycl_addict_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DBFIELD_TEXT_DTEL IMPLEMENTATION.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns data element text
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        output = ycl_addict_data_element=>get_instance( dbfield )->get_text( ).
      CATCH cx_root ##no_Handler.
    ENDTRY.

    IF output IS INITIAL AND me->next IS NOT INITIAL.
      output = me->next->get_text( dbfield ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
