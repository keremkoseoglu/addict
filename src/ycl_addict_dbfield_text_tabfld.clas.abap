CLASS ycl_addict_dbfield_text_tabfld DEFINITION
  PUBLIC
  INHERITING FROM ycl_addict_dbfield_text_abs
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DBFIELD_TEXT_TABFLD IMPLEMENTATION.


  METHOD get_text.
    TRY.
        output = ycl_addict_table_field=>get_instance_by_fullname( dbfield )->get_text( ).
      CATCH cx_root ##no_Handler.
    ENDTRY.

    IF output IS INITIAL AND me->next IS NOT INITIAL.
      output = me->next->get_text( dbfield ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
