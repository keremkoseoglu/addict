*"* use this source file for your ABAP unit test classes
CLASS lcl_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_uncut_alv_fcat_titles FOR TESTING.

ENDCLASS.


CLASS lcl_test_double DEFINITION
  INHERITING FROM ycl_addict_dynamic_itab
  FRIENDS lcl_test.
ENDCLASS.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CLASS lcl_test IMPLEMENTATION.
  METHOD test_uncut_alv_fcat_titles.
    DATA(normal_fcat) = VALUE slis_t_fieldcat_alv( ( seltext_l = 'ABCDE'
                                                     intlen    = 2 )
                                                   ( seltext_m = 'ABCDE'
                                                     intlen    = 9 )
                                                   ( seltext_l = 'ABCDEF'
                                                     seltext_m = 'ABC'
                                                     intlen    = 2 ) ).

    DATA(uncut_fcat) = lcl_test_double=>uncut_alv_fcat_titles( REF #( normal_fcat ) ).

    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = uncut_fcat[ 1 ]-outputlen ).

    cl_abap_unit_assert=>assert_equals( exp = 9
                                        act = uncut_fcat[ 2 ]-outputlen ).

    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = uncut_fcat[ 3 ]-outputlen ).
  ENDMETHOD.
ENDCLASS.
