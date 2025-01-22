*"* use this source file for your ABAP unit test classes
CLASS lcl_test DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS parentify_tadir_key FOR TESTING.

ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD parentify_tadir_key.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE ycl_addict_package=>tadir_key_dict( pgmid    = 'R3TR'
                                                        object   = 'TABL'
                                                        obj_name = 'DUMMY' )
        act = ycl_addict_transport_request=>parentify_tadir_key( VALUE #( pgmid    = 'R3TR'
                                                                          object   = 'TABL'
                                                                          obj_name = 'DUMMY' ) ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = VALUE ycl_addict_package=>tadir_key_dict( pgmid    = 'R3TR'
                                                        object   = 'CLAS'
                                                        obj_name = 'ZCL_SD_E_INV_020' )
        act = ycl_addict_transport_request=>parentify_tadir_key(
                  VALUE #( pgmid    = 'LIMU'
                           object   = 'METH'
                           obj_name = 'ZCL_SD_E_INV_020              ZIF_BC_ENH_IMP~ENHANCE' ) ) ).
  ENDMETHOD.
ENDCLASS.
