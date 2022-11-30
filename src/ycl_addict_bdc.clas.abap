CLASS ycl_addict_bdc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF dismode,
                 all   TYPE ctu_mode VALUE 'A',
                 error TYPE ctu_mode VALUE 'E',
                 none  TYPE ctu_mode VALUE 'N',
               END OF dismode.

    METHODS add_fld
      IMPORTING !nam TYPE fnam_____4
                !val TYPE bdc_fval.

    METHODS add_scr
      IMPORTING !prg TYPE bdc_prog
                !dyn TYPE bdc_dynr.

    METHODS clear.

    METHODS close_group RAISING ycx_addict_function_subrc.

    METHODS insert_tcode
      IMPORTING !tcode TYPE tcode
      RAISING   ycx_addict_function_subrc.

    METHODS open_group
      IMPORTING !group TYPE apqi-groupid
      RAISING   ycx_addict_function_subrc.

    METHODS submit
      IMPORTING !tcode        TYPE sytcode
                !option       TYPE ctu_params
                !validate_msg TYPE abap_bool DEFAULT abap_false
      EXPORTING !msg          TYPE ytt_addict_bdcmsgcoll
      RAISING   ycx_addict_bdc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES bdcdata_list TYPE STANDARD TABLE OF bdcdata WITH EMPTY KEY.
    DATA bdcdata TYPE bdcdata_list.
ENDCLASS.



CLASS YCL_ADDICT_BDC IMPLEMENTATION.


  METHOD add_fld.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Append new field for batch input
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    APPEND VALUE #( fnam = nam fval = val ) TO me->bdcdata.
  ENDMETHOD.


  METHOD add_scr.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Append new screen for batch input
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    APPEND VALUE #( program  = prg
                    dynpro   = dyn
                    dynbegin = abap_true
                  ) TO me->bdcdata.
  ENDMETHOD.


  METHOD clear.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Clears batch input content
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR me->bdcdata.
  ENDMETHOD.


  METHOD close_group.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Close BDC group (SM35)
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
        not_open    = 1
        queue_error = 2
        OTHERS      = 3 ##FM_SUBRC_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_CLOSE_GROUP' ).
    clear( ).
  ENDMETHOD.


  METHOD insert_tcode.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Append transaction code for batch input
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode            = tcode
      TABLES
        dynprotab        = me->bdcdata
      EXCEPTIONS
        internal_error   = 1
        not_open         = 2
        queue_error      = 3
        tcode_invalid    = 4
        printing_invalid = 5
        posting_invalid  = 6
        OTHERS           = 7 ##FM_SUBRC_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_INSERT' ).
    clear( ).
  ENDMETHOD.


  METHOD open_group.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Open group for SM35
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        group               = group
        keep                = abap_true
        user                = sy-uname
      EXCEPTIONS
        client_invalid      = 1
        destination_invalid = 2
        group_invalid       = 3
        group_is_locked     = 4
        holddate_invalid    = 5
        internal_error      = 6
        queue_error         = 7
        running             = 8
        system_lock_error   = 9
        user_invalid        = 10
        OTHERS              = 11 ##FM_SUBRC_OK ##NUMBER_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'BDC_OPEN_GROUP' ).
  ENDMETHOD.


  METHOD submit.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Start Batch Input
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL TRANSACTION tcode                               "#EC CI_CALLTA
         USING me->bdcdata
         MESSAGES INTO msg
         OPTIONS FROM option.

    IF validate_msg = abap_true.
      LOOP AT msg TRANSPORTING NO FIELDS
              WHERE msgtyp IN ycl_simbal=>get_crit_msgty_range( ).
        RAISE EXCEPTION TYPE ycx_addict_bdc EXPORTING bdcmsgcoll = msg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
