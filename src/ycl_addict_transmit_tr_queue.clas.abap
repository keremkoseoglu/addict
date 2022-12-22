CLASS ycl_addict_transmit_tr_queue DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES: BEGIN OF input_dict,
             sysnam     TYPE tmssysnam,
             show_popup TYPE abap_bool,
           END OF input_dict.

    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO ycl_addict_transmit_tr_queue.

    METHODS execute
      IMPORTING !input TYPE input_dict
      RAISING   ycx_addict_class_method.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_TRANSMIT_TR_QUEUE',
               END OF class.

    CONSTANTS: BEGIN OF method,
                 execute          TYPE seocpdname VALUE 'EXECUTE',
                 lazy_read_domnam TYPE seocpdname VALUE 'LAZY_READ_DOMNAM',
               END OF method.

    CONSTANTS: BEGIN OF table,
                 tmscdom TYPE tabname VALUE 'TMSCDOM',
               END OF table.

    CLASS-DATA singleton TYPE REF TO ycl_addict_transmit_tr_queue.
    DATA domnam TYPE tmscdom-domnam.
    METHODS lazy_read_domnam RAISING ycx_addict_class_method.

ENDCLASS.



CLASS ycl_addict_transmit_tr_queue IMPLEMENTATION.
  METHOD get_instance.
    IF ycl_addict_transmit_tr_queue=>singleton IS INITIAL.
      ycl_addict_transmit_tr_queue=>singleton = NEW #( ).
    ENDIF.
    result = ycl_addict_transmit_tr_queue=>singleton.
  ENDMETHOD.


  METHOD execute.
    " Transmit queue """"""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        IF input-show_popup = abap_true.
          CALL FUNCTION 'TMS_UI_TRANSMIT_TR_QUEUE'
            EXPORTING
              iv_system             = input-sysnam
            EXCEPTIONS
              cancelled_by_user     = 1
              without_refresh       = 2
              transmit_queue_failed = 3
              OTHERS                = 4
              ##FM_SUBRC_OK.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_UI_TRANSMIT_TR_QUEUE' ).

        ELSE.
          lazy_read_domnam( ).
          DATA(tp_trque) = VALUE stms_tp_trque( ).

          CALL FUNCTION 'TMS_MGR_TRANSMIT_TR_QUEUE'
            EXPORTING
              iv_tar_sys                = input-sysnam
              iv_tar_dom                = me->domnam
              iv_src_sys                = space
              iv_src_dom                = me->domnam
              iv_loc_grp                = abap_true
              iv_ext_grp                = abap_true
              iv_read_only              = abap_true
              iv_monitor                = abap_true
              iv_verbose                = abap_false
            CHANGING
              cs_tp_trque               = tp_trque
            EXCEPTIONS
              read_config_failed        = 1
              system_not_found          = 2
              group_not_found           = 3
              no_source_systems_found   = 4
              feature_not_available     = 5
              identical_groups          = 6
              check_group_config_failed = 7
              invalid_group_config      = 8
              OTHERS                    = 9 ##FM_SUBRC_OK.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_TRANSMIT_TR_QUEUE' ).

          CALL FUNCTION 'TMS_MGR_TRANSMIT_TR_QUEUE'
            EXPORTING
              iv_tar_sys                = input-sysnam
              iv_tar_dom                = me->domnam
              iv_read_only              = abap_false
              iv_use_list               = abap_true
              iv_without_ftp            = abap_false
              iv_monitor                = abap_true
              iv_verbose                = abap_false
            CHANGING
              cs_tp_trque               = tp_trque
            EXCEPTIONS
              read_config_failed        = 1
              system_not_found          = 2
              group_not_found           = 3
              no_source_systems_found   = 4
              feature_not_available     = 5
              identical_groups          = 6
              check_group_config_failed = 7
              invalid_group_config      = 8
              OTHERS                    = 9
              ##FM_SUBRC_OK.

          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_TRANSMIT_TR_QUEUE' ).

        ENDIF.

      CATCH ycx_addict_class_method INTO DATA(method_error).
        RAISE EXCEPTION method_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_class_method
          EXPORTING
            textid   = ycx_addict_class_method=>unexpected_error
            previous = diaper
            class    = CONV #( ycl_addict_class=>get_class_name( me ) )
            method   = me->method-execute.
    ENDTRY.
  ENDMETHOD.


  METHOD lazy_read_domnam.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Read the transport domain.
    " This method assumes that TMSCDOM will have a single entry.
    " If there are multiple entries, this method will generate
    " an error. In that case, the method would need to be
    " modified to return the accurate DOMNAM.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->domnam IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT domnam FROM tmscdom INTO TABLE @DATA(domnams).

    CASE lines( domnams ).
      WHEN 0.
        RAISE EXCEPTION TYPE ycx_addict_class_method
          EXPORTING
            textid   = ycx_addict_class_method=>unimplemented_feature
            class    = class-me
            method   = method-lazy_read_domnam
            previous = NEW ycx_addict_table_content(
                       textid    = ycx_addict_table_content=>table_empty
                       tabname   = table-tmscdom ).
      WHEN 1.
        me->domnam = domnams[ 1 ].
      WHEN OTHERS.
        RAISE EXCEPTION TYPE ycx_addict_class_method
          EXPORTING
            textid   = ycx_addict_class_method=>unimplemented_feature
            class    = class-me
            method   = method-lazy_read_domnam
            previous = NEW ycx_addict_table_content(
                       textid    = ycx_addict_table_content=>multiple_entries
                       tabname   = table-tmscdom ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
