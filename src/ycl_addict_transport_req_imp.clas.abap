CLASS ycl_addict_transport_req_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF input_dict,
             sysnam       TYPE tmssysnam,
             rfcdest      TYPE rfcdest,
             mandt        TYPE symandt,
             trkorr       TYPE ytt_addict_trkorr_det,
             show_popup   TYPE abap_bool,
             notify_users TYPE abap_bool,
           END OF input_dict.

    METHODS execute
      IMPORTING !input TYPE input_dict
      RAISING   ycx_addict_class_method.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF state_dict,
             input TYPE input_dict,
           END OF state_dict.

    CONSTANTS: BEGIN OF field,
                 sysnam TYPE seocpdname VALUE 'SYSNAM',
                 trkorr TYPE seocpdname VALUE 'TRKORR',
               END OF field.

    CONSTANTS: BEGIN OF table,
                 tmscdom TYPE tabname VALUE 'TMSCDOM',
               END OF table.

    CONSTANTS: BEGIN OF method,
                 execute          TYPE seocpdname VALUE 'EXECUTE',
                 lazy_read_domnam TYPE seocpdname VALUE 'LAZY_READ_DOMNAM',
               END OF method.

    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_TRANSPORT_REQ_IMP',
               END OF class.

    CONSTANTS: BEGIN OF trkorr,
                 some TYPE trkorr VALUE 'SOME',
               END OF trkorr.

    CONSTANTS: critical_message_types TYPE char3 VALUE 'EAX'.

    CLASS-DATA domnam TYPE tmscdom-domnam.
    DATA state TYPE state_dict.

    CLASS-METHODS lazy_read_domnam RAISING ycx_addict_class_method.

    METHODS validate_input RAISING ycx_addict_method_parameter.
    METHODS notify_users.

    METHODS import_requests
      RAISING
        ycx_addict_function_subrc
        ycx_addict_class_method.
ENDCLASS.



CLASS ycl_addict_transport_req_imp IMPLEMENTATION.
  METHOD execute.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Execute request import
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        me->state = VALUE #( input = input ).
        validate_input( ).
        notify_users( ).
        import_requests( ).

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
    DATA(domnam) = REF #( ycl_addict_transport_req_imp=>domnam ).

    IF domnam->* IS NOT INITIAL.
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
        domnam->* = domnams[ 1 ].
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


  METHOD validate_input.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Validate input parameters
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->state-input-sysnam IS INITIAL.
      RAISE EXCEPTION TYPE ycx_addict_method_parameter
        EXPORTING
          textid      = ycx_addict_method_parameter=>param_missing
          class_name  = CONV #( ycl_addict_class=>get_class_name( me ) )
          method_name = me->method-execute
          param_name  = me->field-sysnam.
    ENDIF.

    IF me->state-input-trkorr IS INITIAL.
      RAISE EXCEPTION TYPE ycx_addict_method_parameter
        EXPORTING
          textid      = ycx_addict_method_parameter=>param_missing
          class_name  = CONV #( ycl_addict_class=>get_class_name( me ) )
          method_name = me->method-execute
          param_name  = me->field-trkorr.
    ENDIF.
  ENDMETHOD.


  METHOD notify_users.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Notify users in the target system about the transport in progress.
    " Why?
    " Some ABAP programs may produce errors due to the active import.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->state-input-notify_users = abap_true.

    DATA(expire_date) = sy-datum.
    DATA(expire_time) = sy-uzeit.
    DATA(thour)       = CONV yd_addict_thour( 1 / 10 ).

    ycl_addict_datetime_toolkit=>add_to_time(
      EXPORTING idate = sy-datum
                itime = sy-uzeit
                stdaz = thour
      IMPORTING edate = expire_date
                etime = expire_time ).

    CALL FUNCTION 'SM02_ADD_MESSAGE'
      DESTINATION me->state-input-rfcdest
      EXPORTING
        message              = TEXT-505
        message2             = TEXT-506
        message3             = TEXT-507
        expiration_date      = expire_date
        expiration_time      = expire_time
        delete_date          = expire_date
        delete_time          = expire_time
      EXCEPTIONS
        empty_message        = 1
        server_not_available = 2
        client_not_available = 3
        not_authorized       = 4
        langu_not_available  = 5
        OTHERS               = 6 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD import_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Actually import requests into the target system
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Transmit Queue """"""""""""""""""""""""""""""""""""""""""""""""
    IF me->state-input-show_popup = abap_true.
      CALL FUNCTION 'TMS_UI_TRANSMIT_TR_QUEUE'
        EXPORTING
          iv_system             = me->state-input-sysnam
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
          iv_tar_sys                = me->state-input-sysnam
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
          iv_tar_sys                = me->state-input-sysnam
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

    " Import """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(requests) = VALUE stms_tr_requests(
        FOR _trkorr IN me->state-input-trkorr (
            trkorr = _trkorr ) ).

    IF me->state-input-show_popup = abap_true.
      CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system             = me->state-input-sysnam
          iv_request            = me->trkorr-some
          iv_tarcli             = me->state-input-mandt
          it_requests           = requests
        EXCEPTIONS
          cancelled_by_user     = 1
          import_request_denied = 2
          import_request_failed = 3
          OTHERS                = 4
          ##FM_SUBRC_OK.

      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_UI_IMPORT_TR_REQUEST' ).

    ELSE.
      DATA(retcode) = CONV stpa-retcode( space ).
      DATA(exception) = VALUE stmscalert( ).

      CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system                  = me->state-input-sysnam
          iv_request                 = me->trkorr-some
          iv_client                  = me->state-input-mandt
          iv_overtake                = abap_true  " Leave transport request in queue
          iv_import_again            = abap_true  " Import transport request again
          iv_ignore_originality      = abap_true  " Overwrite originals
          iv_ignore_repairs          = abap_true  " Overwrite objects in unconfirmed repairs
          iv_ignore_transtype        = abap_false " Ignore invalid transport type
          iv_ignore_tabletype        = abap_false " Ignore invalid table class
          iv_ignore_qaflag           = abap_false " ?
          iv_ignore_predec           = abap_false " Skip predecessior relationships
          iv_ignore_cvers            = abap_true  " Ignore invalid component version
          iv_ignore_spam             = abap_false " ?
          it_requests                = requests
        IMPORTING
          ev_tp_ret_code             = retcode
          es_exception               = exception
        EXCEPTIONS
          read_config_failed         = 1
          table_of_requests_is_empty = 2
          OTHERS                     = 3 ##FM_SUBRC_OK.

      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_IMPORT_TR_REQUEST' ).

      IF exception IS NOT INITIAL AND
         exception-msgty CA me->critical_message_types.

        RAISE EXCEPTION TYPE ycx_addict_function_subrc
          EXPORTING
            textid     = ycx_addict_function_subrc=>function_returned_error_txt
            funcname   = 'TMS_MGR_IMPORT_TR_REQUEST'
            error_text = CONV #( exception-text ).
      ENDIF.

      IF NOT ( retcode = '0000' OR retcode = '0' OR
               retcode = '0004' OR retcode = '4' ).
        RAISE EXCEPTION TYPE ycx_addict_function_subrc
          EXPORTING
            textid   = ycx_addict_function_subrc=>function_returned_error
            funcname = 'TMS_MGR_IMPORT_TR_REQUEST'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
