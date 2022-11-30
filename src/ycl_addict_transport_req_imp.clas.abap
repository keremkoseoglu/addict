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

    CONSTANTS: BEGIN OF method,
                 execute TYPE seocpdname VALUE 'EXECUTE',
               END OF method.

    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_TRANSPORT_REQ_IMP',
               END OF class.

    CONSTANTS: BEGIN OF trkorr,
                 some TYPE trkorr VALUE 'SOME',
               END OF trkorr.

    CONSTANTS: critical_message_types TYPE char3 VALUE 'EAX'.

    DATA state TYPE state_dict.

    METHODS validate_input RAISING ycx_addict_method_parameter.
    METHODS notify_users.

    METHODS import_requests
      RAISING
        ycx_addict_function_subrc
        ycx_addict_class_method.
ENDCLASS.



CLASS YCL_ADDICT_TRANSPORT_REQ_IMP IMPLEMENTATION.


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


  METHOD import_requests.
    " Transmit queue """"""""""""""""""""""""""""""""""""""""""""""""
    NEW ycl_addict_transmit_tr_queue( )->execute(
            VALUE #( sysnam     = me->state-input-sysnam
                     show_popup = me->state-input-show_popup ) ).

    " Import requests """""""""""""""""""""""""""""""""""""""""""""""
    DATA(requests) =
      VALUE stms_tr_requests( FOR _trkorr IN me->state-input-trkorr (
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
ENDCLASS.
