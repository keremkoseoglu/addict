CLASS ycl_addict_transport_req_fwd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF input_dict,
             trkorr                    TYPE ytt_addict_trkorr_det,
             sysnam                    TYPE tmscsys-sysnam,
             mandt                     TYPE symandt,
             show_popup                TYPE abap_bool,
             pass_if_already_forwarded TYPE abap_bool,
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

    CONSTANTS: BEGIN OF trkorr,
                 some TYPE trkorr VALUE 'SOME',
               END OF trkorr.

    CONSTANTS critical_message_types TYPE char3 VALUE 'EAX'.

    DATA state TYPE state_dict.

    METHODS validate_input RAISING ycx_addict_method_parameter.
    METHODS forward_requests RAISING ycx_addict_function_subrc.
    METHODS transmit_queue RAISING ycx_addict_class_method.
    METHODS pass_already_forwarded RAISING ycx_addict_class_method.
ENDCLASS.



CLASS ycl_addict_transport_req_fwd IMPLEMENTATION.
  METHOD execute.
    " Execute request forward """""""""""""""""""""""""""""""""""""""
    TRY.
        me->state = VALUE #( input = input ).
        validate_input( ).

        IF me->state-input-pass_if_already_forwarded = abap_true.
          transmit_queue( ).
          pass_already_forwarded( ).

          IF me->state-input-trkorr IS INITIAL.
            RETURN.
          ENDIF.
        ENDIF.

        forward_requests( ).
        transmit_queue( ).

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


  METHOD validate_input.
    " Validate input parameters """""""""""""""""""""""""""""""""""""
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


  METHOD forward_requests.
    " Forward """""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(requests) =
      VALUE stms_tr_requests( FOR _trkorr IN me->state-input-trkorr (
                              trkorr = _trkorr ) ).

    CASE me->state-input-show_popup.
      WHEN abap_true. " Foreground mode
        ##FM_SUBRC_OK
        CALL FUNCTION 'TMS_UI_FORWARD_TR_REQUEST'
          EXPORTING
            iv_request             = me->trkorr-some
            iv_target              = me->state-input-sysnam
            it_requests            = requests
            iv_expert_mode         = abap_true
          EXCEPTIONS
            cancelled_by_user      = 1
            forward_request_failed = 2
            OTHERS                 = 3.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_UI_FORWARD_TR_REQUEST' ).

      WHEN abap_false. " Background mode
        DATA(retcode) = CONV stpa-retcode( space ).
        DATA(exception) = VALUE stmscalert( ).

        ##FM_SUBRC_OK
        CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
          EXPORTING
            iv_request                 = me->trkorr-some
            iv_target                  = me->state-input-sysnam
            iv_import_again            = abap_true
            it_requests                = requests
          IMPORTING
            ev_tp_ret_code             = retcode
            es_exception               = exception
          EXCEPTIONS
            read_config_failed         = 1
            table_of_requests_is_empty = 2
            OTHERS                     = 3.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TMS_MGR_FORWARD_TR_REQUEST' ).

        IF exception IS NOT INITIAL AND
           exception-msgty CA me->critical_message_types.

          RAISE EXCEPTION TYPE ycx_addict_function_subrc
            EXPORTING
              textid     = ycx_addict_function_subrc=>function_returned_error_txt
              funcname   = 'TMS_MGR_FORWARD_TR_REQUEST'
              error_text = CONV #( exception-text ).
        ENDIF.

        IF NOT ( retcode = '0000' OR retcode = '0' OR
                 retcode = '0004' OR retcode = '4' ).
          RAISE EXCEPTION TYPE ycx_addict_function_subrc
            EXPORTING
              textid   = ycx_addict_function_subrc=>function_returned_error
              funcname = 'TMS_MGR_FORWARD_TR_REQUEST'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD transmit_queue.
    " Transmit queue """"""""""""""""""""""""""""""""""""""""""""""""
    NEW ycl_addict_transmit_tr_queue( )->execute(
            VALUE #( sysnam     = me->state-input-sysnam
                     show_popup = me->state-input-show_popup ) ).
  ENDMETHOD.


  METHOD pass_already_forwarded.
    LOOP AT me->state-input-trkorr ASSIGNING FIELD-SYMBOL(<trkorr>).
      DATA(tmsbuffer) = ycl_addict_transport_request=>read_tmsbuffer(
                            trkorr = <trkorr>-trkorr
                            sysid  = CONV #( <trkorr>-tarsystem ) ).

      CHECK tmsbuffer IS NOT INITIAL.
      DELETE me->state-input-trkorr.
      CONTINUE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
