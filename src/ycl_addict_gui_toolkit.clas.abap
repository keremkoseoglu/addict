CLASS ycl_addict_gui_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS are_you_sure
      IMPORTING !text TYPE clike
      RAISING   ycx_addict_user_input.

    CLASS-METHODS display_transport_requests
      IMPORTING requests TYPE ycl_addict_transport_request=>trkorr_list.

    CLASS-METHODS is_gui_on          RETURNING VALUE(result) TYPE abap_bool.
    CLASS-METHODS report_fatal_error IMPORTING !error        TYPE REF TO cx_root.

    CLASS-METHODS popup_strings
      IMPORTING strings TYPE vmcdbg_string_table
                !title  TYPE clike OPTIONAL
                !header TYPE clike OPTIONAL.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF answer,
                 german_yes   TYPE char1 VALUE 'J',
                 english_yes  TYPE char1 VALUE 'Y',
                 first_button TYPE char1 VALUE '1',
               END OF answer.

    CONSTANTS: BEGIN OF icon_name,
                 okay   TYPE iconname VALUE 'ICON_OKAY',
                 cancel TYPE iconname VALUE 'ICON_CANCEL',
               END OF icon_name.
ENDCLASS.


CLASS ycl_addict_gui_toolkit IMPLEMENTATION.
  METHOD are_you_sure.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Asks the user if he/she wants to continue
    " Raises exception on rejection
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA user_answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING  titlebar              = TEXT-608
                 text_question         = text
                 text_button_1         = TEXT-105
                 icon_button_1         = ycl_addict_gui_toolkit=>icon_name-okay
                 text_button_2         = TEXT-106
                 icon_button_2         = ycl_addict_gui_toolkit=>icon_name-cancel
                 display_cancel_button = abap_false
      IMPORTING  answer                = user_answer
      EXCEPTIONS text_not_found        = 1
                 OTHERS                = 2.

    ASSERT sy-subrc = 0.

    IF user_answer <> ycl_addict_gui_toolkit=>answer-first_button.
      RAISE EXCEPTION NEW ycx_addict_user_input( textid = ycx_addict_user_input=>user_cancelled ).
    ENDIF.
  ENDMETHOD.

  METHOD display_transport_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Displays transport requests in standard TCode
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK requests IS NOT INITIAL.

    DATA(displayable_requests) = ycl_addict_transport_request=>get_request_list(
                                     VALUE #( srch_strkorr = abap_true
                                              trkorr_rng   = VALUE #( FOR _request IN requests
                                                                      ( sign   = ycl_addict_toolkit=>sign-include
                                                                        option = ycl_addict_toolkit=>option-eq
                                                                        low    = _request ) ) ) ).

    DATA(wd_requests) = VALUE strhi_requests_wd( ).

    LOOP AT displayable_requests ASSIGNING FIELD-SYMBOL(<request>).
      TRY.
          DATA(request_obj) = ycl_addict_transport_request=>get_instance( <request>-trkorr ).
          DATA(content) = request_obj->get_content( ).

          APPEND VALUE #( h              = CORRESPONDING #( request_obj->get_header( ) )
                          objects        = CORRESPONDING #( content-objects )
                          keys           = CORRESPONDING #( content-keys )
                          objects_filled = abap_true )
                 TO wd_requests.

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    CALL FUNCTION 'TRINT_DISPLAY_REQUESTS'
      EXPORTING it_requests        = wd_requests
                iv_first_node_text = TEXT-013.
  ENDMETHOD.

  METHOD is_gui_on.
    DATA gui_on TYPE answer.

    CALL FUNCTION 'RFC_IS_GUI_ON'
      IMPORTING on = gui_on.

    result = xsdbool( gui_on = answer-english_yes ).
  ENDMETHOD.

  METHOD report_fatal_error.
    CASE sy-batch.
      WHEN abap_true.
        DATA(deep_error) = ycl_addict_class=>get_deepest_exception( error ).
        MESSAGE deep_error TYPE ycl_simbal=>msgty-abort.
      WHEN abap_false.
        ycl_simbal_gui=>display_cx_msg_popup( error ).
    ENDCASE.
  ENDMETHOD.

  METHOD popup_strings.
    CHECK strings IS NOT INITIAL.

    " Attempt """""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING  funcname           = 'POPUP_DISPLAY_STRINGS'
      EXCEPTIONS function_not_exist = 1
                 OTHERS             = 2.

    IF sy-subrc = 0.
      DATA(p_title) = CONV string( title ).
      DATA(p_column_header) = CONV string( header ).

      CALL FUNCTION 'POPUP_DISPLAY_STRINGS'
        EXPORTING p_tab           = strings
                  p_title         = p_title
                  p_column_header = p_column_header.

      RETURN.
    ENDIF.

    " Attempt """""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING  funcname           = 'POPUP_TO_DISPLAY_STRING'
      EXCEPTIONS function_not_exist = 1
                 OTHERS             = 2.

    IF sy-subrc = 0.
      DATA(iv_title) = CONV char35( COND string( WHEN title IS NOT INITIAL  THEN title
                                                 WHEN header IS NOT INITIAL THEN header
                                                 ELSE                            space ) ).

      DATA(iv_string) = REDUCE string( INIT _out TYPE string
                                       FOR _str IN strings
                                       NEXT _out = COND #( WHEN _out IS INITIAL
                                                           THEN _str
                                                           ELSE |{ _out } { _str }| ) ).

      CALL FUNCTION 'POPUP_TO_DISPLAY_STRING'
        EXPORTING iv_title  = iv_title
                  iv_string = iv_string.

      RETURN.
    ENDIF.

    " Fallback """"""""""""""""""""""""""""""""""""""""""""""""""""""
    MESSAGE |{ CONV char50( strings[ 1 ] ) } (...)| TYPE ycl_simbal=>msgty-info.
  ENDMETHOD.
ENDCLASS.
