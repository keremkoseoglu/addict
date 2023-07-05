CLASS ycl_addict_se01_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS execute
      IMPORTING trkorr_rng  TYPE cts_organizer_tt_wd_request OPTIONAL
                as4user_rng TYPE ucon_user_range             OPTIONAL
      EXPORTING !list       TYPE ycl_addict_tpalog_reader=>output_list
      RAISING   ycx_addict_class_method.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF method,
                 execute TYPE seocpdname VALUE 'EXECUTE',
               END OF method.
ENDCLASS.


CLASS ycl_addict_se01_reader IMPLEMENTATION.
  METHOD execute.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of requests and their statuses
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        CLEAR list.
        DATA(req) = VALUE ytt_addict_trkorr_det( ).

        IF trkorr_rng IS INITIAL.
          req = ycl_addict_transport_request=>get_request_list(
                    VALUE #( trfunction_rng = ycl_addict_transport_request=>get_user_creatable_trf_rng( ) ) ).
        ELSE.

          APPEND LINES OF ycl_addict_transport_request=>get_request_list( VALUE #(
                                                                              trkorr_rng     = trkorr_rng
                                                                              as4user_rng    = as4user_rng
                                                                              trfunction_rng = ycl_addict_transport_request=>get_user_creatable_trf_rng( ) ) )
                 TO req.
        ENDIF.

        LOOP AT req ASSIGNING FIELD-SYMBOL(<req>).
          DATA(list_entry) = VALUE ycl_addict_tpalog_reader=>output_dict( trkorr     = <req>-trkorr
                                                                          as4text    = <req>-as4text
                                                                          as4user    = <req>-as4user
                                                                          trfunction = <req>-trfunction
                                                                          tarsystem  = <req>-tarsystem
                                                                          trdate     = <req>-as4date
                                                                          trtime     = <req>-as4time ).

          list_entry-status = SWITCH #( <req>-trstatus
                                        WHEN 'D' OR 'L'
                                        THEN ycl_addict_tpalog_reader=>status-open
                                        ELSE ycl_addict_tpalog_reader=>status-released ).

          APPEND list_entry TO list.
        ENDLOOP.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW ycx_addict_class_method( textid   = ycx_addict_class_method=>unexpected_error
                                                     previous = diaper
                                                     class    = CONV #( ycl_addict_class=>get_class_name( me ) )
                                                     method   = method-execute ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
