CLASS ycl_addict_def_system_rules DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_system_rules.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES as4text_list TYPE STANDARD TABLE OF as4text WITH EMPTY KEY.

    TYPES: BEGIN OF trkorr_ticket_dict,
             trkorr     TYPE trkorr,
             ticket_key TYPE yif_addict_system_rules=>ticket_key_dict,
           END OF trkorr_ticket_dict,

           trkorr_ticket_set TYPE HASHED TABLE OF trkorr_ticket_dict
                             WITH UNIQUE KEY primary_key COMPONENTS trkorr.

    CONSTANTS as4text_separator TYPE char1 VALUE '-'.
    CLASS-DATA trkorr_ticket_cache TYPE trkorr_ticket_set.

    CLASS-METHODS extract_ticket_from_as4text
      IMPORTING !as4text TYPE as4text
      EXPORTING !ticket  TYPE yif_addict_system_rules=>ticket_key_dict
                !summary TYPE clike.
ENDCLASS.



CLASS ycl_addict_def_system_rules IMPLEMENTATION.
  METHOD extract_ticket_from_as4text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Extracts ticket number from transport request text
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA as4text_split TYPE as4text_list.
    CLEAR: ticket, summary.

    SPLIT as4text AT space INTO TABLE as4text_split.

    LOOP AT as4text_split ASSIGNING FIELD-SYMBOL(<split>).
      CASE sy-tabix.
        WHEN 1.
          ticket-ticket_id = <split>.

        WHEN OTHERS.
          CHECK <split> <> as4text_separator.

          summary = |{ summary }| &&
                    |{ COND #( WHEN summary IS NOT INITIAL THEN ` ` ) }| &&
                    |{ <split> }|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD yif_addict_system_rules~get_requests_of_tickets.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the requests of the given ticket.
    " Assumption here: Request text starts with ticket number.
    " Sample format:
    " VOL-12345 - Explanation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(request_details) = ycl_addict_transport_request=>get_request_list(
        VALUE #( as4text_rng = VALUE #( FOR _ticket_id IN tickets (
                   option = ycl_addict_toolkit=>option-cp
                   sign   = ycl_addict_toolkit=>sign-include
                   low    = |{ _ticket_id } - *| ) )
                 srch_strkorr = abap_true ) ).

    requests = VALUE #( FOR _rd IN request_details ( _rd-trkorr ) ).
  ENDMETHOD.


  METHOD yif_addict_system_rules~get_ticket_key_of_request.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the ticket ID of the request
    " Assumption here: Request text starts with ticket number.
    " Sample format:
    " VOL-12345 - Explanation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_def_system_rules=>trkorr_ticket_cache[
             KEY primary_key
             COMPONENTS trkorr = trkorr
           ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(cache) = VALUE trkorr_ticket_dict( trkorr = trkorr ).
      DATA(as4text) = ycl_addict_transport_request=>get_as4text_safe( cache-trkorr ).

      TRY.
          extract_ticket_from_as4text(
            EXPORTING as4text = as4text
            IMPORTING ticket  = ticket_key ).

        CATCH cx_root ##no_handler .
      ENDTRY.

      INSERT cache INTO TABLE ycl_addict_def_system_rules=>trkorr_ticket_cache
             ASSIGNING <cache>.
    ENDIF.

    ticket_key = <cache>-ticket_key.
  ENDMETHOD.


  METHOD yif_addict_system_rules~is_request_toc_safe.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Is request ToC safe?
    " Here is the logic of this method:
    " Workbench request should be ToC safe without any problems.
    " However; assume the following scenario:
    "
    " Dev system, client 220: Customizing request is created
    " Dev system, client 100: ToC is created and
    "                         customizing from 220 is included
    " QA system, client 100: ToC is imported
    "
    " In that case; the ToC includes the data from client 100,
    " not 220. It might even be completely empty if the same
    " keys don't exist in dev 100. When you import that ToC into
    " the QA system, it will damage the data there - instead
    " of importing the data from 220.
    "
    " Therefore, customizing requests from other clients are not
    " ToC safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(request) = ycl_addict_transport_request=>get_instance( trkorr ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA(trf) = request->get_header( )-trfunction.

    safe = xsdbool( NOT (
        ( trf = ycl_addict_transport_request=>trfunction-cust OR
          trf = ycl_addict_transport_request=>trfunction-cust_task ) AND
        ( request->get_source_client( ) <> sy-mandt ) ) ).
  ENDMETHOD.
ENDCLASS.
