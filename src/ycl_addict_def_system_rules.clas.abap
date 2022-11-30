CLASS ycl_addict_def_system_rules DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_system_rules.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DEF_SYSTEM_RULES IMPLEMENTATION.


  METHOD yif_addict_system_rules~get_requests_of_tickets.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the requests of the given ticket.
    " Assumption here: Request text starts with ticket number.
    " Sample format:
    " VOL-12345 - Explanation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(request_details) =
      ycl_addict_transport_request=>get_request_list(
          VALUE #( as4text_rng  = VALUE #( FOR _ticket_key IN ticket_keys
                                           ( option = ycl_addict_toolkit=>option-cp
                                             sign   = ycl_addict_toolkit=>sign-include
                                             low    = |{ _ticket_key-ticket_id } - *| ) )
                   srch_strkorr = abap_true ) ).

    requests = VALUE #( FOR _rd IN request_details
                        WHERE ( strkorr IS INITIAL )
                        ( _rd-trkorr ) ).
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
