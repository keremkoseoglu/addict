CLASS ycl_addict_def_system_rules DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_system_rules.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_addict_def_system_rules IMPLEMENTATION.
  METHOD yif_addict_system_rules~get_requests_of_tickets.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the requests of the given ticket.
    " Assumption here: Request text starts with ticket number.
    " Sample format:
    " VOL-12345 - Explanation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    requests = CORRESPONDING #(
        ycl_addict_transport_request=>get_request_list(
          param = VALUE #(
            as4text_rng = VALUE #( FOR _ticket_id IN tickets (
                option = ycl_addict_toolkit=>option-cp
                sign   = ycl_addict_toolkit=>sign-include
                low    = |{ _ticket_id }*| ) )
            srch_strkorr = abap_true ) ) ).
  ENDMETHOD.

ENDCLASS.
