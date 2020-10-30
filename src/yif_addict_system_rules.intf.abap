INTERFACE yif_addict_system_rules
  PUBLIC .

  TYPES: BEGIN OF ticket_key_dict,
           ticsy_id  TYPE yd_addict_ticsy_id,
           ticket_id TYPE yd_addict_ticket_id,
         END OF ticket_key_dict,

         ticket_key_list TYPE STANDARD TABLE OF ticket_key_dict WITH EMPTY KEY.

  TYPES ticket_id_list TYPE STANDARD TABLE OF yd_addict_ticket_id WITH EMPTY KEY.
  TYPES trkorr_list TYPE STANDARD TABLE OF trkorr WITH EMPTY KEY.

  CONSTANTS: BEGIN OF class,
               me TYPE seoclsname VALUE 'YIF_ADDICT_SYSTEM_RULES',
             END OF class.

  METHODS get_requests_of_tickets
    IMPORTING !ticketing_system TYPE yd_addict_ticsy_id OPTIONAL
              !tickets          TYPE ticket_id_list
    RETURNING VALUE(requests)   TYPE trkorr_list.

  METHODS get_ticket_key_of_request
    IMPORTING !trkorr           TYPE trkorr
    RETURNING VALUE(ticket_key) TYPE ticket_key_dict.

  METHODS is_request_toc_safe
    IMPORTING !trkorr     TYPE trkorr
    RETURNING VALUE(safe) TYPE abap_bool.
ENDINTERFACE.
