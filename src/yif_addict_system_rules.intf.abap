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

  CONSTANTS: BEGIN OF method,
               get_requests_of_tickets     TYPE seocpdname VALUE 'GET_REQUESTS_OF_TICKETS',
               get_ticket_key_of_request   TYPE seocpdname VALUE 'GET_TICKET_KEY_OF_REQUEST',
               get_request_related_tickets TYPE seocpdname VALUE 'GET_REQUEST_RELATED_TICKETS',
               is_request_toc_safe         TYPE seocpdname VALUE 'IS_REQUEST_TOC_SAFE',
             END OF method.

  METHODS get_requests_of_tickets
    IMPORTING !ticket_keys    TYPE ticket_key_list
    RETURNING VALUE(requests) TYPE trkorr_list
    RAISING   ycx_addict_class_method.

  METHODS get_ticket_key_of_request
    IMPORTING !trkorr           TYPE trkorr
    RETURNING VALUE(ticket_key) TYPE ticket_key_dict
    RAISING   ycx_addict_class_method.

  METHODS get_request_related_tickets
    IMPORTING !ticketing_system      TYPE REF TO yif_addict_ticketing_system
              !tickets               TYPE ticket_id_list
    RETURNING VALUE(related_tickets) TYPE ticket_id_list
    RAISING   ycx_addict_class_method.

  METHODS is_request_toc_safe
    IMPORTING !trkorr     TYPE trkorr
    RETURNING VALUE(safe) TYPE abap_bool
    RAISING   ycx_addict_class_method.
ENDINTERFACE.
