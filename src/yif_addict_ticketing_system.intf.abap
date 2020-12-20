INTERFACE yif_addict_ticketing_system PUBLIC.

  TYPES string_list TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  TYPES tcode_list TYPE STANDARD TABLE OF tcode WITH KEY table_line.
  TYPES ticket_id_list TYPE STANDARD TABLE OF yd_addict_ticket_id WITH KEY table_line.
  TYPES status_id_list TYPE STANDARD TABLE OF yd_addict_ticket_status_id WITH KEY table_line.
  types type_id_list type standard table of yd_addict_ticket_type_id with key table_line.

  TYPES: BEGIN OF status_dict,
           status_id   TYPE yd_addict_ticket_status_id,
           status_text TYPE yd_addict_ticket_status_text,
         END OF status_dict.

  TYPES: BEGIN OF ticket_status_dict,
           ticket_id TYPE yd_addict_ticket_id,
           status_id TYPE yd_addict_ticket_status_id,
         END OF ticket_status_dict,

         ticket_status_list TYPE STANDARD TABLE OF ticket_status_dict WITH KEY ticket_id.

  CONSTANTS: BEGIN OF class,
               me TYPE seoclsname VALUE 'YIF_ADDICT_TICKETING_SYSTEM',
             END OF class..

  METHODS is_ticket_id_valid
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(output) TYPE abap_bool
    RAISING   ycx_addict_ticketing_system.

  METHODS get_ticket_header
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(output) TYPE ysaddict_ticket_header
    RAISING   ycx_addict_ticketing_system.

  METHODS get_transport_instructions
    IMPORTING !ticket_id          TYPE yd_addict_ticket_id
    RETURNING VALUE(instructions) TYPE string_list
    RAISING   ycx_addict_ticketing_system.

  METHODS get_sub_tickets
    IMPORTING !parent         TYPE yd_addict_ticket_id
    RETURNING VALUE(children) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.

  METHODS get_linked_tickets
    IMPORTING !ticket_id     TYPE yd_addict_ticket_id
    RETURNING VALUE(tickets) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.

  METHODS get_related_tcodes
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(tcodes) TYPE tcode_list
    RAISING   ycx_addict_ticketing_system.

  METHODS get_tickets_related_to_tcodes
    IMPORTING !tcodes        TYPE tcode_list
    RETURNING VALUE(tickets) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.

  METHODS get_earliest_status
    IMPORTING !statuses       TYPE status_id_list
    RETURNING VALUE(earliest) TYPE status_dict
    RAISING   ycx_addict_ticketing_system.

  METHODS get_tickets_with_status
    IMPORTING !statuses      TYPE status_id_list
              !types         type type_id_list optional
    RETURNING VALUE(tickets) TYPE ticket_status_list
    RAISING   ycx_addict_ticketing_system.

  METHODS set_ticket_status
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              !status_id TYPE yd_addict_ticket_status_id
    RAISING   ycx_addict_ticketing_system.

  METHODS set_ticket_assignee
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              !assignee  TYPE clike
    RAISING   ycx_addict_ticketing_system.

  METHODS set_ticket_assignee_for_status
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              !status_id TYPE yd_addict_ticket_status_id
    RAISING   ycx_addict_ticketing_system.

  METHODS display_ticket
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
    RAISING   ycx_addict_ticketing_system.
ENDINTERFACE.
