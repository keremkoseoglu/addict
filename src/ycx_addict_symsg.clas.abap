CLASS ycx_addict_symsg DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    DATA ms_symsg TYPE symsg READ-ONLY.

    CLASS-METHODS get_instance RETURNING VALUE(ro_cx) TYPE REF TO ycx_addict_symsg.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                ms_symsg  TYPE symsg                    OPTIONAL.

    METHODS init_by_symsg.

    METHODS raise_symsg
      EXCEPTIONS symsg.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycx_addict_symsg IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->ms_symsg = ms_symsg.
    CLEAR me->textid.
    IF textid IS INITIAL AND me->if_t100_message~t100key IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    ro_cx = NEW #( ).
    ro_cx->init_by_symsg( ).
  ENDMETHOD.

  METHOD init_by_symsg.
    " BODY
    mac_msg_set_from_symsg ms_symsg.

    if_t100_message~t100key-msgid = ms_symsg-msgid.
    if_t100_message~t100key-msgno = ms_symsg-msgno.
    if_t100_message~t100key-attr1 = ms_symsg-msgv1.
    if_t100_message~t100key-attr2 = ms_symsg-msgv2.
    if_t100_message~t100key-attr3 = ms_symsg-msgv3.
    if_t100_message~t100key-attr4 = ms_symsg-msgv4.
  ENDMETHOD.

  METHOD raise_symsg.
    " BODY
    mac_symsg_set_from_msg ms_symsg.
    mac_symsg_raise symsg.
  ENDMETHOD.
ENDCLASS.
