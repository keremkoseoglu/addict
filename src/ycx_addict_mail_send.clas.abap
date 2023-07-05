CLASS ycx_addict_mail_send DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF cant_send,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '020',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cant_send.
    CONSTANTS:
      BEGIN OF cant_send_noitem,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '334',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cant_send_noitem.
    CONSTANTS:
      BEGIN OF column_number_not_valid,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '052',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF column_number_not_valid.
    CONSTANTS:
      BEGIN OF couldnt_send_all_mails,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '639',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF couldnt_send_all_mails.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycx_addict_mail_send IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
