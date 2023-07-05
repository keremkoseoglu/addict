CLASS ycx_addict_spool_to_pdf DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    DATA: spoolid TYPE rspoid,
          partnum TYPE adsnum.

    CONSTANTS:
      BEGIN OF conv_error,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '105',
        attr1 TYPE scx_attrname VALUE 'SPOOLID',
        attr2 TYPE scx_attrname VALUE 'PARTNUM',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF conv_error.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                spoolid   TYPE rspoid                   OPTIONAL
                partnum   TYPE adsnum                   OPTIONAL.
ENDCLASS.


CLASS ycx_addict_spool_to_pdf IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->spoolid = spoolid.
    me->partnum = partnum.
  ENDMETHOD.
ENDCLASS.
