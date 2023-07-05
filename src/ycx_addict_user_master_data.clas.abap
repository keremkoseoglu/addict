CLASS ycx_addict_user_master_data DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF user_unknown,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '095',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_unknown.
    CONSTANTS:
      BEGIN OF user_inactive,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '096',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_inactive.
    CONSTANTS:
      BEGIN OF email_missing,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '021',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF email_missing.
    CONSTANTS:
      BEGIN OF user_email_nomatch,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '125',
        attr1 TYPE scx_attrname VALUE 'EMAIL',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF user_email_nomatch.
    CONSTANTS:
      BEGIN OF mobile_missing,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '407',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mobile_missing.
    CONSTANTS:
      BEGIN OF spool_param_not_ok_for_job,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '647',
        attr1 TYPE scx_attrname VALUE 'UNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF spool_param_not_ok_for_job.

    DATA uname TYPE syuname.
    DATA email TYPE ad_smtpadr.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                !uname    TYPE syuname                  OPTIONAL
                email     TYPE ad_smtpadr               OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycx_addict_user_master_data IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->uname = uname.
    me->email = email.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
