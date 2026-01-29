class YCX_ADDICT_SAP_USER definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of USER_UNKNOWN,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'UNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of USER_UNKNOWN .
  constants:
    begin of USER_INACTIVE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'UNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of USER_INACTIVE .
  data UNAME type SYUNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !UNAME type SYUNAME optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_SAP_USER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->UNAME = UNAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
