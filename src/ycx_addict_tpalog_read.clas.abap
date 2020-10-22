class YCX_ADDICT_TPALOG_READ definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of NO_SOURCE_REQUEST,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '277',
      attr1 type scx_attrname value 'RFCDEST',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_SOURCE_REQUEST .
  constants:
    begin of READ_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '320',
      attr1 type scx_attrname value 'RFCDEST',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of READ_ERROR .
  constants:
    begin of TPALOG_READ_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '323',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TPALOG_READ_ERROR .
  data RFCDEST type RFCDEST .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !RFCDEST type RFCDEST optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TPALOG_READ IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RFCDEST = RFCDEST .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
