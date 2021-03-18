class YCX_ADDICT_UNDEFINED_STATUS_CH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of TICKET_CANT_BE_SET,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'TICKET_ID',
      attr2 type scx_attrname value 'STATUS_ID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TICKET_CANT_BE_SET .
  data TICKET_ID type YD_ADDICT_TICKET_ID .
  data STATUS_ID type YD_ADDICT_TICKET_STATUS_ID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TICKET_ID type YD_ADDICT_TICKET_ID optional
      !STATUS_ID type YD_ADDICT_TICKET_STATUS_ID optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_UNDEFINED_STATUS_CH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TICKET_ID = TICKET_ID .
me->STATUS_ID = STATUS_ID .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
