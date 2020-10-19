class YCX_ADDICT_BDC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of YCX_ADDICT_BDC,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '901',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of YCX_ADDICT_BDC .
  data BDCMSGCOLL type YTT_ADDICT_BDCMSGCOLL .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !BDCMSGCOLL type YTT_ADDICT_BDCMSGCOLL optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_BDC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->BDCMSGCOLL = BDCMSGCOLL .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = YCX_ADDICT_BDC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
