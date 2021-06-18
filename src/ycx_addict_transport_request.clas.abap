class YCX_ADDICT_TRANSPORT_REQUEST definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of MUST_BE_MANUALLY_IMPORTED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value 'SYSID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MUST_BE_MANUALLY_IMPORTED .
  data TRKORR type TRKORR .
  data SYSID type SYSYSID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional
      !SYSID type SYSYSID optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TRANSPORT_REQUEST IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
me->SYSID = SYSID .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
