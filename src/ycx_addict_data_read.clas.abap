class YCX_ADDICT_DATA_READ definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of CANT_READ,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_READ .
  constants:
    begin of CANT_READ_DATA_TYPE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '470',
      attr1 type scx_attrname value 'OBJECTID',
      attr2 type scx_attrname value 'DATA_TYPE',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_READ_DATA_TYPE .
  constants:
    begin of CANT_READ_PARCEL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '077',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_READ_PARCEL .
  constants:
    begin of NO_DATA_FOUND,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '460',
      attr1 type scx_attrname value 'OBJECTID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_DATA_FOUND .
  constants:
    begin of CANT_READ_QUEUE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '051',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_READ_QUEUE .
  data OBJECTID type CDOBJECTV .
  data DATA_TYPE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJECTID type CDOBJECTV optional
      !DATA_TYPE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_DATA_READ IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->OBJECTID = OBJECTID .
me->DATA_TYPE = DATA_TYPE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
