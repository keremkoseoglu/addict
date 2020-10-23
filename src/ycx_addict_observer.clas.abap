class YCX_ADDICT_OBSERVER definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of YCX_ADDICT_OBSERVER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '350',
      attr1 type scx_attrname value 'HOST',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of YCX_ADDICT_OBSERVER .
  constants:
    begin of OBSERVER_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '349',
      attr1 type scx_attrname value 'HOST',
      attr2 type scx_attrname value 'OBSERVER',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBSERVER_ERROR .
  data HOST type SEOCLSNAME .
  data OBSERVER type SEOCLSNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !HOST type SEOCLSNAME optional
      !OBSERVER type SEOCLSNAME optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_OBSERVER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->HOST = HOST .
me->OBSERVER = OBSERVER .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = YCX_ADDICT_OBSERVER .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
