class YCX_ADDICT_METHOD_PARAMETER definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of PARAM_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAM_ERROR .
  constants:
    begin of CANT_CALL_WITHOUT_PARAM,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '328',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_CALL_WITHOUT_PARAM .
  constants:
    begin of PARAM_NOT_CACHED_YET,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '440',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of PARAM_NOT_CACHED_YET .
  constants:
    begin of NO_ORDER_ENABLED_MAT,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '915',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_ORDER_ENABLED_MAT .
  constants:
    begin of FIELD_MISSING_IN_STR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '399',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value 'PARAM_NAME_2',
    end of FIELD_MISSING_IN_STR .
  constants:
    begin of RANGE_HAS_I_AND_E,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '386',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of RANGE_HAS_I_AND_E .
  constants:
    begin of RANGE_CAN_ONLY_HAVE_SINGULAR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '385',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value 'PARAM_NAME_2',
    end of RANGE_CAN_ONLY_HAVE_SINGULAR .
  constants:
    begin of PARAM_MISSING,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '148',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of PARAM_MISSING .
  constants:
    begin of PARAM_PAIR_INCONSISTENT,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '423',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value 'PARAM_NAME_2',
    end of PARAM_PAIR_INCONSISTENT .
  constants:
    begin of PARAM_VALUE_INITIAL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '421',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of PARAM_VALUE_INITIAL .
  constants:
    begin of PARAM_VALUE_INVALID,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '180',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value 'METHOD_NAME',
      attr3 type scx_attrname value 'PARAM_NAME',
      attr4 type scx_attrname value '',
    end of PARAM_VALUE_INVALID .
  data CLASS_NAME type SEOCLSNAME .
  data METHOD_NAME type SEOCPDNAME .
  data PARAM_NAME type SEOCPDNAME .
  data PARAM_NAME_2 type SEOCPDNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CLASS_NAME type SEOCLSNAME optional
      !METHOD_NAME type SEOCPDNAME optional
      !PARAM_NAME type SEOCPDNAME optional
      !PARAM_NAME_2 type SEOCPDNAME optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_METHOD_PARAMETER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->CLASS_NAME = CLASS_NAME .
me->METHOD_NAME = METHOD_NAME .
me->PARAM_NAME = PARAM_NAME .
me->PARAM_NAME_2 = PARAM_NAME_2 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
