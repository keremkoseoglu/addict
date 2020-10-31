class YCX_ADDICT_CLASS_METHOD definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of CALL_AFTER_OPERATION,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '147',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CALL_AFTER_OPERATION .
  constants:
    begin of ALREADY_CALLED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '247',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ALREADY_CALLED .
  constants:
    begin of MISSING_FIELD_VALUE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '586',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value 'FNAME',
      attr4 type scx_attrname value '',
    end of MISSING_FIELD_VALUE .
  constants:
    begin of ERROR_WITH_TEXT,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '540',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value 'ERROR_TEXT',
      attr4 type scx_attrname value '',
    end of ERROR_WITH_TEXT .
  constants:
    begin of UNEXPECTED_FIELD_VALUE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '387',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value 'FNAME',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_FIELD_VALUE .
  constants:
    begin of AUTHORIZATION,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '358',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of AUTHORIZATION .
  constants:
    begin of OBLIG_FIELDS_INITIAL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '327',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBLIG_FIELDS_INITIAL .
  constants:
    begin of INCORRECT_LINES,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '301',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INCORRECT_LINES .
  constants:
    begin of CALL_BEFORE_OPERATION,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '186',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CALL_BEFORE_OPERATION .
  constants:
    begin of UNEXPECTED_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '135',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNEXPECTED_ERROR .
  constants:
    begin of UNIMPLEMENTED_FEATURE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'CLASS',
      attr2 type scx_attrname value 'METHOD',
      attr3 type scx_attrname value 'FNAME',
      attr4 type scx_attrname value '',
    end of UNIMPLEMENTED_FEATURE .
  data CLASS type SEOCLSNAME .
  data METHOD type SEOCPDNAME .
  data FNAME type FIELDNAME .
  data ERROR_TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CLASS type SEOCLSNAME optional
      !METHOD type SEOCPDNAME optional
      !FNAME type FIELDNAME optional
      !ERROR_TEXT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_CLASS_METHOD IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->CLASS = CLASS .
me->METHOD = METHOD .
me->FNAME = FNAME .
me->ERROR_TEXT = ERROR_TEXT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
