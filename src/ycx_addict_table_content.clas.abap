class YCX_ADDICT_TABLE_CONTENT definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of NO_ENTRY_FOR_OBJECTID,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_ENTRY_FOR_OBJECTID .
  constants:
    begin of ENTRY_FIELD_EMPTY,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value 'FIELDNAME',
      attr4 type scx_attrname value '',
    end of ENTRY_FIELD_EMPTY .
  constants:
    begin of INVALID_ENTRY,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_ENTRY .
  constants:
    begin of COLUMN_VALUES_DUPLICATE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '188',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'FIELDNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of COLUMN_VALUES_DUPLICATE .
  constants:
    begin of VALUE_INVALID,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value 'OBJECTID',
      attr3 type scx_attrname value 'FIELDNAME',
      attr4 type scx_attrname value '',
    end of VALUE_INVALID .
  constants:
    begin of TABLE_EMPTY,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '179',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TABLE_EMPTY .
  constants:
    begin of MULTIPLE_ENTRIES,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'TABNAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MULTIPLE_ENTRIES .
  data TABNAME type TABNAME .
  data OBJECTID type CDOBJECTV .
  data FIELDNAME type FIELDNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TABNAME type TABNAME optional
      !OBJECTID type CDOBJECTV optional
      !FIELDNAME type FIELDNAME optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TABLE_CONTENT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TABNAME = TABNAME .
me->OBJECTID = OBJECTID .
me->FIELDNAME = FIELDNAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
