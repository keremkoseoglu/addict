class YCX_ADDICT_TRANS_REQ_IMPORT definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of CANCELLED_BY_USER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '040',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANCELLED_BY_USER .
  constants:
    begin of IMPORT_REQUEST_DENIED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '041',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IMPORT_REQUEST_DENIED .
  constants:
    begin of IMPORT_REQUEST_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '042',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IMPORT_REQUEST_FAILED .
  constants:
    begin of READ_CONFIG_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '043',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of READ_CONFIG_FAILED .
  constants:
    begin of TABLE_OF_REQUESTS_IS_EMPTY,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '044',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TABLE_OF_REQUESTS_IS_EMPTY .
  constants:
    begin of IMPORT_FAILED_WITH_REASON,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '045',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value 'CITEXT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IMPORT_FAILED_WITH_REASON .
  constants:
    begin of IMPORT_TEST_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '046',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IMPORT_TEST_FAILED .
  constants:
    begin of IMPORT_REQUEST_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '047',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IMPORT_REQUEST_ERROR .
  constants:
    begin of PARAMETER_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '048',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAMETER_ERROR .
  constants:
    begin of TRANSMIT_TR_QUEUE_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '049',
      attr1 type scx_attrname value 'SYSNAM',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TRANSMIT_TR_QUEUE_FAILED .
  data SYSNAM type TMSSYSNAM .
  data CITEXT type CITEXT .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !SYSNAM type TMSSYSNAM optional
      !CITEXT type CITEXT optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TRANS_REQ_IMPORT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->SYSNAM = SYSNAM .
me->CITEXT = CITEXT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
