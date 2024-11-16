class YCX_ADDICT_TRANS_REQ_RELEASE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of CTS_INITIALIZATION_FAILURE,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '025',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CTS_INITIALIZATION_FAILURE .
  constants:
    begin of ENQUEUE_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ENQUEUE_FAILED .
  constants:
    begin of NO_AUTHORIZATION,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_AUTHORIZATION .
  constants:
    begin of INVALID_REQUEST,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '028',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_REQUEST .
  constants:
    begin of ERROR_IN_EXPORT_METHODS,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '030',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_IN_EXPORT_METHODS .
  constants:
    begin of OBJECT_CHECK_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '031',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECT_CHECK_ERROR .
  constants:
    begin of DOCU_MISSING,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '032',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DOCU_MISSING .
  constants:
    begin of DB_ACCESS_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '033',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DB_ACCESS_ERROR .
  constants:
    begin of ACTION_ABORTED_BY_USER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '034',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ACTION_ABORTED_BY_USER .
  constants:
    begin of EXPORT_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '035',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of EXPORT_FAILED .
  constants:
    begin of RELEASE_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '036',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RELEASE_FAILED .
  constants:
    begin of REPEAT_TOO_EARLY,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '029',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REPEAT_TOO_EARLY .
  constants:
    begin of REQUEST_NOT_FOUND,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '037',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REQUEST_NOT_FOUND .
  constants:
    begin of EMPTY_TASK_DEL_FAIL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '038',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of EMPTY_TASK_DEL_FAIL .
  constants:
    begin of SUBTASK_READ_FAIL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '039',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SUBTASK_READ_FAIL .
  data TRKORR type TRKORR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TRANS_REQ_RELEASE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
