class YCX_ADDICT_TRANS_REQ_DELETE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of FILE_ACCESS_ERROR,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '053',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FILE_ACCESS_ERROR .
  constants:
    begin of ORDER_ALREADY_RELEASED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '054',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_ALREADY_RELEASED .
  constants:
    begin of ORDER_CONTAINS_C_MEMBER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '055',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_CONTAINS_C_MEMBER .
  constants:
    begin of ORDER_CONTAINS_LOCKED_ENTRIES,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '056',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_CONTAINS_LOCKED_ENTRIES .
  constants:
    begin of ORDER_IS_REFERED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '057',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_IS_REFERED .
  constants:
    begin of REPAIR_ORDER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '058',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REPAIR_ORDER .
  constants:
    begin of USER_NOT_OWNER,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '059',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of USER_NOT_OWNER .
  constants:
    begin of DELETE_WAS_CANCELLED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '060',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DELETE_WAS_CANCELLED .
  constants:
    begin of OBJECTS_FREE_BUT_STILL_LOCKS,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '061',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECTS_FREE_BUT_STILL_LOCKS .
  constants:
    begin of ORDER_LOCK_FAILED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '062',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value 'BNAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_LOCK_FAILED .
  constants:
    begin of WRONG_CLIENT,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '063',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WRONG_CLIENT .
  constants:
    begin of PROJECT_STILL_REFERENCED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '064',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROJECT_STILL_REFERENCED .
  constants:
    begin of SUCCESSORS_ALREADY_RELEASED,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '065',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SUCCESSORS_ALREADY_RELEASED .
  constants:
    begin of REQUEST_DELETE_FAIL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '066',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REQUEST_DELETE_FAIL .
  constants:
    begin of CANT_FIND_EMP_SUBS_CANT_DEL,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '067',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_FIND_EMP_SUBS_CANT_DEL .
  data TRKORR type TRKORR .
  data BNAME type XUBNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional
      !BNAME type XUBNAME optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_TRANS_REQ_DELETE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
me->BNAME = BNAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
