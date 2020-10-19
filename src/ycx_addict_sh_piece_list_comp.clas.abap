class YCX_ADDICT_SH_PIECE_LIST_COMP definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of YCX_ADDICT_SH_PIECE_LIST_COMP,
      msgid type symsgid value 'YADDICT',
      msgno type symsgno value '902',
      attr1 type scx_attrname value 'TRKORR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of YCX_ADDICT_SH_PIECE_LIST_COMP .
  data TRKORR type TRKORR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TRKORR type TRKORR optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_ADDICT_SH_PIECE_LIST_COMP IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TRKORR = TRKORR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = YCX_ADDICT_SH_PIECE_LIST_COMP .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
