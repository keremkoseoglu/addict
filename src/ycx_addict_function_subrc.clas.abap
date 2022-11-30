CLASS ycx_addict_function_subrc DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF ycx_addict_function_subrc,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '324',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ycx_addict_function_subrc .
    CONSTANTS:
      BEGIN OF subrc_error,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE 'PARAM',
        attr4 TYPE scx_attrname VALUE 'STEXT',
      END OF subrc_error .
    CONSTANTS:
      BEGIN OF function_returned_error,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '382',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error .
    CONSTANTS:
      BEGIN OF function_returned_error_txt,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '456',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error_txt .
    CONSTANTS:
      BEGIN OF no_result_returned,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '584',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'STEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_result_returned .
    DATA funcname TYPE funct-funcname .
    DATA subrc TYPE sysubrc .
    DATA param TYPE funct-parameter .
    DATA stext TYPE funct-stext .
    DATA error_text TYPE string .

    CLASS-METHODS raise_if_sysubrc_not_initial
      IMPORTING
        !funcname TYPE funct-funcname
      RAISING
        ycx_addict_function_subrc .

    METHODS constructor
      IMPORTING
        !textid     LIKE if_t100_message=>t100key OPTIONAL
        !previous   LIKE previous OPTIONAL
        !funcname   TYPE funct-funcname OPTIONAL
        !subrc      TYPE sysubrc OPTIONAL
        !param      TYPE funct-parameter OPTIONAL
        !stext      TYPE funct-stext OPTIONAL
        !error_text TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCX_ADDICT_FUNCTION_SUBRC IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->funcname = funcname .
    me->subrc = subrc .
    me->param = param .
    me->stext = stext .
    me->error_text = error_text .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = ycx_addict_function_subrc .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_if_sysubrc_not_initial.

    CHECK sy-subrc IS NOT INITIAL.

    DATA(subrc_bak) = sy-subrc.

    SELECT SINGLE parameter FROM fupararef
           WHERE funcname  = @funcname AND
                 paramtype = @abap_true AND
                 pposition = @subrc_bak
           INTO @DATA(parameter).

    SELECT SINGLE stext FROM funct
           WHERE spras     = @sy-langu  AND
                 funcname  = @funcname  AND
                 parameter = @parameter AND
                 kind      = @abap_true
           INTO @DATA(stext).

    IF sy-subrc <> 0.
      SELECT SINGLE stext FROM funct
             WHERE funcname  = @funcname  AND
                   parameter = @parameter AND
                   kind      = @abap_true
             INTO @stext.
    ENDIF.

    RAISE EXCEPTION TYPE ycx_addict_function_subrc
      EXPORTING
        funcname = funcname
        param    = parameter
        stext    = stext
        subrc    = subrc_bak
        textid   = ycx_addict_function_subrc=>subrc_error.
  ENDMETHOD.
ENDCLASS.
