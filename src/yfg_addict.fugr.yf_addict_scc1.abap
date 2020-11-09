FUNCTION YF_ADDICT_SCC1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRKORR) TYPE  TRKORR
*"     VALUE(SRC_CLIENT) TYPE  SYMANDT
*"  EXCEPTIONS
*"      SCC1_ERROR
*"----------------------------------------------------------------------
  DATA(subrc) = CONV sysubrc( 0 ) ##OPERATOR.

  CALL FUNCTION 'SCCR_PERFORM_SCC1'
    EXPORTING
      ccsupcopy    = abap_false
      cccomfile    = trkorr
      quellmandant = src_client
      incl_task    = abap_true
      insert_only  = abap_false
    IMPORTING
      rcod         = subrc.

  IF subrc IS NOT INITIAL.
    RAISE scc1_error.
  ENDIF.
ENDFUNCTION.
