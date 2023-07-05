CLASS ycl_addict_spool_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS conv_spool_to_pdf
      IMPORTING spoolid TYPE rspoid
                partnum TYPE adsnum DEFAULT 1
      EXPORTING pdf     TYPE fpcontent
                solix   TYPE solix_tab
      RAISING   ycx_addict_spool_to_pdf.

    CLASS-METHODS print_option_get_for_pdf
      EXPORTING control_param  TYPE ssfctrlop
                composer_param TYPE ssfcompop.

    CLASS-METHODS ssf_name
      IMPORTING formname      TYPE tdsfname
      RETURNING VALUE(result) TYPE rs38l_fnam
      RAISING   ycx_addict_function_subrc.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_addict_spool_toolkit IMPLEMENTATION.
  METHOD conv_spool_to_pdf.
    CALL FUNCTION 'FPCOMP_CREATE_PDF_FROM_SPOOL'
      EXPORTING  i_spoolid      = spoolid
                 i_partnum      = partnum
      IMPORTING  e_pdf          = pdf
      EXCEPTIONS ads_error      = 1
                 usage_error    = 2
                 system_error   = 3
                 internal_error = 4
                 OTHERS         = 5.

    IF sy-subrc <> 0.
      DATA(symsg_error) = ycx_addict_symsg=>get_instance( ).
      RAISE EXCEPTION NEW ycx_addict_spool_to_pdf( partnum  = partnum
                                                   previous = symsg_error
                                                   spoolid  = spoolid
                                                   textid   = ycx_addict_spool_to_pdf=>conv_error ).
    ENDIF.

    CHECK solix IS REQUESTED.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer     = pdf
      TABLES    binary_tab = solix.
  ENDMETHOD.

  METHOD print_option_get_for_pdf.
    control_param-getotf    = abap_true.
    control_param-no_dialog = abap_true.
    control_param-preview   = abap_true.
    control_param-device    = 'PRINTER'.
    composer_param-tddest   = 'LP01'.
    composer_param-tdnoprev = ''.
  ENDMETHOD.

  METHOD ssf_name.
    ##FM_SUBRC_OK
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING  formname           = formname
      IMPORTING  fm_name            = result
      EXCEPTIONS no_form            = 1
                 no_function_module = 2
                 OTHERS             = 3.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'SSF_FUNCTION_MODULE_NAME' ).
  ENDMETHOD.
ENDCLASS.
