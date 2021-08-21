CLASS ycl_addict_alv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF form_dict,
             html_end_of_list TYPE slis_formname,
             pf_status        TYPE slis_formname,
             user_command     TYPE slis_formname,
           END OF form_dict.

    TYPES field_list TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY.

    TYPES: BEGIN OF build_fcat_input_dict,
             structure       TYPE tabname,
             itab_name       TYPE tabname,
             tech_fields     TYPE field_list,
             edit_fields     TYPE field_list,
             hotspot_fields  TYPE field_list,
             checkbox_fields TYPE field_list,
           END OF build_fcat_input_dict.

    CONSTANTS: BEGIN OF default_fields,
                 alvsl   TYPE fieldname     VALUE 'ALVSL',
                 celltab TYPE fieldname     VALUE 'CELLTAB',
               END OF default_fields.

    CONSTANTS: BEGIN OF default_forms,
                 html_end_of_list TYPE slis_formname VALUE 'HTML_END_OF_LIST',
                 pf_status        TYPE slis_formname VALUE 'PF_STATUS',
                 user_command     TYPE slis_formname VALUE 'USER_COMMAND',
               END OF default_forms.

    DATA cprog  TYPE sycprog.
    DATA itab   TYPE REF TO data.
    DATA forms  TYPE form_dict.
    DATA layout TYPE slis_layout_alv.
    DATA fcat   TYPE slis_t_fieldcat_alv.

    METHODS constructor
      IMPORTING !itab       TYPE REF TO data
                !cprog      TYPE sy-cprog OPTIONAL
                !forms      TYPE form_dict OPTIONAL
                !layout     TYPE slis_layout_alv OPTIONAL
                !fcat       TYPE slis_t_fieldcat_alv OPTIONAL
                !fcat_param TYPE build_fcat_input_dict OPTIONAL
      RAISING   ycx_addict_alv.

    METHODS build_fcat
      IMPORTING !input TYPE build_fcat_input_dict
      RAISING   ycx_addict_alv.

    METHODS show_grid RAISING ycx_addict_alv.
    METHODS show_list RAISING ycx_addict_alv.
    METHODS show RAISING ycx_addict_alv.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES field_range TYPE RANGE OF fieldname.
    CONSTANTS alv_save TYPE char1 VALUE 'A'.

    CLASS-METHODS conv_field_list_to_range
      IMPORTING !list        TYPE field_list
      RETURNING VALUE(range) TYPE field_range.

    METHODS set_fcat_from_itab RAISING ycx_addict_alv.

    METHODS set_fcat_from_itab_line
      IMPORTING !line TYPE REF TO data
      RAISING   ycx_addict_alv.
ENDCLASS.



CLASS ycl_addict_alv IMPLEMENTATION.
  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Object creation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->cprog = cprog.
    me->itab  = itab.
    me->forms = forms.

    IF fcat IS NOT INITIAL.
      me->fcat = fcat.
    ELSEIF fcat_param IS NOT INITIAL.
      build_fcat( fcat_param ).
    ELSE.
      TRY.
          set_fcat_from_itab( ).
        CATCH cx_root ##no_handler .
      ENDTRY.
    ENDIF.

    me->layout = COND #(
        WHEN layout IS SUPPLIED THEN layout
        ELSE VALUE #( zebra = abap_true
                      colwidth_optimize = abap_true ) ).
  ENDMETHOD.


  METHOD build_fcat.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Builds FCAT out of structure
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
          EXPORTING
            i_internal_tabname     = input-itab_name
            i_structure_name       = input-structure
          CHANGING
            ct_fieldcat            = me->fcat
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3 ##FM_SUBRC_OK.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_FIELDCATALOG_MERGE' ).

        IF input-edit_fields IS NOT INITIAL.
          DATA(edit_rng) = conv_field_list_to_range( input-edit_fields ).

          MODIFY me->fcat FROM VALUE #( edit = abap_true )
                 TRANSPORTING edit
                 WHERE fieldname IN edit_rng.
        ENDIF.

        IF input-tech_fields IS NOT INITIAL.
          DATA(tech_rng) = conv_field_list_to_range( input-tech_fields ).

          MODIFY me->fcat FROM VALUE #( tech = abap_true )
                 TRANSPORTING tech
                 WHERE fieldname IN tech_rng.
        ENDIF.

        IF input-hotspot_fields IS NOT INITIAL.
          DATA(hotspot_rng) = conv_field_list_to_range( input-hotspot_fields ).

          MODIFY me->fcat FROM VALUE #( hotspot = abap_true )
                 TRANSPORTING hotspot
                 WHERE fieldname IN hotspot_rng.
        ENDIF.

        IF input-checkbox_fields IS NOT INITIAL.
          DATA(checkbox_rng) = conv_field_list_to_range( input-checkbox_fields ).

          MODIFY me->fcat FROM VALUE #( checkbox = abap_true )
                 TRANSPORTING checkbox
                 WHERE fieldname IN checkbox_rng.
        ENDIF.

      CATCH ycx_addict_alv INTO DATA(alv_error).
        RAISE EXCEPTION alv_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_alv
          EXPORTING
            textid   = ycx_addict_alv=>fcat_creation_error
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD show_grid.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Display grid
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE STANDARD TABLE.

    TRY.
        ASSIGN me->itab->* TO <itab>.
        DATA(grid_settings) = VALUE lvc_s_glay( edt_cll_cb = ycl_addict_gui_toolkit=>is_gui_on( ) ).

        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            i_callback_program          = me->cprog
            i_callback_pf_status_set    = me->forms-pf_status
            i_callback_user_command     = me->forms-user_command
            i_callback_html_end_of_list = me->forms-html_end_of_list
            i_grid_settings             = grid_settings
            is_layout                   = me->layout
            it_fieldcat                 = me->fcat
            i_save                      = me->alv_save
          TABLES
            t_outtab                    = <itab>
          EXCEPTIONS
            program_error               = 1
            OTHERS                      = 2 ##FM_SUBRC_OK.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_GRID_DISPLAY' ).

      CATCH ycx_addict_alv INTO DATA(alv_error).
        RAISE EXCEPTION alv_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_alv
          EXPORTING
            textid   = ycx_addict_alv=>grid_error
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD show_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Display list
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE STANDARD TABLE.

    TRY.
        ASSIGN me->itab->* TO <itab>.

        CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
          EXPORTING
            i_callback_program       = me->cprog
            i_callback_pf_status_set = me->forms-pf_status
            i_callback_user_command  = me->forms-user_command
            is_layout                = me->layout
            it_fieldcat              = me->fcat
            i_save                   = me->alv_save
          TABLES
            t_outtab                 = <itab>
          EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2 ##FM_SUBRC_OK.

        ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'REUSE_ALV_LIST_DISPLAY' ).

      CATCH ycx_addict_alv INTO DATA(alv_error).
        RAISE EXCEPTION alv_error.
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_alv
          EXPORTING
            textid   = ycx_addict_alv=>grid_error
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD show.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Display grid or list, depending on SY-BATCH
    " Why? Grid produces an error in case the program is in the background
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CASE sy-batch.
      WHEN abap_true . show_list( ).
      WHEN abap_false. show_grid( ).
    ENDCASE.
  ENDMETHOD.


  METHOD conv_field_list_to_range.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Convert field list to field range
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    range = VALUE #(
        FOR _field IN list (
          sign   = ycl_addict_toolkit=>sign-include
          option = ycl_addict_toolkit=>option-eq
          low    = _field ) ).
  ENDMETHOD.


  METHOD set_fcat_from_itab.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sets the field catalog from the passed internal table
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE STANDARD TABLE.

    ASSIGN me->itab->* TO <itab>.

    IF <itab> IS INITIAL.
      DATA(itab_was_initial) = abap_true.
      APPEND INITIAL LINE TO <itab>.
    ENDIF.

    ASSIGN <itab>[ 1 ] TO FIELD-SYMBOL(<line>).
    set_fcat_from_itab_line( REF #( <line> ) ).

    IF itab_was_initial = abap_true.
      CLEAR <itab>.
    ENDIF.
  ENDMETHOD.


  METHOD set_fcat_from_itab_line.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sets the field catalog from the passed internal table line
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(dscr) = cl_abap_typedescr=>describe_by_data_ref( line ).
    DATA(name) = dscr->get_relative_name( ).
    build_fcat( VALUE #( structure = name ) ).
  ENDMETHOD.
ENDCLASS.
