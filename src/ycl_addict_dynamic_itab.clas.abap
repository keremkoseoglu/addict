CLASS ycl_addict_dynamic_itab DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF fld_dict,
             fnam TYPE fieldname,
             dtel TYPE rollname,
           END OF fld_dict,

           fld_list   TYPE STANDARD TABLE OF fld_dict WITH EMPTY KEY,
           fnam_range TYPE RANGE OF fieldname.

    DATA fields TYPE fld_list READ-ONLY.

    CLASS-METHODS create_range
      EXPORTING wa   TYPE REF TO data
                !tab TYPE REF TO data.

    CLASS-METHODS get_instance_as_range
      IMPORTING rollname   TYPE rollname
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_dynamic_itab
      RAISING   ycx_addict_method_parameter
                ycx_addict_table_content.

    CLASS-METHODS get_instance_with_tabname
      IMPORTING tabname    TYPE tabname
                fnam_rng   TYPE fnam_range OPTIONAL
                extra_fld  TYPE fld_list   OPTIONAL
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_dynamic_itab
      RAISING   ycx_addict_method_parameter.

    METHODS constructor
      IMPORTING fld TYPE fld_list
      RAISING   ycx_addict_method_parameter.

    METHODS get_alv_fcat                   RETURNING VALUE(fcat) TYPE slis_t_fieldcat_alv.

    METHODS get_alv_fcat_with_uncut_titles RETURNING VALUE(fcat) TYPE slis_t_fieldcat_alv.

    METHODS get_lvc_fcat                   RETURNING VALUE(fcat) TYPE lvc_t_fcat.
    METHODS get_itab_ref                   RETURNING VALUE(ref)  TYPE REF TO data.
    METHODS get_wa_ref                     RETURNING VALUE(ref)  TYPE REF TO data.

    METHODS set_fcat_text
      IMPORTING fieldname TYPE fieldname
                !text     TYPE clike.

  PROTECTED SECTION.
    CLASS-METHODS uncut_alv_fcat_titles
      IMPORTING normal_fcat   TYPE REF TO slis_t_fieldcat_alv
      RETURNING VALUE(result) TYPE slis_t_fieldcat_alv.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF field_name,
                 char18 TYPE fieldname VALUE 'CHAR18',
                 include TYPE fieldname VALUE '.INCLUDE',
                 fld    TYPE fieldname VALUE 'FLD',
                 fnam   TYPE fieldname VALUE 'FNAM',
               END OF field_name.

    CONSTANTS: BEGIN OF method,
                 constructor TYPE seocpdname VALUE 'CONSTRUCTOR',
               END OF method.

    DATA: comp                  TYPE cl_abap_structdescr=>component_table,
          slis_fcat_cache       TYPE slis_t_fieldcat_alv,
          uncut_slis_fcat_cache TYPE slis_t_fieldcat_alv,
          lvc_fcat_cache        TYPE lvc_t_fcat,
          itab_ref_cache        TYPE REF TO data,
          wa_ref_cache          TYPE REF TO data.

    METHODS build_comp.

    METHODS validate_fld
      IMPORTING fld TYPE fld_list
      RAISING   ycx_addict_table_content.

    METHODS obtain_alv_fcat RETURNING VALUE(result) TYPE REF TO slis_t_fieldcat_alv.
ENDCLASS.


CLASS ycl_addict_dynamic_itab IMPLEMENTATION.
  METHOD create_range.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Creates a dynamic range
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA tabledescr  TYPE REF TO cl_abap_tabledescr.
    DATA datadescr   TYPE REF TO cl_abap_datadescr.
    DATA components  TYPE abap_component_tab.
    DATA component   TYPE LINE OF abap_component_tab.

    component-name = ycl_addict_toolkit=>field-sign.
    component-type = cl_abap_elemdescr=>get_c( p_length = 1 ).
    INSERT component INTO TABLE components.

    component-name = ycl_addict_toolkit=>field-option.
    component-type = cl_abap_elemdescr=>get_c( p_length = 2 ).
    INSERT component INTO TABLE components.

    component-name  = ycl_addict_toolkit=>field-low.
    component-type ?= cl_abap_elemdescr=>describe_by_name( field_name-char18 ).
    INSERT component INTO TABLE components.

    component-name  = ycl_addict_toolkit=>field-high.
    component-type ?= cl_abap_elemdescr=>describe_by_name( field_name-char18 ).
    INSERT component INTO TABLE components.

    structdescr = cl_abap_structdescr=>create( components ).
    CREATE DATA wa TYPE HANDLE structdescr.
    datadescr = structdescr.
    tabledescr = cl_abap_tabledescr=>create( datadescr ).
    CREATE DATA tab TYPE HANDLE tabledescr.
  ENDMETHOD.

  METHOD get_instance_as_range.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory, returning a dynamic range
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(dtel) = ycl_addict_data_element=>get_instance( rollname ).

    obj = NEW #( VALUE #( ( fnam = ycl_addict_toolkit=>field-sign   dtel = ycl_addict_toolkit=>rollname-sign )
                          ( fnam = ycl_addict_toolkit=>field-option dtel = ycl_addict_toolkit=>rollname-option )
                          ( fnam = ycl_addict_toolkit=>field-low    dtel = rollname )
                          ( fnam = ycl_addict_toolkit=>field-high   dtel = rollname ) ) ).
  ENDMETHOD.

  METHOD get_instance_with_tabname.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory, returning a dynamic internal table
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA fld TYPE fld_list.

    SELECT fieldname AS fnam,
           rollname  AS dtel
           FROM dd03l
           WHERE tabname    = @tabname
             AND fieldname IN @fnam_rng
             AND fieldname <> @field_name-include
           ORDER BY position
           INTO CORRESPONDING FIELDS OF TABLE @fld.

    APPEND LINES OF extra_fld TO fld.
    obj = NEW #( fld ).
  ENDMETHOD.

  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Called on object creation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        validate_fld( fld ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION NEW ycx_addict_method_parameter( textid      = ycx_addict_method_parameter=>param_value_invalid
                                                         previous    = diaper
                                                         class_name  = ycl_addict_class=>get_class_name( me )
                                                         method_name = method-constructor
                                                         param_name  = CONV #( field_name-fld ) ).
    ENDTRY.

    me->fields = fld.
  ENDMETHOD.

  METHOD get_alv_fcat.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return the ALV FCAT of the dynamic ITAB
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cached_fcat) = obtain_alv_fcat( ).
    fcat = cached_fcat->*.
  ENDMETHOD.

  METHOD get_alv_fcat_with_uncut_titles.
    IF me->uncut_slis_fcat_cache IS INITIAL.
      DATA(normal_fcat) = obtain_alv_fcat( ).
      me->uncut_slis_fcat_cache = uncut_alv_fcat_titles( normal_fcat ).
    ENDIF.

    fcat = me->uncut_slis_fcat_cache.
  ENDMETHOD.

  METHOD get_lvc_fcat.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return the LVC FCAT of the dynamic ITAB
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE STANDARD TABLE.

    IF me->lvc_fcat_cache IS INITIAL.
      DATA(slis_fcat) = get_alv_fcat( ).
      DATA(itab) = get_itab_ref( ).
      ASSIGN itab->* TO <itab>.

      CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
        EXPORTING  it_fieldcat_alv = slis_fcat
        IMPORTING  et_fieldcat_lvc = me->lvc_fcat_cache
        TABLES     it_data         = <itab>
        EXCEPTIONS it_data_missing = 1
                   OTHERS          = 2 ##FM_SUBRC_OK.
    ENDIF.

    fcat = me->lvc_fcat_cache.
  ENDMETHOD.

  METHOD get_itab_ref.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return internal table reference
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->itab_ref_cache IS INITIAL.
      build_comp( ).

      DATA(tab) = cl_abap_tabledescr=>create( p_line_type  = cl_abap_structdescr=>create( me->comp )
                                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                                              p_unique     = abap_false ).

      CREATE DATA me->itab_ref_cache TYPE HANDLE tab.
    ENDIF.

    ref = me->itab_ref_cache.
  ENDMETHOD.

  METHOD get_wa_ref.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return work area reference
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->wa_ref_cache IS INITIAL.
      build_comp( ).
      DATA(str) = cl_abap_structdescr=>create( me->comp ).
      CREATE DATA me->wa_ref_cache TYPE HANDLE str.
    ENDIF.

    ref = me->wa_ref_cache.
  ENDMETHOD.

  METHOD set_fcat_text.
    TRY.
        get_alv_fcat( ).
        DATA(fcat_entry) = REF #( me->slis_fcat_cache[ fieldname = fieldname ] ).

      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    fcat_entry->seltext_l = text.
    fcat_entry->seltext_m = text.
    fcat_entry->seltext_s = text.
  ENDMETHOD.

  METHOD uncut_alv_fcat_titles.
    LOOP AT normal_fcat->* REFERENCE INTO DATA(normal).
      APPEND normal->* TO result REFERENCE INTO DATA(uncut).

      DATA(text_len) = COND i( WHEN uncut->seltext_l IS NOT INITIAL    THEN strlen( uncut->seltext_l )
                               WHEN uncut->seltext_m IS NOT INITIAL    THEN strlen( uncut->seltext_m )
                               WHEN uncut->seltext_s IS NOT INITIAL    THEN strlen( uncut->seltext_s )
                               WHEN uncut->reptext_ddic IS NOT INITIAL THEN strlen( uncut->reptext_ddic ) ).

      DATA(big_len) = COND i( WHEN text_len > uncut->intlen
                              THEN text_len
                              ELSE uncut->intlen ).

      uncut->outputlen = big_len.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_comp.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build components
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA element TYPE REF TO cl_abap_elemdescr.
    DATA tab     TYPE REF TO cl_abap_tabledescr.

    CHECK me->comp IS INITIAL.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<fld>).
      TRY.
          element ?= cl_abap_elemdescr=>describe_by_name( <fld>-dtel ).
          APPEND VALUE #( name = <fld>-fnam type = element ) TO me->comp.
          CONTINUE.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

      TRY.
          tab ?= cl_abap_tabledescr=>describe_by_name( <fld>-dtel ).
          APPEND VALUE #( name = <fld>-fnam type = tab ) TO me->comp.
          CONTINUE.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_fld.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Runs validations on the provided field list
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(fld_copy) = fld.
    SORT fld_copy BY fnam.
    DELETE ADJACENT DUPLICATES FROM fld_copy COMPARING fnam.

    IF lines( fld_copy ) <> lines( fld ).
      RAISE EXCEPTION NEW ycx_addict_table_content( textid    = ycx_addict_table_content=>column_values_duplicate
                                                    tabname   = me->field_name-fld
                                                    fieldname = me->field_name-fnam ).
    ENDIF.
  ENDMETHOD.

  METHOD obtain_alv_fcat.
    result = REF #( me->slis_fcat_cache ).
    CHECK result->* IS INITIAL.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<fld>).
      TRY.
          DATA(dtel) = ycl_addict_data_element=>get_instance( <fld>-dtel ).
          DATA(doma) = dtel->get_domain( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      APPEND VALUE #( fieldname = <fld>-fnam
                      intlen    = doma->def-leng
                      rollname  = <fld>-dtel
                      lowercase = doma->def-lowercase )
             TO result->*.

      DATA(title) = ycl_addict_data_element=>get_text_safe( <fld>-dtel ).
      IF title IS INITIAL.
        title = <fld>-fnam.
      ENDIF.

      set_fcat_text( fieldname = <fld>-fnam
                     text      = title ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
