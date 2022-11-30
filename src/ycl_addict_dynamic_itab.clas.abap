CLASS ycl_addict_dynamic_itab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF fld_dict,
             fnam TYPE fieldname,
             dtel TYPE rollname,
           END OF fld_dict,

           fld_list   TYPE STANDARD TABLE OF fld_dict WITH EMPTY KEY,
           fnam_range TYPE RANGE OF fieldname.

    DATA fields TYPE fld_list READ-ONLY.

    CLASS-METHODS create_range
      IMPORTING !field TYPE fieldname
      EXPORTING !wa    TYPE REF TO data
                !tab   TYPE REF TO data .

    CLASS-METHODS get_instance_as_range
      IMPORTING !rollname  TYPE rollname
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_dynamic_itab
      RAISING   ycx_addict_method_parameter
                ycx_addict_table_content.

    CLASS-METHODS get_instance_with_tabname
      IMPORTING !tabname   TYPE tabname
                !fnam_rng  TYPE fnam_range OPTIONAL
                !extra_fld TYPE fld_list OPTIONAL
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_dynamic_itab
      RAISING   ycx_addict_method_parameter.

    METHODS constructor
      IMPORTING !fld TYPE fld_list
      RAISING   ycx_addict_method_parameter.

    METHODS get_alv_fcat RETURNING VALUE(fcat) TYPE slis_t_fieldcat_alv.
    METHODS get_lvc_fcat RETURNING VALUE(fcat) TYPE lvc_t_fcat.
    METHODS get_itab_ref RETURNING VALUE(ref) TYPE REF TO data.
    METHODS get_wa_ref RETURNING VALUE(ref) TYPE REF TO data.

    METHODS set_fcat_text
      IMPORTING !fieldname TYPE fieldname
                !text      TYPE clike.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_DYNAMIC_ITAB',
               END OF class.

    CONSTANTS: BEGIN OF field_name,
                 char18  TYPE fieldname VALUE 'CHAR18',
                 include TYPE fieldname VALUE '.INCLUDE',
                 fld     TYPE fieldname VALUE 'FLD',
                 fnam    TYPE fieldname VALUE 'FNAM',
               END OF field_name.

    CONSTANTS: BEGIN OF method,
                 constructor TYPE seocpdname VALUE 'CONSTRUCTOR',
               END OF method.

    DATA comp     TYPE cl_abap_structdescr=>component_table.
    DATA fcat     TYPE slis_t_fieldcat_alv.
    DATA lvc_fcat TYPE lvc_t_fcat.
    DATA tref     TYPE REF TO data.
    DATA wref     TYPE REF TO data.

    METHODS build_comp.

    METHODS validate_fld
      IMPORTING !fld TYPE fld_list
      RAISING   ycx_addict_table_content.
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

    MOVE ycl_addict_toolkit=>field-sign TO component-name.
    component-type = cl_abap_elemdescr=>get_c( p_length = 1 ).
    INSERT component INTO TABLE components.

    MOVE ycl_addict_toolkit=>field-option TO component-name.
    component-type = cl_abap_elemdescr=>get_c( p_length = 2 ).
    INSERT component INTO TABLE components.

    MOVE ycl_addict_toolkit=>field-low TO component-name.
    component-type ?= cl_abap_elemdescr=>describe_by_name( field_name-char18 ).
    INSERT component INTO TABLE components.

    MOVE ycl_addict_toolkit=>field-high TO component-name.
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

    obj = NEW #( VALUE #(
        ( fnam = ycl_addict_toolkit=>field-sign   dtel = ycl_addict_toolkit=>rollname-sign )
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
           WHERE tabname    = @tabname AND
                 fieldname IN @fnam_rng AND
                 fieldname <> @field_name-include
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
        RAISE EXCEPTION TYPE ycx_addict_method_parameter
          EXPORTING
            textid      = ycx_addict_method_parameter=>param_value_invalid
            previous    = diaper
            class_name  = class-me
            method_name = method-constructor
            param_name  = CONV #( field_name-fld ).
    ENDTRY.

    me->fields = fld.
  ENDMETHOD.


  METHOD get_alv_fcat.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return the ALV FCAT of the dynamic ITAB
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->fcat[] IS INITIAL.

      LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<fld>).
        TRY.
            DATA(dtel) = ycl_addict_data_element=>get_instance( <fld>-dtel ).
            DATA(doma) = dtel->get_domain( ).
          CATCH cx_root .
            CONTINUE.
        ENDTRY.

        APPEND VALUE #( fieldname = <fld>-fnam
                        intlen    = doma->def-leng
                        rollname  = <fld>-dtel
                        lowercase = doma->def-lowercase )
               TO me->fcat.

        DATA(title) = ycl_addict_data_element=>get_text_safe( <fld>-dtel ).
        IF title IS INITIAL.
          title = <fld>-fnam.
        ENDIF.

        set_fcat_text( fieldname = <fld>-fnam
                       text      = title ).
      ENDLOOP.
    ENDIF.

    fcat[] = me->fcat[].
  ENDMETHOD.


  METHOD get_lvc_fcat.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return the LVC FCAT of the dynamic ITAB
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE STANDARD TABLE.

    IF me->lvc_fcat IS INITIAL.
      DATA(slis_fcat) = get_alv_fcat( ).
      DATA(itab) = get_itab_ref( ).
      ASSIGN itab->* TO <itab>.

      CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
        EXPORTING
          it_fieldcat_alv = slis_fcat
        IMPORTING
          et_fieldcat_lvc = me->lvc_fcat
        TABLES
          it_data         = <itab>
        EXCEPTIONS
          it_data_missing = 1
          OTHERS          = 2 ##FM_SUBRC_OK.
    ENDIF.

    fcat = me->lvc_fcat.
  ENDMETHOD.


  METHOD get_itab_ref.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return internal table reference
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->tref IS INITIAL.
      build_comp( ).

      DATA(tab) = cl_abap_tabledescr=>create(
          p_line_type  = cl_abap_structdescr=>create( me->comp )
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false ).

      CREATE DATA me->tref TYPE HANDLE tab.
    ENDIF.

    ref = me->tref.
  ENDMETHOD.


  METHOD get_wa_ref.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy build & return work area reference
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->wref IS INITIAL.
      build_comp( ).
      DATA(str) = cl_abap_structdescr=>create( me->comp ).
      CREATE DATA me->wref TYPE HANDLE str.
    ENDIF.

    ref = me->wref.
  ENDMETHOD.


  METHOD set_fcat_text.
    TRY.
        get_alv_fcat( ).
        DATA(fcat_entry) = REF #( me->fcat[ fieldname = fieldname ] ).

      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    fcat_entry->seltext_l =
    fcat_entry->seltext_m =
    fcat_entry->seltext_s = text.
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
          element ?=  cl_abap_elemdescr=>describe_by_name( <fld>-dtel ).
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
      RAISE EXCEPTION TYPE ycx_addict_table_content
        EXPORTING
          textid    = ycx_addict_table_content=>column_values_duplicate
          tabname   = me->field_name-fld
          fieldname = me->field_name-fnam.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
