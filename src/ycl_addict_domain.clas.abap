CLASS ycl_addict_domain DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES: BEGIN OF line_dict,
             value TYPE val_single,
             text  TYPE val_text,
           END OF line_dict,

           line_set TYPE HASHED TABLE OF line_dict
                    WITH UNIQUE KEY primary_key COMPONENTS value.

    DATA def TYPE dd01l READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !domname      TYPE domname
      RETURNING VALUE(output) TYPE REF TO ycl_addict_domain
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_value_text_safe
      IMPORTING !domname      TYPE domname
                !value        TYPE val_single
      RETURNING VALUE(output) TYPE val_text.

    METHODS get_text RETURNING VALUE(output) TYPE ddtext.

    METHODS get_value_line
      IMPORTING value         TYPE val_single
      RETURNING VALUE(output) TYPE line_dict
      RAISING   ycx_addict_domain.

    METHODS get_value_tab RETURNING VALUE(output) TYPE line_set.

    METHODS get_value_text
      IMPORTING !value        TYPE val_single
      RETURNING VALUE(output) TYPE val_text
      RAISING   ycx_addict_domain.

    METHODS validate_value
      IMPORTING !value TYPE val_single
      RAISING   ycx_addict_domain
                ycx_addict_table_content.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF lazy_flag_dict,
             text_read       TYPE abap_bool,
             value_text_read TYPE abap_bool,
           END OF lazy_flag_dict.

    TYPES: BEGIN OF multiton_dict,
             domname TYPE domname,
             obj     TYPE REF TO ycl_addict_domain,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS domname.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'DD01L',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    DATA lazy_flag TYPE lazy_flag_dict.
    DATA lines TYPE line_set.
    DATA text TYPE ddtext.

    METHODS ensure_text_read.
    METHODS ensure_value_read.
ENDCLASS.



CLASS ycl_addict_domain IMPLEMENTATION.
  METHOD ensure_text_read.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy reads texts
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->lazy_flag-text_read = abap_false.

    SELECT SINGLE ddtext FROM dd01t                     "#EC CI_NOORDER
           WHERE domname = @me->def-domname AND
                 ddlanguage = @sy-langu
           INTO @me->text ##WARN_OK.

    IF sy-subrc <> 0.
      SELECT SINGLE ddtext FROM dd01t                   "#EC CI_NOORDER
             WHERE domname = @me->def-domname
             INTO @me->text ##WARN_OK.
    ENDIF.

    me->lazy_flag-text_read = abap_true.
  ENDMETHOD.


  METHOD ensure_value_read.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy read values
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->lazy_flag-value_text_read = abap_false.

    SELECT dd07l~domvalue_l AS value,                  "#EC CI_BUFFJOIN
           dd07t~ddtext AS text
           FROM dd07l
                LEFT JOIN dd07t ON
                     dd07t~domname    = dd07l~domname AND
                     dd07t~ddlanguage = @sy-langu AND
                     dd07t~as4local   = dd07l~as4local AND
                     dd07t~valpos     = dd07l~valpos AND
                     dd07t~as4vers    = dd07l~as4vers
           WHERE dd07l~domname = @me->def-domname
           INTO CORRESPONDING FIELDS OF TABLE @me->lines.

    me->lazy_flag-value_text_read = abap_true.
  ENDMETHOD.


  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Multiton factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_domain=>multitons[
        KEY primary_key
        COMPONENTS domname = domname
      ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict( domname = domname ).

      multiton-obj = NEW #( ).

      SELECT SINGLE * FROM dd01l                        "#EC CI_NOORDER
             WHERE domname = @multiton-domname
             INTO @multiton-obj->def ##WARN_OK.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = CONV #( multiton-domname )
            tabname  = ycl_addict_domain=>table-def.
      ENDIF.

      INSERT multiton INTO TABLE ycl_addict_domain=>multitons
             ASSIGNING <multiton>.
    ENDIF.

    output = <multiton>-obj.
  ENDMETHOD.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns domain text
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ensure_text_read( ).
    output = me->text.
  ENDMETHOD.


  METHOD get_value_line.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a value line
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ensure_value_read( ).

    ASSIGN me->lines[ KEY primary_key
                      COMPONENTS value = value
                    ] TO FIELD-SYMBOL(<line>).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_addict_domain
        EXPORTING
          textid     = ycx_addict_domain=>invalid_value
          domname    = me->def-domname
          domvalue_l = value.
    ENDIF.

    output = <line>.
  ENDMETHOD.


  METHOD get_value_tab.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns all domain values
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ensure_value_read( ).
    output = me->lines.
  ENDMETHOD.


  METHOD get_value_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the text of a given domain value
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = get_value_line( value )-text.
  ENDMETHOD.


  METHOD get_value_text_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the text of a value without generating an exception
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        output = get_instance( domname )->get_value_text( value ).
      CATCH cx_root.
        output = value.
    ENDTRY.
  ENDMETHOD.


  METHOD validate_value.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ensures that the passed value is valid within the domain
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA dummy TYPE string.

    ensure_value_read( ).

    IF me->lines IS NOT INITIAL.
      get_value_line( value ).
      RETURN.
    ENDIF.

    IF me->def-entitytab IS NOT INITIAL.
      DATA(table_keys) = ycl_addict_table=>get_instance( me->def-entitytab )->get_key_fields( with_mandt = abap_false ).
      DATA(table_key) = table_keys[ 1 ]-fieldname.

      DATA(where) = |{ table_key } = '{ value }'|.

      SELECT SINGLE (table_key) INTO dummy
             FROM (me->def-entitytab)
             WHERE (where).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_domain
          EXPORTING
            textid     = ycx_addict_domain=>invalid_value
            domname    = me->def-domname
            domvalue_l = value.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
