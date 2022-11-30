CLASS ycl_addict_data_element DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_DATA_ELEMENT',
               END OF class.

    DATA def TYPE dd04l READ-ONLY.
    DATA txt TYPE dd04t READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !rollname     TYPE rollname
      RETURNING VALUE(output) TYPE REF TO ycl_addict_data_element
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_shortest_text_safe
      IMPORTING !rollname   TYPE rollname
      RETURNING VALUE(text) TYPE ddtext.

    CLASS-METHODS get_text_safe
      IMPORTING !rollname   TYPE rollname
      RETURNING VALUE(text) TYPE ddtext.

    METHODS get_domain
      RETURNING VALUE(domain) TYPE REF TO ycl_addict_domain
      RAISING   ycx_addict_table_content.

    METHODS get_shortest_text
      IMPORTING !worst_case_rollname TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(text)          TYPE ddtext.

    METHODS get_text
      IMPORTING !worst_case_rollname TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(text)          TYPE ddtext.

    METHODS validate_value
      IMPORTING !value TYPE val_single
      RAISING   ycx_addict_domain
                ycx_addict_table_content.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF lazy_flg_dict,
             shortest_text TYPE abap_bool,
           END OF lazy_flg_dict.

    TYPES: BEGIN OF lazy_val_dict,
             shortest_text TYPE string,
           END OF lazy_val_dict.

    TYPES: BEGIN OF lazy_dict,
             flg TYPE lazy_flg_dict,
             val TYPE lazy_val_dict,
           END OF lazy_dict.

    TYPES: BEGIN OF multiton_dict,
             rollname TYPE rollname,
             obj      TYPE REF TO ycl_addict_data_element,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS rollname.

    CONSTANTS: BEGIN OF table,
                 main TYPE tabname VALUE 'DD04L',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.

    DATA lazy TYPE lazy_dict.

    METHODS constructor
      IMPORTING !rollname TYPE rollname
      RAISING   ycx_addict_table_content.
ENDCLASS.



CLASS YCL_ADDICT_DATA_ELEMENT IMPLEMENTATION.


  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Object creation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT SINGLE * FROM dd04l                          "#EC CI_NOORDER
           WHERE rollname = @rollname
           INTO @me->def ##WARN_OK.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_addict_table_content
        EXPORTING
          textid   = ycx_addict_table_content=>no_entry_for_objectid
          objectid = CONV #( rollname )
          tabname  = me->table-main.
    ENDIF.

    SELECT SINGLE * FROM dd04t                          "#EC CI_NOORDER
           WHERE rollname   = @rollname AND
                 ddlanguage = @sy-langu
           INTO @me->txt ##WARN_OK.

    IF sy-subrc <> 0.
      SELECT SINGLE * FROM dd04t                        "#EC CI_NOORDER
             WHERE rollname = @rollname
             INTO @me->txt ##WARN_OK.
    ENDIF.
  ENDMETHOD.


  METHOD get_domain.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns domain of data element
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    domain = ycl_addict_domain=>get_instance( me->def-domname ).
  ENDMETHOD.


  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a new instance
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_data_element=>multitons[
             KEY primary_key COMPONENTS
             rollname = rollname
          ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict(
          rollname = rollname
          obj      = NEW #( rollname ) ).

      INSERT multiton INTO TABLE ycl_addict_data_element=>multitons
             ASSIGNING <multiton>.
    ENDIF.

    output = <multiton>-obj.
  ENDMETHOD.


  METHOD get_shortest_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns shortest text of data element
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy-flg-shortest_text = abap_false.

      me->lazy-val-shortest_text = ycl_addict_text_toolkit=>get_shortest_text(
        VALUE #( ( CONV #( me->txt-ddtext ) )
                 ( CONV #( me->txt-reptext ) )
                 ( CONV #( me->txt-scrtext_l ) )
                 ( CONV #( me->txt-scrtext_m ) )
                 ( CONV #( me->txt-scrtext_s ) ) ) ).

      me->lazy-flg-shortest_text = abap_true.
    ENDIF.

    text = COND #(
        WHEN me->lazy-val-shortest_text IS NOT INITIAL
        THEN me->lazy-val-shortest_text
        ELSE COND #(
          WHEN worst_case_rollname = abap_true
          THEN me->def-rollname
          ELSE space ) ).
  ENDMETHOD.


  METHOD get_shortest_text_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns shortext text without raising an exception
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        text = ycl_addict_data_element=>get_instance( rollname )->get_shortest_text( ).
      CATCH cx_root.
        text = rollname.
    ENDTRY.

  ENDMETHOD.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns data element text
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    text = COND #(
        WHEN me->txt-ddtext    IS NOT INITIAL THEN me->txt-ddtext
        WHEN me->txt-reptext   IS NOT INITIAL THEN me->txt-reptext
        WHEN me->txt-scrtext_l IS NOT INITIAL THEN me->txt-scrtext_l
        WHEN me->txt-scrtext_m IS NOT INITIAL THEN me->txt-scrtext_m
        WHEN me->txt-scrtext_s IS NOT INITIAL THEN me->txt-scrtext_s
        WHEN worst_case_rollname = abap_true  THEN me->def-rollname ).
  ENDMETHOD.


  METHOD get_text_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns text without raising an exception
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        text = ycl_addict_data_element=>get_instance( rollname )->get_text( ).
      CATCH cx_root.
        text = rollname.
    ENDTRY.
  ENDMETHOD.


  METHOD validate_value.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Validates a value within the domain
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    get_domain( )->validate_value( value ).
  ENDMETHOD.
ENDCLASS.
