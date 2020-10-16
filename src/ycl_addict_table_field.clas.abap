CLASS ycl_addict_table_field DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    DATA data_element TYPE REF TO ycl_addict_data_element READ-ONLY.
    DATA def          TYPE dd03l READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !tab       TYPE tabname
                !fld       TYPE fieldname
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_table_field
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_instance_by_fullname
      IMPORTING !fullname  TYPE clike
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_table_field
      RAISING   ycx_addict_method_parameter
                ycx_addict_table_content.

    METHODS get_text RETURNING VALUE(text) TYPE ddtext.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             tab TYPE tabname,
             fld TYPE fieldname,
             obj TYPE REF TO ycl_addict_table_field,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS tab fld.

    CONSTANTS fullname_separator TYPE char1 VALUE '-'.

    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_TABLE_FIELD',
               END OF class.

    CONSTANTS: BEGIN OF method,
                 gibf TYPE seocpdname VALUE 'GET_INSTANCE_BY_FULLNAME',
               END OF method.

    CONSTANTS: BEGIN OF field,
                 fullname TYPE seocpdname VALUE 'FULLNAME',
               END OF field.

    CONSTANTS: BEGIN OF table,
                 main TYPE tabname VALUE 'DD03L',
               END OF table.

    CLASS-DATA multitons TYPE multiton_set.
ENDCLASS.



CLASS ycl_addict_table_field IMPLEMENTATION.
  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    ASSIGN ycl_addict_table_field=>multitons[
             KEY primary_key COMPONENTS
             tab = tab
             fld = fld
           ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict(
          tab = tab
          fld = fld ).

      CREATE OBJECT multiton-obj.

      SELECT SINGLE * FROM dd03l
             WHERE tabname   = @multiton-tab AND
                   fieldname = @multiton-fld
             INTO @multiton-obj->def ##WARN_OK.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = |{ multiton-tab }-{ multiton-fld }|
            tabname  = ycl_addict_table_field=>table-main.
      ENDIF.

      IF multiton-obj->def-rollname IS NOT INITIAL.
        multiton-obj->data_element = ycl_addict_data_element=>get_instance( multiton-obj->def-rollname ).
      ENDIF.

      INSERT multiton INTO TABLE ycl_addict_table_field=>multitons
             ASSIGNING <multiton>.
    ENDIF.

    obj = <multiton>-obj.
  ENDMETHOD.


  METHOD get_instance_by_fullname.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory by full name
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA fld TYPE fieldname.
    DATA tab TYPE tabname.

    SPLIT fullname
          AT ycl_addict_table_field=>fullname_separator
          INTO tab fld.

    IF tab IS INITIAL OR fld IS INITIAL.
      RAISE EXCEPTION TYPE ycx_addict_method_parameter
        EXPORTING
          textid      = ycx_addict_method_parameter=>param_value_invalid
          class_name  = ycl_addict_table_field=>class-me
          method_name = ycl_addict_table_field=>method-gibf
          param_name  = ycl_addict_table_field=>field-fullname.
    ENDIF.

    obj = get_instance( tab = tab
                        fld = fld ).
  ENDMETHOD.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns text of table field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->data_element IS NOT INITIAL.
      text = me->data_element->get_text( worst_case_rollname = abap_false ).
    ENDIF.

    IF text IS INITIAL.
      text = |{ me->def-tabname }{ me->fullname_separator }{ me->def-fieldname }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
