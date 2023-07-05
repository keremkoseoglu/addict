CLASS ycl_addict_itab_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_itab_components
      IMPORTING itab          TYPE ANY TABLE
      RETURNING VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS get_struct_desc_components
      IMPORTING struct_desc   TYPE REF TO cl_abap_structdescr
      RETURNING VALUE(result) TYPE cl_abap_structdescr=>component_table.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_addict_itab_toolkit IMPLEMENTATION.
  METHOD get_itab_components.
    DATA(table_desc)  = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( itab ) ).
    DATA(struct_desc) = CAST cl_abap_structdescr( table_desc->get_table_line_type( ) ).
    result            = get_struct_desc_components( struct_desc ).
  ENDMETHOD.

  METHOD get_struct_desc_components.
    DATA(components) = struct_desc->get_components( ).

    LOOP AT components REFERENCE INTO DATA(component).
      CASE component->as_include.
        WHEN abap_true.
          DATA(sub_structure) = component->type->absolute_name.
          REPLACE ALL OCCURRENCES OF '\TYPE=' IN sub_structure WITH space.
          DATA(sub_str_def) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( sub_structure ) ).
          APPEND LINES OF get_struct_desc_components( sub_str_def ) TO result.
        WHEN abap_false.
          APPEND component->* TO result.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
