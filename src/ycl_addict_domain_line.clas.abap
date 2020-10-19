CLASS ycl_addict_domain_line DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA domain TYPE REF TO ycl_addict_domain.
    DATA line   TYPE ycl_addict_domain=>line_dict READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !domname   TYPE domname
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_domain_line
      RAISING   ycx_addict_table_content.

    METHODS set_value
      IMPORTING !value TYPE val_single
      RAISING   ycx_addict_domain.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             domname TYPE domname,
             obj     TYPE REF TO ycl_addict_domain_line,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS domname.

    CLASS-DATA multiton TYPE multiton_set.
ENDCLASS.



CLASS ycl_addict_domain_line IMPLEMENTATION.
  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_domain_line=>multiton[
             KEY primary_key COMPONENTS
             domname = domname
           ] TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(sing) = VALUE multiton_dict( domname = domname ).
      sing-obj = NEW #( ).
      sing-obj->domain = ycl_addict_domain=>get_instance( sing-domname ).
      INSERT sing INTO TABLE ycl_addict_domain_line=>multiton ASSIGNING <multiton>.
    ENDIF.

    obj = <multiton>-obj.
  ENDMETHOD.


  METHOD set_value.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sets a new value line
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    line = domain->get_value_line( value ).
  ENDMETHOD.
ENDCLASS.
