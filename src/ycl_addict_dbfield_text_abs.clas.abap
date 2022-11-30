CLASS ycl_addict_dbfield_text_abs DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS build_cor RETURNING VALUE(output) TYPE REF TO ycl_addict_dbfield_text_abs.

    CLASS-METHODS get_text_via_chain
      IMPORTING !dbfield      TYPE clike
      RETURNING VALUE(output) TYPE ddtext.

    METHODS get_text ABSTRACT
      IMPORTING !dbfield      TYPE clike
      RETURNING VALUE(output) TYPE ddtext.

    METHODS set_next IMPORTING !next TYPE REF TO ycl_addict_dbfield_text_abs.

  PROTECTED SECTION.
    DATA next TYPE REF TO ycl_addict_dbfield_text_abs.

  PRIVATE SECTION.
    TYPES: BEGIN OF cor_dict,
             clsname TYPE seoclsname,
             obj     TYPE REF TO ycl_addict_dbfield_text_abs,
           END OF cor_dict,

           cor_list TYPE STANDARD TABLE OF cor_dict WITH EMPTY KEY.

    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_DBFIELD_TEXT_ABS',
               END OF class.

    CLASS-DATA cors TYPE cor_list.
ENDCLASS.



CLASS YCL_ADDICT_DBFIELD_TEXT_ABS IMPLEMENTATION.


  METHOD build_cor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Builds the chain of responsibility
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA obj TYPE REF TO object.

    TRY.
        data(cls) = ycl_addict_class=>get_instance( class-me ).
        data(instanceable_subclasses) = cls->get_instanceable_subclasses( ).

        ycl_addict_dbfield_text_abs=>cors = value #(
            for _is in instanceable_subclasses ( clsname = _is ) ).
      CATCH cx_root ##no_Handler.
    ENDTRY.

    LOOP AT ycl_addict_dbfield_text_abs=>cors ASSIGNING FIELD-SYMBOL(<cor>).
      DATA(tabix) = sy-tabix.

      TRY.
          CREATE OBJECT obj TYPE (<cor>-clsname).
          <cor>-obj ?= obj.
        CATCH cx_root.
          DELETE ycl_addict_dbfield_text_abs=>cors.
          CONTINUE.
      ENDTRY.

      CHECK tabix > 1.

      <cor>-obj->set_next( ycl_addict_dbfield_text_abs=>cors[ tabix - 1 ]-obj ).
    ENDLOOP.

    IF ycl_addict_dbfield_text_abs=>cors IS NOT INITIAL.
      output = ycl_addict_dbfield_text_abs=>cors[ lines( ycl_addict_dbfield_text_abs=>cors ) ]-obj.
    ENDIF.
  ENDMETHOD.


  METHOD get_text_via_chain.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the table field text via the chain of responsibility
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(first) = build_cor( ).

    IF first IS NOT INITIAL.
      output = first->get_text( dbfield ).
    ENDIF.

    IF output IS INITIAL.
      output = dbfield.
    ENDIF.
  ENDMETHOD.


  METHOD set_next.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sets the next link in the chain of responsibility
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->next = next.
  ENDMETHOD.
ENDCLASS.
