CLASS ycx_addict_object DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF cant_create_instance,
        msgid TYPE symsgid VALUE 'YADDICT',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'CLSNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cant_create_instance .
    DATA clsname TYPE seoclsname .

    CLASS-METHODS raise_instance_error_for_obj
      IMPORTING
        !object   TYPE REF TO object
        !previous TYPE REF TO cx_root
      RAISING
        ycx_addict_object .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !clsname  TYPE seoclsname OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF class,
                 junk_prefix_1 TYPE seoclsname VALUE '/CLASS=',
                 junk_prefix_2 TYPE seoclsname VALUE '\CLASS=',
                 unknown       TYPE seoclsname VALUE '?',
               END OF class.
ENDCLASS.



CLASS YCX_ADDICT_OBJECT IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->clsname = clsname .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_instance_error_for_obj.
    IF object IS NOT INITIAL.
      DATA(clsname) = cl_abap_classdescr=>get_class_name( object ).
      REPLACE ALL OCCURRENCES OF: class-junk_prefix_1 IN clsname WITH space,
                                  class-junk_prefix_2 IN clsname WITH space.
    ELSE.
      clsname = class-unknown.
    ENDIF.

    RAISE EXCEPTION TYPE ycx_addict_object
      EXPORTING
        textid   = ycx_addict_object=>cant_create_instance
        previous = previous
        clsname  = CONV #( clsname ).
  ENDMETHOD.
ENDCLASS.
