CLASS ycl_addict_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF field,
                 high   TYPE fieldname VALUE 'HIGH',
                 low    TYPE fieldname VALUE 'LOW',
                 option TYPE fieldname VALUE 'OPTION',
                 sign   TYPE fieldname VALUE 'SIGN',
               END OF field.

    CONSTANTS: BEGIN OF option,
                 bt TYPE ddoption VALUE 'BT',
                 cp TYPE ddoption VALUE 'CP',
                 eq TYPE ddoption VALUE 'EQ',
                 ge TYPE ddoption VALUE 'GE',
                 le TYPE ddoption VALUE 'LE',
                 ne TYPE ddoption VALUE 'NE',
               END OF option.

    CONSTANTS: BEGIN OF rollname,
                 option TYPE rollname VALUE 'DDOPTION',
                 sign   TYPE rollname VALUE 'DDSIGN',
               END OF rollname.

    CONSTANTS: BEGIN OF sign,
                 exclude TYPE ddsign VALUE 'E',
                 include TYPE ddsign VALUE 'I',
               END OF sign.

    CLASS-METHODS get_system_definitions RETURNING VALUE(def) TYPE ytaddict_sydef.
    CLASS-METHODS get_system_rules RETURNING VALUE(rules) TYPE REF TO yif_addict_system_rules.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA sydef TYPE ytaddict_sydef.
    CLASS-DATA rules TYPE REF TO yif_addict_system_rules.
ENDCLASS.



CLASS YCL_ADDICT_TOOLKIT IMPLEMENTATION.


  METHOD get_system_definitions.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Reads system definitions and returns default values if
    " the definition is not present
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ycl_addict_toolkit=>sydef IS INITIAL.
      SELECT SINGLE * FROM yv_addict_system_definitions( sy_sysid = @sy-sysid )
             INTO CORRESPONDING FIELDS OF @ycl_addict_toolkit=>sydef.
    ENDIF.

    def = ycl_addict_toolkit=>sydef.

    " Where Used List: ycl_addict_def_system_rules has been
    " mentioned in the CDS View above.
    IF 1 = 0.
      DATA(dummy) = NEW ycl_addict_def_system_rules( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_system_rules.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns an object representing the rules of the system
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA obj TYPE REF TO object.

    IF ycl_addict_toolkit=>rules IS INITIAL.
      DATA(def) = get_system_definitions( ).
      CREATE OBJECT obj TYPE (def-rule_class).
      ycl_addict_toolkit=>rules ?= obj.
    ENDIF.

    rules = ycl_addict_toolkit=>rules.
  ENDMETHOD.
ENDCLASS.
