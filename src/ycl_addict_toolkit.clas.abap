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

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_addict_toolkit IMPLEMENTATION.
ENDCLASS.
