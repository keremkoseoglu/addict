CLASS ycl_addict_text_toolkit DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES string_list TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CLASS-METHODS get_shortest_text
      IMPORTING
        !candidates     TYPE string_list
        ignore_if_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(shortest) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF text_and_len_dict,
             text TYPE string,
             len  TYPE i,
           END OF text_and_len_dict,

           text_and_len_list TYPE STANDARD TABLE OF text_and_len_dict WITH EMPTY KEY.
ENDCLASS.



CLASS YCL_ADDICT_TEXT_TOOLKIT IMPLEMENTATION.


  METHOD get_shortest_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the shortest text among candidates
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(tl) = VALUE text_and_len_list(
        FOR _candidate IN candidates
        ( text = _candidate
          len  = strlen( _candidate ) ) ).

    IF ignore_if_empty EQ abap_true.
      DELETE tl WHERE text IS INITIAL.
    ENDIF.

    IF tl IS NOT INITIAL.
      SORT tl BY len.
      shortest = tl[ 1 ]-text.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
