CLASS ycl_addict_gui_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS are_you_sure
      IMPORTING !text TYPE clike
      RAISING   ycx_addict_user_input.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF answer,
                 yes TYPE char1 VALUE 'J',
               END OF answer.
ENDCLASS.



CLASS ycl_addict_gui_toolkit IMPLEMENTATION.
  METHOD are_you_sure.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Asks the user if he/she wants to continue
    " Raises exception on rejection
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA user_answer(1).

    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
        textline1 = text
        titel     = TEXT-608
      IMPORTING
        answer    = user_answer.

    IF user_answer <> ycl_addict_gui_toolkit=>answer-yes.
      RAISE EXCEPTION TYPE ycx_addict_user_input
        EXPORTING
          textid = ycx_addict_user_input=>user_cancelled.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
