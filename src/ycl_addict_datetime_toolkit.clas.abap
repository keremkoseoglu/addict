CLASS ycl_addict_datetime_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS is_factory_workday
      IMPORTING !datum         TYPE sydatum
                !calid         TYPE scal-fcalid DEFAULT 'TR'
      RETURNING VALUE(workday) TYPE abap_bool
      RAISING   ycx_addict_function_subrc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF factory_workday_cache_dict,
             datum   TYPE sydatum,
             calid   TYPE scal-fcalid,
             cx      TYPE REF TO ycx_addict_function_subrc,
             workday TYPE abap_bool,
           END OF factory_workday_cache_dict,

           factory_workday_cache_set TYPE HASHED TABLE OF factory_workday_cache_dict
             WITH UNIQUE KEY primary_key COMPONENTS datum calid.

    CLASS-DATA factory_workday_cache TYPE factory_workday_cache_set.
ENDCLASS.



CLASS ycl_addict_datetime_toolkit IMPLEMENTATION.
  METHOD is_factory_workday.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tells if the date is a workday or not
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_datetime_toolkit=>factory_workday_cache[
             KEY primary_key COMPONENTS
             datum = datum
             calid = calid
           ] TO FIELD-SYMBOL(<fwd>).

    IF sy-subrc <> 0.
      DATA(fwd) = VALUE factory_workday_cache_dict(
                    datum = datum
                    calid = calid ).

      TRY.
          CALL FUNCTION 'YF_ADDICT_DATE_CHECK_WORKDAY'
            EXPORTING
              date                       = fwd-datum
              factory_calendar_id        = fwd-calid
              message_type               = ycl_simbal=>msgty-status
            EXCEPTIONS
              date_after_range           = 1
              date_before_range          = 2
              date_invalid               = 3
              date_no_workingday         = 4
              factory_calendar_not_found = 5
              message_type_invalid       = 6
              OTHERS                     = 7.

          CASE sy-subrc.
            WHEN 0.
              fwd-workday = abap_true.
            WHEN 4.
              fwd-workday = abap_false.
            WHEN OTHERS.
              ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'DATE_CHECK_WORKINGDAY' ).
          ENDCASE.

        CATCH ycx_addict_function_subrc INTO DATA(function_error).
          fwd-cx = function_error.
      ENDTRY.

      INSERT fwd INTO TABLE ycl_addict_datetime_toolkit=>factory_workday_cache
             ASSIGNING <fwd>.
    ENDIF.

    IF <fwd>-cx IS NOT INITIAL.
      RAISE EXCEPTION <fwd>-cx.
    ENDIF.

    workday = <fwd>-workday.
  ENDMETHOD.
ENDCLASS.
