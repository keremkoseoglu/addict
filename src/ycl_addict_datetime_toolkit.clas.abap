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

    CLASS-METHODS add_to_time
      IMPORTING !idate TYPE dats
                !itime TYPE tims
                !stdaz TYPE yd_addict_thour
      EXPORTING !edate TYPE dats
                !etime TYPE tims.

    CLASS-METHODS get_day_in_week
      IMPORTING !date         TYPE sydatum
      RETURNING VALUE(result) TYPE i.

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

    TYPES: BEGIN OF day_in_week_dict,
             datum TYPE sydatum,
             day   TYPE i,
           END OF day_in_week_dict,

           day_in_week_set TYPE HASHED TABLE OF day_in_week_dict
                           WITH UNIQUE KEY primary_key COMPONENTS datum.

    CLASS-DATA factory_workday_cache TYPE factory_workday_cache_set.
    CLASS-DATA day_in_week_cache TYPE day_in_week_set.
ENDCLASS.



CLASS YCL_ADDICT_DATETIME_TOOLKIT IMPLEMENTATION.


  METHOD add_to_time.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Add time to a date + time.
    " This method has been shamelessly copied from CATT_ADD_TO_TIME
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA low_date TYPE d VALUE '19000101'.
    DATA s(16) TYPE p.
    s = ( idate - low_date ) * 86400 + itime ##NUMBER_OK.
    s = s + stdaz * 3600 ##NUMBER_OK.
    edate = low_date + ( s DIV 86400 ) ##NUMBER_OK.
    etime = s MOD 86400 ##NUMBER_OK.
  ENDMETHOD.


  METHOD get_day_in_week.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the day in week
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA day TYPE scal-indicator.

    ASSIGN day_in_week_cache[ datum = date ]
           TO FIELD-SYMBOL(<day_in_week>).

    IF sy-subrc <> 0.
      DATA(cache) = VALUE day_in_week_dict( datum = date ).

      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = cache-datum
        IMPORTING
          day  = day.

      cache-day = day.
      INSERT cache INTO TABLE day_in_week_cache ASSIGNING <day_in_week>.
    ENDIF.

    result = <day_in_week>-day.
  ENDMETHOD.


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
