FUNCTION YF_ADDICT_DATE_CHECK_WORKDAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATE) LIKE  SY-DATUM
*"     VALUE(FACTORY_CALENDAR_ID) LIKE  SCAL-FCALID
*"     VALUE(MESSAGE_TYPE) LIKE  SY-MSGTY
*"  EXCEPTIONS
*"      DATE_AFTER_RANGE
*"      DATE_BEFORE_RANGE
*"      DATE_INVALID
*"      DATE_NO_WORKINGDAY
*"      FACTORY_CALENDAR_NOT_FOUND
*"      MESSAGE_TYPE_INVALID
*"----------------------------------------------------------------------
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " This function has shamelessly been copied from DATE_CHECK_WORKINGDAY
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  DATA: d10_str(10)     TYPE c,
        workingday_flag LIKE scal-indicator.

  IF message_type CN 'IWEAS'.
    RAISE message_type_invalid.
  ENDIF.

  IF factory_calendar_id <> space.
    WRITE date TO d10_str DD/MM/YYYY.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        date                       = date
        factory_calendar_id        = factory_calendar_id
      IMPORTING
        workingday_indicator       = workingday_flag
      EXCEPTIONS
        date_invalid               = 1
        date_before_range          = 2
        date_after_range           = 3
        factory_calendar_not_found = 4.
    CASE sy-subrc.
      WHEN 1.                          " Ungueltiges Datum
        MESSAGE e205(yaddict) WITH d10_str RAISING date_invalid.
      WHEN 2.                          " Datum vor Beginn Fabrikkalender
        MESSAGE i207(yaddict) WITH d10_str factory_calendar_id
                RAISING date_before_range.
      WHEN 3.                          " Datum nach Ende Fabrikkalender
        MESSAGE i208(yaddict) WITH d10_str factory_calendar_id
                RAISING date_after_range.
      WHEN 4.                          " Unbekannter Fabrikkalender
        MESSAGE i209(yaddict) WITH factory_calendar_id
                RAISING factory_calendar_not_found.
    ENDCASE.

    IF workingday_flag <> space.       " Kein Fabriktag
      MESSAGE ID 'T0' TYPE message_type NUMBER 206 WITH d10_str
              RAISING date_no_workingday.
    ENDIF.
  ENDIF.
ENDFUNCTION.
