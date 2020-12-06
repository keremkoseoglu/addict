*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_YTADDICT_CLASS
*   generation date: 06.12.2020 at 12:24:20
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_YTADDICT_CLASS     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
