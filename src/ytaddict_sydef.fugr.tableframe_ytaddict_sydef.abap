*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_YTADDICT_SYDEF
*   generation date: 19.10.2020 at 15:35:05
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_YTADDICT_SYDEF     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
