*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_YTADDICT_NSOBJ
*   generation date: 29.10.2020 at 16:39:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_YTADDICT_NSOBJ     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
