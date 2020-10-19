*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.10.2020 at 15:35:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YTADDICT_SYDEF..................................*
DATA:  BEGIN OF STATUS_YTADDICT_SYDEF                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YTADDICT_SYDEF                .
CONTROLS: TCTRL_YTADDICT_SYDEF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YTADDICT_SYDEF                .
TABLES: YTADDICT_SYDEF                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
