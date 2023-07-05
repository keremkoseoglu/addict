*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: YTADDICT_NSOBJ..................................*
DATA:  BEGIN OF STATUS_YTADDICT_NSOBJ                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YTADDICT_NSOBJ                .
CONTROLS: TCTRL_YTADDICT_NSOBJ
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YTADDICT_NSOBJ                .
TABLES: YTADDICT_NSOBJ                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
