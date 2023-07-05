*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: YTADDICT_CLASS..................................*
DATA:  BEGIN OF STATUS_YTADDICT_CLASS                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YTADDICT_CLASS                .
CONTROLS: TCTRL_YTADDICT_CLASS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YTADDICT_CLASS                .
TABLES: YTADDICT_CLASS                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
