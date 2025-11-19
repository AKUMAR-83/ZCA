*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCA_TEXTS.......................................*
DATA:  BEGIN OF STATUS_ZCA_TEXTS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCA_TEXTS                     .
CONTROLS: TCTRL_ZCA_TEXTS
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZMM_TEXTS.......................................*
DATA:  BEGIN OF STATUS_ZMM_TEXTS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_TEXTS                     .
CONTROLS: TCTRL_ZMM_TEXTS
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZCA_TEXTS                     .
TABLES: *ZMM_TEXTS                     .
TABLES: ZCA_TEXTS                      .
TABLES: ZMM_TEXTS                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
