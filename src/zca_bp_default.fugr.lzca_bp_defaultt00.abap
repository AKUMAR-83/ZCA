*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCA_BP_DEFAULT..................................*
DATA:  BEGIN OF STATUS_ZCA_BP_DEFAULT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCA_BP_DEFAULT                .
CONTROLS: TCTRL_ZCA_BP_DEFAULT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCA_BP_DEFAULT                .
TABLES: ZCA_BP_DEFAULT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
