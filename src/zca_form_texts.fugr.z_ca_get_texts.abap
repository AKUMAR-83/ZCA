FUNCTION z_ca_get_texts.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IV_LANGU) TYPE  SPRAS
*"  EXPORTING
*"     REFERENCE(ES_TXT_TRANS) TYPE  ZCA_TEXTS_STR
*"----------------------------------------------------------------------

***********************************************************************
* Development       : Rondo Ganahl
* Program Name      : Z_CA_GET_TEXTS
* Description       : Function to get translations form ZCA_TEXTS table
* Author            : Trong Nam Le (trongnamle), Itelligence AG
* Date              : 27.03.2018
* NetWeaver Release : S/4HANA EM 1610
*======================================================================
* Package           : ZCA
* Function Group    : ZCA_FORM_TEXTS
*----------------------------------------------------------------------
* History of Change
* 27.03.2018| trongnamle | initial created
***********************************************************************
*: Bitte kein Doppelpunkt am Ende des Textes in der Tabelle pflegen!!!
*: Bitte kein Doppelpunkt am Ende des Textes in der Tabelle pflegen!!!

  DATA: lt_txt_trans TYPE TABLE OF zca_texts,
        ls_txt_trans TYPE zca_texts.
  DATA: lt_txt_trans_en TYPE TABLE OF zca_texts,
        ls_txt_trans_en TYPE zca_texts.

  FIELD-SYMBOLS: <fs_fieldname> TYPE any.

  CLEAR: es_txt_trans.

  " Select texts in requested and english language
  SELECT * FROM zca_texts INTO CORRESPONDING FIELDS OF TABLE lt_txt_trans
    WHERE spras = iv_langu.
  SELECT * FROM zca_texts INTO CORRESPONDING FIELDS OF TABLE lt_txt_trans_en
    WHERE spras = 'EN'.

  " Fill the structure in english first. we assume that english is maintained at 100%
  LOOP AT lt_txt_trans_en INTO ls_txt_trans_en.
    ASSIGN COMPONENT ls_txt_trans_en-varname
    OF STRUCTURE es_txt_trans TO <fs_fieldname>.
    IF sy-subrc = 0.
      <fs_fieldname>  = ls_txt_trans_en-varvalue.
    ENDIF.
  ENDLOOP.

  " Fill the structure with the texts in the requested language
  LOOP AT lt_txt_trans INTO ls_txt_trans.
    ASSIGN COMPONENT ls_txt_trans-varname
    OF STRUCTURE es_txt_trans TO <fs_fieldname>.
    IF sy-subrc = 0.
      <fs_fieldname>  = ls_txt_trans-varvalue.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
