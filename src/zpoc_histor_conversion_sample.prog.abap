*&---------------------------------------------------------------------*
*& Report  ZPOC_HISTOR_CONVERSION_SAMPLE
*&---------------------------------------------------------------------*
*& creates POB event data for post O2C processes, using definition O2C_HANA
*& open issues:
*& - different types of predecessor documents
*& - change document mapping to tasks.
*&---------------------------------------------------------------------*
REPORT zpoc_histor_conversion_sample.

TABLES: tvak.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-tb1. " Parameter
  " Originally fd_auart was a Parameter
  " See event INITIALIZATION for default values
  SELECT-OPTIONS: fd_auart FOR tvak-auart.    " sales order type

  PARAMETERS: fd_start TYPE datum,   " order creation date start
              fd_end   TYPE datum,   " order creation data end
              fd_item  TYPE flag,    " include item events
              fs_cd    TYPE flag.    " include change documents
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-tb2. " Simulation
  PARAMETERS: p_sim AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK block2.

*----------------------------------------------------------------------*
*       CLASS reproc DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_o2c_historic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: main.
  PRIVATE SECTION.
    CONSTANTS:
      "   of business object types used for events, see POC_FACADE
      mc_so           TYPE poc_bo_type VALUE '114',      " sales order
      mc_delivery     TYPE poc_bo_type VALUE '73',       " outbound delivery
      mc_goods_issue  TYPE poc_bo_type VALUE '467B',     " goods issue
      mc_billing      TYPE poc_bo_type VALUE '28',       " customer invoice
      "   of event types (= task types) used for events, see POC_FACADE
      mc_evt_crea     TYPE poc_ba_type VALUE '21',       " create
      mc_evt_upd      TYPE poc_ba_type VALUE '88',       " update
      mc_evt_crea_itm TYPE poc_ba_type VALUE '901',      " create item
      mc_evt_upd_itm  TYPE poc_ba_type VALUE '903'.      " update item

    CLASS-DATA:
      mv_logsys    TYPE logsys,
      mv_tzonesys  TYPE tznzonesys,
      mt_poc_event TYPE poc_t_event,
      mt_so_hdr    TYPE TABLE OF vbak,
      mt_so_itm    TYPE TABLE OF vbap,
      mt_del_hdr   TYPE TABLE OF likp,
      mt_del_itm   TYPE TABLE OF lips,
      mt_bil_hdr   TYPE TABLE OF vbrk,
      mt_bil_itm   TYPE TABLE OF vbrp,
      mt_gi_hdr    TYPE TABLE OF mkpf,
      m_cursor     TYPE cursor.

    CLASS-METHODS:
      initialize,
      clean_all,
      read_o2c_data,
      screen_output,
      create_sales_order_events,
      create_delivery_events,
      create_goods_issue_events,
      create_billing_events,
      raise_events,
      add_gi_hdr_event IMPORTING it_gi    TYPE table
                       CHANGING  ct_event TYPE table,
      add_cd_events IMPORTING it_hdr     TYPE table
                              iv_cd_type TYPE cdobjectcl
                              iv_bo_type TYPE poc_bo_type
                    CHANGING  ct_event   TYPE table,
      add_sd_hdr_event IMPORTING it_hdr         TYPE table
                                 it_itm         TYPE table
                                 iv_bo_type     TYPE poc_bo_type
                                 iv_predecessor TYPE poc_bo_type
                       CHANGING  ct_event       TYPE table,

      add_sd_item_event IMPORTING it_hdr         TYPE table
                                  it_itm         TYPE table
                                  iv_bo_type     TYPE poc_bo_type
                                  iv_predecessor TYPE poc_bo_type
                        CHANGING  ct_event       TYPE table
                        .

ENDCLASS.                    "reproc DEFINITION


*----------------------------------------------------------------------*
*       CLASS reproc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_o2c_historic IMPLEMENTATION.
  METHOD initialize.

* logsys
    SELECT SINGLE logsys FROM t000 INTO mv_logsys WHERE mandt = sy-mandt.

    SELECT SINGLE tzonesys FROM ttzcu INTO mv_tzonesys.

* cursor for sales order selection, dependent on sales document type
    IF fd_auart IS INITIAL.
      OPEN CURSOR WITH HOLD m_cursor FOR
        SELECT vbeln vbtyp erdat erzet ernam auart FROM vbak
        WHERE erdat BETWEEN fd_start AND fd_end
        AND vbtyp = 'C'.
    ELSE.
      OPEN CURSOR WITH HOLD m_cursor FOR
        SELECT vbeln vbtyp erdat erzet ernam auart FROM vbak
        WHERE erdat BETWEEN fd_start AND fd_end
        AND auart IN fd_auart
        AND vbtyp = 'C'.
    ENDIF.
  ENDMETHOD.                    "initialize


  METHOD clean_all.
    REFRESH mt_poc_event.
    REFRESH mt_so_hdr.
    REFRESH mt_so_itm.
    REFRESH mt_del_hdr.
    REFRESH mt_del_itm.
    REFRESH mt_gi_hdr.
  ENDMETHOD.      "initialize


  METHOD read_o2c_data.
    TYPES:
      BEGIN OF ty_del_itm_hlp,
        lfbnr TYPE lfbnr,
        lfpos TYPE lfpos,
      END OF ty_del_itm_hlp,
      BEGIN OF ty_bi_hlp,
        awkey TYPE awkey,
      END OF ty_bi_hlp.

    DATA:
      mt_del_itm_hlp TYPE TABLE OF ty_del_itm_hlp,
      ls_del_itm_hlp TYPE ty_del_itm_hlp,
      mt_bi_hlp      TYPE TABLE OF ty_bi_hlp,
      ls_bi_hlp      TYPE ty_bi_hlp.

    DATA:
      ls_so_hdr  TYPE vbak,
      ls_so_itm  TYPE vbap,
      ls_del_hdr TYPE likp,
      ls_del_itm TYPE lips,
      ls_gi_hdr  TYPE mkpf,
      ls_gi_itm  TYPE mseg,
      ls_bi_hdr  TYPE vbrk,
      ls_bi_itm  TYPE vbrp.
    DATA lv_count TYPE i.

* select sales orders


    FETCH NEXT CURSOR m_cursor
        INTO CORRESPONDING FIELDS OF TABLE mt_so_hdr
        PACKAGE SIZE 500.

    IF sy-subrc NE 0.
      CLOSE CURSOR m_cursor.
    ELSE.


      DESCRIBE TABLE mt_so_hdr LINES lv_count.
      IF lv_count > 0.
        SELECT vbeln posnr erdat erzet ernam matnr vgbel vgpos FROM vbap INTO CORRESPONDING FIELDS OF TABLE mt_so_itm
        FOR ALL ENTRIES IN mt_so_hdr
        WHERE vbeln = mt_so_hdr-vbeln.
      ENDIF.


* delivery
      DESCRIBE TABLE mt_so_itm LINES lv_count.
      IF lv_count > 0.
        SELECT vbeln posnr erdat erzet ernam matnr vgbel vgpos FROM lips INTO CORRESPONDING FIELDS OF TABLE mt_del_itm
        FOR ALL ENTRIES IN mt_so_itm
        WHERE vgbel = mt_so_itm-vbeln
        AND vgpos = mt_so_itm-posnr.
      ENDIF.

      DESCRIBE TABLE mt_del_itm LINES lv_count.
      IF lv_count > 0.
        SELECT vbeln erdat erzet ernam lfart FROM likp INTO CORRESPONDING FIELDS OF TABLE mt_del_hdr
        FOR ALL ENTRIES IN mt_del_itm
        WHERE vbeln = mt_del_itm-vbeln.
      ENDIF.

      LOOP AT mt_del_itm INTO ls_del_itm.
        ls_del_itm_hlp-lfbnr = ls_del_itm-vbeln.
        ls_del_itm_hlp-lfpos = ls_del_itm-posnr.
        APPEND ls_del_itm_hlp TO mt_del_itm_hlp.
      ENDLOOP.

* goods issue
      DESCRIBE TABLE mt_del_hdr LINES lv_count.
      IF lv_count > 0.
        SELECT mblnr mjahr cpudt cputm usnam le_vbeln tcode FROM mkpf INTO CORRESPONDING FIELDS OF TABLE mt_gi_hdr
        FOR ALL ENTRIES IN mt_del_hdr
        WHERE le_vbeln = mt_del_hdr-vbeln
        AND vgart = 'WL'.
      ENDIF.

* billing
      DESCRIBE TABLE mt_so_itm LINES lv_count.
      IF lv_count > 0.
        SELECT vbeln posnr erdat erzet ernam matnr aubel aupos FROM vbrp INTO CORRESPONDING FIELDS OF TABLE mt_bil_itm
        FOR ALL ENTRIES IN mt_so_itm
        WHERE aubel = mt_so_itm-vbeln
        AND aupos = mt_so_itm-posnr.
      ENDIF.

      DESCRIBE TABLE mt_bil_itm LINES lv_count.
      IF lv_count > 0.
        SELECT vbeln erdat erzet ernam fkart FROM vbrk INTO CORRESPONDING FIELDS OF TABLE mt_bil_hdr
        FOR ALL ENTRIES IN mt_bil_itm
        WHERE vbeln = mt_bil_itm-vbeln.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "read_o2c_data


  METHOD screen_output.
    DATA:
      ls_so_hdr  TYPE vbak,
      ls_so_itm  TYPE vbap,
      ls_inq_hdr TYPE vbak,
      ls_inq_itm TYPE vbap,
      ls_quo_hdr TYPE vbak,
      ls_quo_itm TYPE vbap,
      ls_del_hdr TYPE likp,
      ls_del_itm TYPE lips,
      ls_bil_hdr TYPE vbrk,
      ls_bil_itm TYPE vbrp,
      ls_gi_hdr  TYPE mkpf,
      ls_gi_itm  TYPE mseg,
      ls_bi_hdr  TYPE vbrk,
      ls_bi_itm  TYPE vbrp,
      ls_acc     TYPE bkpf.


    IF mt_so_hdr IS NOT INITIAL.
      IF p_sim IS INITIAL.
        WRITE: / 'CREATE EVENTS',
               / '============='.
      ELSE.
        WRITE: / 'SIMLUATION',
               / '=========='.
      ENDIF.


      LOOP AT mt_so_hdr INTO ls_so_hdr.
        AT NEW vbeln.
          SKIP.
        ENDAT.

        WRITE: /'Sales Order        :'.
        WRITE:  ls_so_hdr-vbeln, '      ', ls_so_hdr-erdat, ls_so_hdr-erzet, ls_so_hdr-ernam.
        " Event Sales Order Create

        LOOP AT mt_so_itm INTO ls_so_itm WHERE vbeln = ls_so_hdr-vbeln.
          WRITE: / 'Sales Order Item   :'.
          WRITE:  ls_so_itm-vbeln, ls_so_itm-posnr, ls_so_itm-erdat, ls_so_itm-erzet, ls_so_itm-ernam, ls_so_itm-matnr.
          " Event Sales Order Item Create


          LOOP AT mt_del_itm INTO ls_del_itm
               WHERE vgbel = ls_so_itm-vbeln
               AND vgpos = ls_so_itm-posnr.
            LOOP AT mt_del_hdr INTO ls_del_hdr
            WHERE vbeln = ls_del_itm-vbeln.
              WRITE: /'  Delivery         :'.
              WRITE:  ls_del_hdr-vbeln, '      ', ls_del_hdr-erdat, ls_del_hdr-erzet, ls_del_hdr-ernam.

              LOOP AT mt_gi_hdr INTO ls_gi_hdr
              WHERE le_vbeln = ls_del_hdr-vbeln.

                WRITE: /'      Goods Issue  :'.
                WRITE:  ls_gi_hdr-mblnr, ls_gi_hdr-mjahr, ' ', ls_gi_hdr-cpudt, ls_gi_hdr-cputm, ls_gi_hdr-usnam.
              ENDLOOP.
            ENDLOOP.
            WRITE: /'    Delivery       :'.
            WRITE:  ls_del_itm-vbeln, ls_del_itm-posnr, ls_del_itm-erdat, ls_del_itm-erzet, ls_del_itm-ernam, ls_del_itm-matnr.
          ENDLOOP.

          LOOP AT mt_bil_itm INTO ls_bil_itm
                WHERE aubel = ls_so_itm-vbeln
                AND   aupos = ls_so_itm-posnr.

            LOOP AT mt_bil_hdr INTO ls_bil_hdr
                 WHERE vbeln = ls_bil_itm-vbeln.
              WRITE: /'  Billing          :'.
              WRITE:  ls_bil_hdr-vbeln, '      ', ls_bil_hdr-erdat, ls_bil_hdr-erzet, ls_bil_hdr-ernam.
            ENDLOOP.
            WRITE: /'  Billing Item     :'.
            WRITE:  ls_bil_itm-vbeln, ls_bil_itm-posnr, ls_bil_itm-erdat, ls_bil_itm-erzet, ls_bil_itm-ernam, ls_bil_itm-matnr.
          ENDLOOP.

        ENDLOOP.
      ENDLOOP.
      ULINE.
    ENDIF.
  ENDMETHOD.                    " screen_output


  METHOD create_sales_order_events.
    add_sd_hdr_event( EXPORTING it_hdr         = mt_so_hdr
                                it_itm         = mt_so_itm
                                iv_bo_type     = mc_so
                                iv_predecessor = space
                      CHANGING  ct_event       = mt_poc_event ).
    add_sd_item_event( EXPORTING it_hdr         = mt_so_hdr
                                 it_itm         = mt_so_itm
                                 iv_bo_type     = mc_so
                                 iv_predecessor = space
                       CHANGING  ct_event       = mt_poc_event ).
* SO change documents
    add_cd_events( EXPORTING it_hdr     = mt_so_hdr
                             iv_cd_type = 'VERKBELEG'
                             iv_bo_type = mc_so
                   CHANGING  ct_event   = mt_poc_event ).
  ENDMETHOD.                    "create_sales_order_events


  METHOD create_delivery_events.
    add_sd_hdr_event( EXPORTING it_hdr         = mt_del_hdr
                                it_itm         = mt_del_itm
                                iv_bo_type     = mc_delivery
                                iv_predecessor = mc_so
                      CHANGING  ct_event       = mt_poc_event ).
    add_sd_item_event( EXPORTING it_hdr         = mt_del_hdr  "               mt_so_hdr
                                 it_itm         = mt_del_itm
                                 iv_bo_type     = mc_delivery
                                 iv_predecessor = mc_so
                       CHANGING  ct_event       = mt_poc_event ).
* Delivery Change Documents
    add_cd_events( EXPORTING it_hdr     = mt_del_hdr
                             iv_cd_type = 'LIEFERUNG'
                             iv_bo_type = mc_delivery
                   CHANGING  ct_event   = mt_poc_event ).
  ENDMETHOD.                    "create_delivery_events(


  METHOD create_billing_events.
    add_sd_hdr_event( EXPORTING it_hdr         = mt_bil_hdr
                                it_itm         = mt_bil_itm
                                iv_bo_type     = mc_billing
                                iv_predecessor = mc_so
                      CHANGING  ct_event       = mt_poc_event ).
    add_sd_item_event( EXPORTING it_hdr         = mt_bil_hdr
                                 it_itm         = mt_bil_itm
                                 iv_bo_type     = mc_billing
                                 iv_predecessor = mc_so
                       CHANGING  ct_event       = mt_poc_event ).
* Billing Change Documents
    add_cd_events( EXPORTING it_hdr     = mt_bil_hdr
                             iv_cd_type = 'FAKTBELEG'
                             iv_bo_type = mc_billing
                   CHANGING  ct_event   = mt_poc_event ).
  ENDMETHOD.                    "create_billing_events(


  METHOD create_goods_issue_events.
    add_gi_hdr_event(
      EXPORTING
        it_gi    = mt_gi_hdr
      CHANGING
        ct_event = mt_poc_event ).
  ENDMETHOD.              "create_delivery_events(


  METHOD raise_events.
    SORT mt_poc_event BY executed_at item_id ASCENDING.
    CALL FUNCTION 'POC_RAISE_EVENT'
      EXPORTING
        it_event  = mt_poc_event
        iv_commit = abap_false.
  ENDMETHOD.                    "raise_events


  METHOD add_gi_hdr_event.
    DATA: ls_poc_event TYPE poc_s_event,
          lt_pre_bo    TYPE poc_t_pre_bo_event,
          ls_pre_bo    TYPE poc_s_pre_bo_event.
    DATA  ls_gi_hdr TYPE mkpf.
    DATA lv_timestring TYPE char21.

    LOOP AT it_gi INTO ls_gi_hdr.
      REFRESH lt_pre_bo.
      CLEAR ls_poc_event.

      " goods issue created event
      ls_poc_event-event_type = mc_evt_crea.
      ls_poc_event-bo_type = mc_goods_issue.
      ls_poc_event-bo_id = ls_gi_hdr-mblnr.
      IF ls_gi_hdr-tcode IS NOT INITIAL.
        ls_poc_event-cbe_type = ls_gi_hdr-tcode.
        ls_poc_event-cbe_category = '01'.
      ENDIF.
      ls_poc_event-executed_by = ls_gi_hdr-usnam.
      CONVERT DATE ls_gi_hdr-cpudt TIME ls_gi_hdr-cputm
      INTO TIME STAMP ls_poc_event-executed_at TIME ZONE mv_tzonesys.   "  'UTC   '.
      lv_timestring = ls_poc_event-executed_at.
      CONCATENATE mv_logsys lv_timestring INTO ls_poc_event-transaction_id SEPARATED BY '-'.
      CLEAR ls_pre_bo.
      ls_pre_bo-pre_bo_type = mc_delivery.
      ls_pre_bo-pre_bo_id = ls_gi_hdr-le_vbeln.
      APPEND ls_pre_bo TO lt_pre_bo.
      APPEND LINES OF lt_pre_bo TO ls_poc_event-previous_bo.
      APPEND ls_poc_event TO ct_event.
    ENDLOOP.
  ENDMETHOD.                    " add_gi_hdr_event


  METHOD add_cd_events.
    FIELD-SYMBOLS <ls_hdr> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <auart> TYPE auart.
    FIELD-SYMBOLS <lfart> TYPE lfart.
    FIELD-SYMBOLS <fkart> TYPE fkart.

    TYPES:
      BEGIN OF ty_cd_hdr_hlp,
        objectid TYPE cdobjectv,
        auart    TYPE auart,
        lfart    TYPE lfart,
        fkart    TYPE fkart,
      END OF ty_cd_hdr_hlp.

    DATA: ls_poc_event TYPE poc_s_event,
          lt_pre_bo    TYPE poc_t_pre_bo_event,
          ls_pre_bo    TYPE poc_s_pre_bo_event.
    DATA: lt_cdhdr     TYPE TABLE OF cdhdr,
          ls_cdhdr     TYPE cdhdr,
          lt_cdpos     TYPE TABLE OF cdpos,
          ls_cdpos     TYPE cdpos,
          lt_cdhdr_hlp TYPE TABLE OF ty_cd_hdr_hlp,
          ls_cdhdr_hlp TYPE ty_cd_hdr_hlp.
    DATA lv_timestring TYPE char21.

    IF fs_cd IS NOT INITIAL AND it_hdr[] IS NOT INITIAL.
      REFRESH lt_cdhdr_hlp.
      LOOP AT it_hdr ASSIGNING <ls_hdr>.
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_hdr> TO <field>.
        ASSIGN COMPONENT 'AUART' OF STRUCTURE <ls_hdr> TO <auart>.
        ASSIGN COMPONENT 'LFART' OF STRUCTURE <ls_hdr> TO <lfart>.
        ASSIGN COMPONENT 'FKART' OF STRUCTURE <ls_hdr> TO <fkart>.

        ls_cdhdr_hlp-objectid = <field>.
        IF <auart> IS ASSIGNED.
          ls_cdhdr_hlp-auart = <auart>.
        ENDIF.
        IF <lfart> IS ASSIGNED.
          ls_cdhdr_hlp-lfart = <lfart>.
        ENDIF.
        IF <fkart> IS ASSIGNED.
          ls_cdhdr_hlp-fkart = <fkart>.
        ENDIF.

        APPEND ls_cdhdr_hlp TO lt_cdhdr_hlp.
      ENDLOOP.

      REFRESH lt_cdhdr.
      REFRESH lt_cdpos.

      " read from change documents
      IF lt_cdhdr_hlp IS NOT INITIAL.
        SELECT objectid username udate utime tcode changenr FROM cdhdr INTO CORRESPONDING FIELDS OF TABLE lt_cdhdr
           FOR ALL ENTRIES IN lt_cdhdr_hlp
           WHERE objectclas = iv_cd_type
        AND objectid = lt_cdhdr_hlp-objectid
        AND change_ind = 'U'.

        SELECT objectid changenr tabname tabkey FROM cdpos INTO CORRESPONDING FIELDS OF TABLE lt_cdpos
          FOR ALL ENTRIES IN lt_cdhdr_hlp
          WHERE objectclas = iv_cd_type
        AND objectid = lt_cdhdr_hlp-objectid
        AND chngind = 'U'.
      ENDIF.

      LOOP AT lt_cdhdr INTO ls_cdhdr.
        DATA ls_parameter TYPE poc_s_parameter_name_value.

        CLEAR ls_poc_event.
        " update event
        ls_poc_event-event_type = mc_evt_upd.
        ls_poc_event-bo_type = iv_bo_type.
        ls_poc_event-bo_id = ls_cdhdr-objectid.
        ls_poc_event-executed_by = ls_cdhdr-username.
        CONVERT DATE ls_cdhdr-udate TIME ls_cdhdr-utime
        INTO TIME STAMP ls_poc_event-executed_at TIME ZONE mv_tzonesys. "   'UTC   '.
        lv_timestring = ls_poc_event-executed_at.
        CONCATENATE mv_logsys lv_timestring INTO ls_poc_event-transaction_id SEPARATED BY '-'.
        IF ls_cdhdr-tcode IS NOT INITIAL.
          ls_poc_event-cbe_type = ls_cdhdr-tcode.
          ls_poc_event-cbe_category = '01'.
        ENDIF.

        " add AUART / LFART / FKART for item update
        READ TABLE lt_cdhdr_hlp INTO ls_cdhdr_hlp WITH KEY objectid = ls_poc_event-bo_id.
        IF sy-subrc = 0.
          IF ls_cdhdr_hlp-auart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'AUART'.
            ls_parameter-parameter_value = ls_cdhdr_hlp-auart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ELSEIF ls_cdhdr_hlp-lfart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'LFART'.
            ls_parameter-parameter_value = ls_cdhdr_hlp-lfart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ELSEIF ls_cdhdr_hlp-fkart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'FKART'.
            ls_parameter-parameter_value = ls_cdhdr_hlp-fkart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ENDIF.
        ENDIF.

        APPEND ls_poc_event TO ct_event.

        LOOP AT lt_cdpos INTO ls_cdpos
        WHERE objectid = ls_cdhdr-objectid
        AND changenr = ls_cdhdr-changenr
        AND ( tabname = 'VBAP' OR tabname = 'VBEP'
              OR tabname = 'LIKPS'                         " delivery
              OR tabname = 'VBRP').                         " invoice

          " item update event
          ls_poc_event-event_type = mc_evt_upd_itm.
          ls_poc_event-item_id = ls_cdpos-tabkey+13(6).
          APPEND ls_poc_event TO ct_event.

        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    " add_cd_events


  METHOD add_sd_hdr_event.
    FIELD-SYMBOLS <ls_hdr> TYPE any.
    FIELD-SYMBOLS <ls_itm> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <field2> TYPE any.
    FIELD-SYMBOLS <field3> TYPE any.
    FIELD-SYMBOLS <field4> TYPE any.
    FIELD-SYMBOLS <field5> TYPE any.
    FIELD-SYMBOLS <field6> TYPE any.
    FIELD-SYMBOLS <field7> TYPE any.
    FIELD-SYMBOLS <field8> TYPE any.
    FIELD-SYMBOLS <field9> TYPE any.
    FIELD-SYMBOLS <field10> TYPE any.

    DATA: ls_poc_event TYPE poc_s_event,
          lt_pre_bo    TYPE poc_t_pre_bo_event,
          ls_pre_bo    TYPE poc_s_pre_bo_event,
          ls_parameter TYPE poc_s_parameter_name_value.

    DATA lv_timestring TYPE char21.

    DATA lv_erdat_old TYPE erdat.
    DATA lv_erzet_old TYPE erzet.


    " loop at headers
    LOOP AT it_hdr ASSIGNING <ls_hdr>.

      CLEAR ls_poc_event.
      REFRESH lt_pre_bo.

      ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_hdr> TO <field>.
      ASSIGN COMPONENT 'ERDAT' OF STRUCTURE <ls_hdr> TO <field2>.
      ASSIGN COMPONENT 'ERZET' OF STRUCTURE <ls_hdr> TO <field3>.
      ASSIGN COMPONENT 'AUART' OF STRUCTURE <ls_hdr> TO <field8>.
      ASSIGN COMPONENT 'LFART' OF STRUCTURE <ls_hdr> TO <field9>.
      ASSIGN COMPONENT 'FKART' OF STRUCTURE <ls_hdr> TO <field10>.

      " get predecessors from items
      LOOP AT it_itm ASSIGNING <ls_itm>.

        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_itm> TO <field5>.
        ASSIGN COMPONENT 'ERDAT' OF STRUCTURE <ls_itm> TO <field6>.
        ASSIGN COMPONENT 'ERZET' OF STRUCTURE <ls_itm> TO <field7>.

        IF <field5> = <field> AND <field6> = <field2> AND <field7> = <field3>.

          ASSIGN COMPONENT 'VGBEL' OF STRUCTURE <ls_itm> TO <field4>.
          IF <field4> IS NOT INITIAL AND iv_predecessor NE space.
            CLEAR ls_pre_bo.
            ls_pre_bo-pre_bo_type = iv_predecessor.
            ls_pre_bo-pre_bo_id = <field4>.
            APPEND ls_pre_bo TO lt_pre_bo.        "dont like
          ENDIF.
          ASSIGN COMPONENT 'AUBEL' OF STRUCTURE <ls_itm> TO <field4>.
          IF <field4> IS NOT INITIAL AND iv_predecessor NE space.
            CLEAR ls_pre_bo.
            ls_pre_bo-pre_bo_type = iv_predecessor.
            ls_pre_bo-pre_bo_id = <field4>.
            APPEND ls_pre_bo TO lt_pre_bo.        "dont like
          ENDIF.

        ENDIF.

      ENDLOOP.

      " create event - header
      ls_poc_event-event_type = mc_evt_crea.
      ls_poc_event-bo_type = iv_bo_type.
      ls_poc_event-bo_id = <field>.
      ASSIGN COMPONENT 'TCODE' OF STRUCTURE <ls_hdr> TO <field4>.
      IF sy-subrc = 0.
        IF <field4> IS NOT INITIAL.
          ls_poc_event-cbe_type = <field4>.
          ls_poc_event-cbe_category = '01'.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT 'ERNAM' OF STRUCTURE <ls_hdr> TO <field4>.
      ls_poc_event-executed_by = <field4>.
      CONVERT DATE <field2> TIME <field3>
          INTO TIME STAMP ls_poc_event-executed_at TIME ZONE mv_tzonesys. "   'UTC   '.
      lv_timestring = ls_poc_event-executed_at.
      CONCATENATE mv_logsys lv_timestring INTO ls_poc_event-transaction_id SEPARATED BY '-'.

      APPEND LINES OF lt_pre_bo TO ls_poc_event-previous_bo.

      " AUART / LFART as parameter for create event
      CLEAR ls_parameter.
      IF <field8> IS ASSIGNED AND <field8> IS NOT INITIAL.
        ls_parameter-parameter_name = 'AUART'.
        ls_parameter-parameter_value = <field8>.
        APPEND ls_parameter TO ls_poc_event-parameter_data.
      ELSEIF <field9> IS ASSIGNED AND <field9> IS NOT INITIAL.
        ls_parameter-parameter_name = 'LFART'.
        ls_parameter-parameter_value = <field9>.
        APPEND ls_parameter TO ls_poc_event-parameter_data.
      ELSEIF <field10> IS ASSIGNED AND <field10> IS NOT INITIAL.
        ls_parameter-parameter_name = 'FKART'.
        ls_parameter-parameter_value = <field10>.
        APPEND ls_parameter TO ls_poc_event-parameter_data.
      ENDIF.

      APPEND ls_poc_event TO ct_event.

    ENDLOOP.
  ENDMETHOD.                    "add_sd_hdr_event


  METHOD add_sd_item_event.
    TYPES:
      BEGIN OF ty_hdr_hlp,
        vbeln TYPE vbeln,
        auart TYPE auart,
        lfart TYPE lfart,
        fkart TYPE fkart,
      END OF ty_hdr_hlp.

    FIELD-SYMBOLS  <ls_hdr> TYPE any.
    FIELD-SYMBOLS  <ls_itm> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <field2> TYPE any.
    FIELD-SYMBOLS <field3> TYPE any.
    FIELD-SYMBOLS <auart> TYPE any.
    FIELD-SYMBOLS <lfart> TYPE any.
    FIELD-SYMBOLS <fkart> TYPE any.

    DATA: lt_hdr_hlp TYPE TABLE OF ty_hdr_hlp,
          ls_hdr_hlp TYPE ty_hdr_hlp.


    DATA: ls_poc_event TYPE poc_s_event,
          lt_pre_bo    TYPE poc_t_pre_bo_event,
          ls_pre_bo    TYPE poc_s_pre_bo_event,
          ls_parameter TYPE poc_s_parameter_name_value.

    DATA lv_timestring TYPE char21.


    IF fd_item IS NOT INITIAL AND it_hdr[] IS NOT INITIAL.

      REFRESH lt_hdr_hlp.
      LOOP AT it_hdr ASSIGNING <ls_hdr>.
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_hdr> TO <field>.
        ASSIGN COMPONENT 'AUART' OF STRUCTURE <ls_hdr> TO <auart>.
        ASSIGN COMPONENT 'LFART' OF STRUCTURE <ls_hdr> TO <lfart>.
        ASSIGN COMPONENT 'FKART' OF STRUCTURE <ls_hdr> TO <fkart>.
        ls_hdr_hlp-vbeln = <field>.
        IF <auart> IS ASSIGNED.
          ls_hdr_hlp-auart = <auart>.
        ENDIF.
        IF <lfart> IS ASSIGNED.
          ls_hdr_hlp-lfart = <lfart>.
        ENDIF.
        IF <fkart> IS ASSIGNED.
          ls_hdr_hlp-fkart = <fkart>.
        ENDIF.
        APPEND ls_hdr_hlp TO lt_hdr_hlp.
      ENDLOOP.

      " loop over items
      LOOP AT it_itm ASSIGNING <ls_itm>.

        REFRESH lt_pre_bo.
        CLEAR ls_poc_event.

        " item create event
        ls_poc_event-event_type = mc_evt_crea_itm.
        ls_poc_event-bo_type = iv_bo_type.
        ASSIGN ls_poc_event-bo_id TO <field>.
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_itm> TO <field>.
        ls_poc_event-bo_id = <field>.
        ASSIGN COMPONENT 'POSNR' OF STRUCTURE <ls_itm> TO <field>.
        ls_poc_event-item_id = <field>.
        ASSIGN COMPONENT 'ERNAM' OF STRUCTURE <ls_itm> TO <field>.
        ls_poc_event-executed_by = <field>.

        ASSIGN COMPONENT 'ERDAT' OF STRUCTURE <ls_itm> TO <field>.
        ASSIGN COMPONENT 'ERZET' OF STRUCTURE <ls_itm> TO <field2>.
        CONVERT DATE <field> TIME <field2>
        INTO TIME STAMP ls_poc_event-executed_at TIME ZONE mv_tzonesys. "   'UTC   '.
        lv_timestring = ls_poc_event-executed_at.
        CONCATENATE mv_logsys lv_timestring INTO ls_poc_event-transaction_id SEPARATED BY '-'.
        CLEAR ls_pre_bo.

        " create predecessors
        ASSIGN COMPONENT 'VGBEL' OF STRUCTURE <ls_itm> TO <field>.
        IF <field> IS NOT INITIAL AND iv_predecessor NE space.
          ls_pre_bo-pre_bo_type = iv_predecessor.
          ls_pre_bo-pre_bo_id = <field>.
          ASSIGN COMPONENT 'VGPOS' OF STRUCTURE <ls_itm> TO <field>.
          ls_pre_bo-pre_item_id = <field>.
          APPEND ls_pre_bo TO lt_pre_bo.
        ENDIF.
        ASSIGN COMPONENT 'AUBEL' OF STRUCTURE <ls_itm> TO <field>.
        IF <field> IS NOT INITIAL AND iv_predecessor NE space.
          ls_pre_bo-pre_bo_type = iv_predecessor.
          ls_pre_bo-pre_bo_id = <field>.
          ASSIGN COMPONENT 'AUPOS' OF STRUCTURE <ls_itm> TO <field>.
          ls_pre_bo-pre_item_id = <field>.
          APPEND ls_pre_bo TO lt_pre_bo.
        ENDIF.

        CLEAR ls_pre_bo.
        ls_pre_bo-pre_bo_type = iv_bo_type.      " dont like
        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_itm> TO <field>.
        ls_pre_bo-pre_bo_id = <field>.
        APPEND ls_pre_bo TO lt_pre_bo.              " dont like

        APPEND LINES OF lt_pre_bo TO ls_poc_event-previous_bo.

        " AUART / LFART  /FKART as parameter for create event
        READ TABLE lt_hdr_hlp INTO ls_hdr_hlp WITH KEY vbeln = ls_poc_event-bo_id.
        IF sy-subrc = 0.
          IF ls_hdr_hlp-auart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'AUART'.
            ls_parameter-parameter_value = ls_hdr_hlp-auart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ELSEIF ls_hdr_hlp-lfart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'LFART'.
            ls_parameter-parameter_value = ls_hdr_hlp-lfart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ELSEIF ls_hdr_hlp-fkart IS NOT INITIAL.
            CLEAR ls_parameter.
            ls_parameter-parameter_name = 'FKART'.
            ls_parameter-parameter_value = ls_hdr_hlp-fkart.
            APPEND ls_parameter TO ls_poc_event-parameter_data.
          ENDIF.
        ENDIF.

        APPEND ls_poc_event TO ct_event.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.                    "add_sd_item_event


  " add_sd_hdr_event
  METHOD main.

* initialize
    initialize( ).

    WHILE m_cursor IS NOT INITIAL.
* reset tables
      clean_all( ).

* read data
      read_o2c_data( ).
* screen output
      screen_output( ).

** create events
* Sales Order
      create_sales_order_events( ).
* Deliveries
      create_delivery_events( ).
* Goods Issues
      create_goods_issue_events( ).
* Billing
      create_billing_events( ).

      IF p_sim IS INITIAL.
* Sort and raise events
        raise_events( ).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.                    "main
ENDCLASS.                    "reproc IMPLEMENTATION


INITIALIZATION.
  CLEAR fd_auart.
  fd_auart-sign = 'I'.
  fd_auart-option = 'EQ'.
  fd_auart-low = 'TA'.
  APPEND fd_auart.
  fd_auart-low = 'L2'.
  APPEND fd_auart.
  fd_auart-low = 'G2'.
  APPEND fd_auart.


START-OF-SELECTION.
  cl_o2c_historic=>main( ).
