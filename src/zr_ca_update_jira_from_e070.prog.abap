*----------------------------------------------------------------------
* Report: zr_ca_update_jira_from_e070
*
* Description: Helper program to update Jira issues with transport numbers
*
* creation date: 23.05.2024
*
* developer: Sebastian Kittel

* ----------------------------------------------------------------------
* Change / date
* ----- -------
* [change] [user] [date]
*
* ----------------------------------------------------------------------
* description
* this program runs every few minutes in batch mode and updates the Jira
* issues with the transports which where created in the x minutes before
* also the transport owner gets an information email if required
* the next step is to update the Jira with the production import status
* ----------------------------------------------------------------------
REPORT zr_ca_update_jira_from_e070.

TABLES e070.

CLASS lcl_jira_srv DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_result,
        trkorr     TYPE trkorr,
        trfunction TYPE trfunction,
        as4text    TYPE as4text,
        cre_user   TYPE as4user,
        cre_date   TYPE as4date,
        cre_time   TYPE as4time,
        reference  TYPE trvalue,
      END OF ty_result,

      tr_trkorr TYPE RANGE OF trkorr,

      tt_result TYPE TABLE OF ty_result.

*    CLASS-DATA:
*    gt_result TYPE TABLE OF ty_result.

    CLASS-METHODS select_data
      IMPORTING
        iv_dats   TYPE    dats
        iv_time   TYPE    tims
        it_trkorr TYPE    tr_trkorr OPTIONAL
      EXPORTING
        et_result TYPE    tt_result.

    CLASS-METHODS select_data_transp
      IMPORTING
                iv_dats          TYPE    dats
                iv_time          TYPE    tims
                it_trkorr        TYPE    tr_trkorr OPTIONAL
      RETURNING VALUE(et_result) TYPE    tmstpalogs.


    CLASS-METHODS send_mail
      IMPORTING
        it_result TYPE tmstpalogs
        iv_mlrec  TYPE so_dli_nam.

    CLASS-METHODS get_jira_from_attr
      IMPORTING iv_trkorr      TYPE trkorr
                iv_as4text     TYPE as4text
      RETURNING VALUE(ev_jira) TYPE string.

    CLASS-METHODS get_jira_from_text
      IMPORTING iv_as4text     TYPE as4text
      RETURNING VALUE(ev_jira) TYPE string.


  PRIVATE SECTION.
    CLASS-METHODS convert_timestamp
      IMPORTING
                iv_timestamp   TYPE   tstamp
      RETURNING VALUE(rv_time) TYPE string.


    CLASS-METHODS build_jira_link
      IMPORTING
                iv_as4text          TYPE   as4text
      RETURNING VALUE(rv_jira_link) TYPE string.



    CLASS-METHODS get_name_from_user
      IMPORTING
                iv_user        TYPE bapibname-bapibname
      RETURNING VALUE(rv_name) TYPE string.

ENDCLASS.

CLASS lcl_jira_srv IMPLEMENTATION.

  METHOD select_data.


    SELECT a~trkorr, e~trfunction, t~as4text, a~cre_user, a~cre_date, a~cre_time, c~reference
      FROM e070create AS a JOIN e070 AS e ON a~trkorr = e~trkorr
                           JOIN e07t AS t ON a~trkorr = t~trkorr
                           LEFT OUTER JOIN e070a AS c ON a~trkorr = c~trkorr
                           AND   c~attribute = 'Z_TICKET'
      INTO CORRESPONDING FIELDS OF TABLE @et_result
      WHERE a~cre_date >= @iv_dats
      AND   a~cre_time >= @iv_time
      AND   ( e~trfunction = 'K' OR e~trfunction = 'W' )
      AND   ( t~langu = 'D' OR t~langu = 'E' )
      AND   a~trkorr IN @it_trkorr.

  ENDMETHOD.                    "select_data

  METHOD send_mail.

  ENDMETHOD.                    "send_mail


  METHOD select_data_transp.

    DATA lt_tmstpalogs TYPE tmstpalogs.

    CONVERT DATE iv_dats TIME iv_time  "DAYLIGHT SAVING TIME 'X'
        INTO TIME STAMP DATA(lv_time_stamp) TIME ZONE sy-zonlo.


    CALL FUNCTION 'TMS_TM_GET_TRLIST' "##FM_SUBRC_OK
      EXPORTING
        iv_system    = 'SBP'
*       IV_DOMAIN    =
        iv_startdate = iv_dats
        iv_starttime = iv_time
        iv_enddate   = sy-datum
        iv_endtime   = '125959'
*       IV_TRSTEPS   =
        iv_allcli    = ''
        iv_trcli     = '100'
*       IV_TRFUNCTION   =
*       IV_PROJECT   =
*       IV_LISTNAME  =
        iv_imports   = 'X'
*       iv_exports   = ''
*       IV_LAST_IMPORT  = ''
*       IV_TPSTAT_KEY   =
*       IV_LATEST    =
*       IV_GET_LINENR   =
      IMPORTING
*       EV_SERVICE_VERS =
        et_tmstpalog = lt_tmstpalogs
*       ES_EXCEPTION =
      TABLES
        irt_requests = it_trkorr
      EXCEPTIONS
        alert        = 1
        OTHERS       = 2.

    LOOP AT lt_tmstpalogs ASSIGNING FIELD-SYMBOL(<fs_tmstplaog>)
    WHERE trtime >= lv_time_stamp.
      APPEND <fs_tmstplaog> TO et_result.
    ENDLOOP.


  ENDMETHOD.

  METHOD convert_timestamp.

    DATA: lv_timestamp TYPE timestamp,
          lv_char1     TYPE char10,
          lv_char2     TYPE char8.

    lv_timestamp = iv_timestamp.

    CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC'
        INTO DATE DATA(lv_date)
             TIME DATA(lv_time).

    WRITE lv_date TO lv_char1.
    WRITE lv_time TO lv_char2.

    CONCATENATE lv_char1 lv_char2 INTO rv_time SEPARATED BY space.

*    rv_time = lv_char1 && space && lv_char2.

  ENDMETHOD.

  METHOD build_jira_link.

*    DATA(lv_jira) = zcl_transport=>get_suffix( iv_as4text ).

*    rv_jira_link = '<a href="https://https://sonnen2020.atlassian.net/browse/' && lv_jira && '">' && lv_jira && '</a>'. "e.g..: https://sonnen2020.atlassian.net/browse/SAP-117

*iv_as4text

  ENDMETHOD.

  METHOD get_name_from_user.

    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    CONCATENATE ls_address-firstname ls_address-lastname '(' iv_user ')' INTO rv_name SEPARATED BY space.
    IF line_exists( lt_return[ type = 'E' ] ).
      rv_name = iv_user.
    ENDIF.
  ENDMETHOD.

  METHOD get_jira_from_attr.
    SELECT SINGLE reference
       FROM e070a
       INTO @ev_jira
       WHERE  trkorr = @iv_trkorr
       AND   attribute = 'Z_TICKET'.

    IF sy-subrc <> 0. "try to get Jira from text
      ev_jira = get_jira_from_text( iv_as4text = iv_as4text ).
    ENDIF.

  ENDMETHOD.

  METHOD get_jira_from_text.

    FIND PCRE '[A-Z]{2,}-\d+' IN iv_as4text RESULTS DATA(ls_result).
    IF sy-subrc = 0.
      ev_jira = substring( val = iv_as4text off = ls_result-offset len = ls_result-length ).
      CONDENSE ev_jira.
    ENDIF.

  ENDMETHOD.

ENDCLASS.



SELECTION-SCREEN BEGIN OF BLOCK d WITH FRAME TITLE TEXT-002.

  PARAMETERS:
    pv_crea TYPE flag,
    pv_trsp TYPE flag.

SELECTION-SCREEN END OF BLOCK d.


SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
    so_trkor   FOR e070-trkorr.

  PARAMETERS:
    pv_date TYPE dats,
    pv_time TYPE tims.

SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-003. "Transport SBP - update Jira

  PARAMETERS:
    pv_mail  TYPE flag NO-DISPLAY, "send mail?
    pv_mlrec TYPE so_dli_nam . "DEFAULT 'TRSP_RESULT'.    "SO23 mail list

SELECTION-SCREEN END OF BLOCK b.

*SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE TEXT-002.
*PARAMETERS:
*   pv_mail     TYPE c AS CHECKBOX. "send result as mail
*
*SELECTION-SCREEN END OF BLOCK c.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.


***********************************************************************
* "Selektion
* calculate values per request and prepare output
***********************************************************************
  DATA:
    lv_transport TYPE string,
    lv_jira      TYPE string,
    lt_result    TYPE lcl_jira_srv=>tt_result,
    lo_jira      TYPE REF TO zcl_ca_jira_ticket.

  FIELD-SYMBOLS:
        <ls_result>       TYPE lcl_jira_srv=>ty_result.


**********************************************************************
* Update Jira with transport request when new creation
**********************************************************************
  IF pv_crea = abap_true.

    lcl_jira_srv=>select_data(
      EXPORTING
        iv_time   = pv_time
        iv_dats   = pv_date
        it_trkorr = so_trkor[]
      IMPORTING
        et_result = lt_result ).


    LOOP AT lt_result ASSIGNING <ls_result>.

      lv_jira =  <ls_result>-reference.
      IF <ls_result>-reference IS INITIAL.
        lv_jira = lcl_jira_srv=>get_jira_from_text( iv_as4text = <ls_result>-as4text ).
      ENDIF.
      IF lv_jira IS NOT INITIAL.
        TRY .
            lo_jira = NEW #( iv_ticket = lv_jira ).
            lo_jira->add_transport_number( iv_transport_number = CONV #( <ls_result>-trkorr ) iv_type = <ls_result>-trfunction iv_user = <ls_result>-cre_user ).
            "the method takes care of adding it when necessary
          CATCH zcx_ca_jira INTO DATA(lx_jira).
            "error handling not necessary here
            EXIT. "EC CI_NOORDER
        ENDTRY.
      ENDIF.
    ENDLOOP.
*    IF sy-subrc = 4.
*      WRITE TEXT-005. "no data selected
*    ENDIF.
*
  ENDIF.

**********************************************************************
* Update Jira with info that it was transported
**********************************************************************
  IF pv_trsp = abap_true.
    DATA(lt_transported) =  lcl_jira_srv=>select_data_transp( iv_time = pv_time iv_dats = pv_date it_trkorr = so_trkor[] ).

    LOOP AT lt_transported ASSIGNING FIELD-SYMBOL(<fs_transported>).

      lv_jira = lcl_jira_srv=>get_jira_from_attr( iv_trkorr = <fs_transported>-trkorr iv_as4text = <fs_transported>-as4text ).
      TRY .
          lo_jira = NEW #( iv_ticket = lv_jira ).

          DATA lv_text TYPE string.
          DATA lv_retcode TYPE i.
          lv_retcode = <fs_transported>-retcode.
          lv_text = <fs_transported>-trkorr && | | && TEXT-010 && | | && <fs_transported>-retcode. "transport successfull on SBP with return code &1
          IF lv_retcode >= 8.
            lv_text = TEXT-011 && | | && <fs_transported>-trkorr. "(x) Transport failed on SBP (x) with number &1
          ENDIF.
          lo_jira->add_comment( lv_text ).

        CATCH zcx_ca_jira INTO lx_jira.
          "error handling not necessary here
          "this transport did not have a Jira- might still be Wrike- so don't do anything
          CONTINUE.
      ENDTRY.

    ENDLOOP.
*    IF sy-subrc = 4.
*      WRITE TEXT-005. "no data selected
*    ELSE.
*      WRITE TEXT-010. "updates successful
*    ENDIF.

  ENDIF.

  IF pv_mail IS NOT INITIAL OR pv_mlrec IS NOT INITIAL. "send mail

    lcl_jira_srv=>send_mail( it_result = lcl_jira_srv=>select_data_transp( iv_time = pv_time iv_dats = pv_date it_trkorr = so_trkor[] ) iv_mlrec = pv_mlrec ).

  ENDIF.
