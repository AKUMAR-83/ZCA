CLASS zcl_ca_sap_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING if_username TYPE xubname
        RAISING   zcx_ca_sap_user,
      is_valid
        RETURNING VALUE(rf_is_valid) TYPE boolean,
      is_locked_no_unlock
        RETURNING VALUE(rf_lockedadmin) TYPE boolean,
      is_locked_unlock
        RETURNING VALUE(rf_lockedcorrect) TYPE boolean,
      is_sso_user
        RETURNING VALUE(rf_sso) TYPE boolean,
      get_email_address
        RETURNING VALUE(rf_email) TYPE ad_smtpadr,       "#EC CI_VALPAR
      get_username RETURNING VALUE(rf_result) TYPE xubname,
      get_firstname     RETURNING VALUE(rf_name) TYPE ad_namefir,
      get_userlanguage RETURNING VALUE(rf_spras) TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      gs_logondata TYPE bapilogond,
      gs_address   TYPE bapiaddr3,
      gs_snc       TYPE bapisncu,
      gs_lockdata  TYPE bapislockd,
      gv_username  TYPE xubname,
      gs_defaults  TYPE bapidefaul.
ENDCLASS.



CLASS zcl_ca_sap_user IMPLEMENTATION.
  METHOD constructor.

    gv_username = if_username.
    DATA:
          lt_return TYPE TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = if_username
        cache_results = 'X'
*       extuid_get    =
      IMPORTING
        logondata     = gs_logondata
        defaults      = gs_defaults
        address       = gs_address
*       company       =
        snc           = gs_snc
*       ref_user      =
*       alias         =
*       uclass        =
*       lastmodified  =
        islocked      = gs_lockdata
*       identity      =
*       admindata     =
*       description   =
*       tech_user     =
*       sapuser_uuid  =
      TABLES
*       parameter     =
*       profiles      =
*       activitygroups    =
        return        = lt_return
*       addtel        =
*       addfax        =
*       addttx        =
*       addtlx        =
*       addsmtp       =
*       addrml        =
*       addx400       =
*       addrfc        =
*       addprt        =
*       addssf        =
*       adduri        =
*       addpag        =
*       addcomrem     =
*       parameter1    =
*       groups        =
*       uclasssys     =
*       extidhead     =
*       extidpart     =
*       systems       =
*       extuid        =
*       sapuser_uuid_hist =
*       usattribute   =
      .
    IF line_exists(  lt_return[ type = 'E' ] ).          "#EC CI_STDSEQ
      DATA(ls_error) = lt_return[ type = 'E' ]. "#EC CI_CONV_OK  #EC CI_STDSEQ
      RAISE EXCEPTION TYPE zcx_ca_sap_user
      MESSAGE ID ls_error-id
      TYPE ls_error-type
      NUMBER ls_error-number.
    ENDIF.
    IF me->get_email_address( ) IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_sap_user MESSAGE ID 'ZCA' TYPE 'E' NUMBER 010. "User has no email address maintained.
    ENDIF.

  ENDMETHOD.

  METHOD get_email_address.
    rf_email = gs_address-e_mail.
  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD is_locked_no_unlock.
*    L Locked
*    U   Not Locked
    IF gs_lockdata-glob_lock = 'L' OR
    gs_lockdata-local_lock = 'L' OR
    gs_lockdata-no_user_pw = 'L'.
      rf_lockedadmin = abap_true.
    ELSE.
      rf_lockedadmin = abap_false.
    ENDIF.


  ENDMETHOD.

  METHOD is_locked_unlock.
    IF me->is_locked_no_unlock( ) = abap_false
        AND gs_lockdata-wrng_logon = 'L'.
      rf_lockedcorrect = abap_true.
    ELSE.
      rf_lockedcorrect = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD is_sso_user.
    rf_sso = gs_snc-guiflag.
  ENDMETHOD.

  METHOD is_valid.
    IF gs_logondata-gltgb >= sy-datum.
      rf_is_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_username.
    rf_result = me->gv_username.
  ENDMETHOD.

  METHOD get_userlanguage.
    rf_spras = me->gs_defaults-langu.
    IF rf_spras IS INITIAL.
      rf_spras = 'EN'.
    ENDIF.
  ENDMETHOD.

  METHOD get_firstname.
    rf_name = gs_address-firstname.
  ENDMETHOD.

ENDCLASS.
