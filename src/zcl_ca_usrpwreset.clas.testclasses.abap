*"* use this source file for your ABAP unit test classes
CLASS lcl_usrpwreset DEFINITION DEFERRED.
CLASS zcl_ca_usrpwreset DEFINITION LOCAL FRIENDS lcl_usrpwreset.

CLASS lcl_usrpwreset DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.
*  CLASS zcl_ca_usrpwreset DEFINITION LOCAL FRIENDS ltcl_usrpwreset.

  PRIVATE SECTION.
    DATA:
        go_usrpwreset TYPE REF TO zcl_ca_usrpwreset.
    METHODS:
      wrong_usr_in_exc_out FOR TESTING RAISING cx_static_check,
      check_link_created FOR TESTING RAISING cx_static_check,
      check_pw_correct FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_usrpwreset IMPLEMENTATION.

  METHOD wrong_usr_in_exc_out.
    TRY.
        go_usrpwreset = NEW #( 'DOESNOTEXIST' ).
      CATCH zcx_ca_sap_user INTO DATA(lx_cx_user) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_cx_user
        msg = 'There should be an exception' ).
  ENDMETHOD.

  METHOD check_pw_correct.
    DATA: lf_pw TYPE string.
    TRY.
        go_usrpwreset = NEW #( 'S.KITTEL' ).
        go_usrpwreset->generate_password(
          EXPORTING
            if_length = 10
          RECEIVING
            rf_pw     = lf_pw
        ).
      CATCH zcx_ca_sap_user ##NO_HANDLER. " INTO DATA(lx_cx_user).
    ENDTRY.
*
    cl_abap_unit_assert=>assert_text_matches(
      EXPORTING
        pattern = '(?=^.{8,}$)(?=.*\d)(?=.*[!@#$%^&*-_]+)(?![.\n])(?=.*[A-Z])(?=.*[a-z]).*$'
        text    = lf_pw
*       full    = abap_false
        msg     = 'Password quality not good enough'
*       level   = if_abap_unit_constant=>severity-medium
*       quit    = if_abap_unit_constant=>quit-test
*      RECEIVING
*       assertion_failed =
    ).


  ENDMETHOD.

  METHOD check_link_created.
*    DATA: lf_pw TYPE string.
    TRY.
        go_usrpwreset = NEW #( 'S.KITTEL' ).
        DATA(lf_link) = go_usrpwreset->generate_pw_link( if_pw = 'Abcd,1234$' ).
      CATCH zcx_ca_sap_user ##NO_HANDLER. " INTO DATA(lx_cx_user).
    ENDTRY.

    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = lf_link
        exp = ''
        msg = 'Link is initial'
    ).
*    cl_abap_unit_assert=>assert_text_matches(
*      EXPORTING
*        pattern = '(?=^.{8,}$)(?=.*\d)(?=.*[!@#$%^&*-_]+)(?![.\n])(?=.*[A-Z])(?=.*[a-z]).*$'
*        text    = lf_pw
**       full    = abap_false
*        msg     = 'Password quality not good enough'
**       level   = if_abap_unit_constant=>severity-medium
**       quit    = if_abap_unit_constant=>quit-test
**      RECEIVING
**       assertion_failed =
*    ).

  ENDMETHOD.

ENDCLASS.
