*"* use this source file for your ABAP unit test classes
CLASS ltcl_jira_ticket DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
       lo_jira_ticket TYPE REF TO zcl_ca_jira_ticket.
    METHODS:
      get_existing_jira FOR TESTING RAISING cx_static_check,
      get_nonexisting_jira FOR TESTING RAISING cx_static_check,
      get_summary FOR TESTING RAISING cx_static_check,
      get_transport_number FOR TESTING RAISING cx_static_check,
      get_component_zz FOR TESTING RAISING cx_static_check,
      get_component_empty FOR TESTING RAISING cx_static_check,
      get_component_auth FOR TESTING RAISING cx_static_check,
      get_component_sd FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_jira_ticket IMPLEMENTATION.

  METHOD get_existing_jira.
    TRY.
        lo_jira_ticket = NEW #( 'SAP-1' ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lo_jira_ticket
        msg = 'There should be an object' ).
*    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  ENDMETHOD.

  METHOD get_nonexisting_jira.
    TRY.
        lo_jira_ticket = NEW #( 'SAP-123' ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
   EXPORTING
     act = lx_cx_jira
     msg = 'There should be an exception' ).
  ENDMETHOD.

  METHOD get_summary.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-1' ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = condense( lo_jira_ticket->get_summary( ) )
        exp = 'SAP Password reset - self service tool'
        msg = 'summary differs' ).
*    cl_abap_unit_assert=>fail( 'Implement your first test here' ).

  ENDMETHOD.

  METHOD get_transport_number.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-314' ).

        DATA(lt_transport) = lo_jira_ticket->get_transport_numbers( ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_transport[ 1 ]
        exp = 'SBDK915644'
        msg = 'summary differs' ).

  ENDMETHOD.

  METHOD get_component_empty.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-133' ).

        DATA(lv_component) = lo_jira_ticket->get_component( ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_component
        exp = ''
        msg = 'component differs' ).

  ENDMETHOD.

  METHOD get_component_sd.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-246' ).

        DATA(lv_component) = lo_jira_ticket->get_component( ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_component
        exp = 'SD'
        msg = 'component differs' ).

  ENDMETHOD.

  METHOD get_component_zz.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-314' ).

        DATA(lv_component) = lo_jira_ticket->get_component( ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_component
        exp = 'ZZ'
        msg = 'component differs' ).

  ENDMETHOD.

  METHOD get_component_auth.

    TRY.
        lo_jira_ticket = NEW #( 'SAP-232' ).

        DATA(lv_component) = lo_jira_ticket->get_component( ).
      CATCH zcx_ca_jira INTO DATA(lx_cx_jira) ##NO_HANDLER .

    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_component
        exp = 'AUTH'
        msg = 'component differs' ).

  ENDMETHOD.

ENDCLASS.
