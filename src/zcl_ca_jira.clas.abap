CLASS zcl_ca_jira DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_all_jira_projects
      RETURNING VALUE(rt_projects) TYPE string_t.

  PROTECTED SECTION.

    CONSTANTS c_rfcdestination TYPE rfcdest VALUE 'JIRA'.   "#EC NOTEXT


    CLASS-METHODS call_jira_rest_method
      IMPORTING
*        !method_type    TYPE string
        !iv_method         TYPE string
        !iv_data           TYPE string OPTIONAL
        !iv_parameters     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_response) TYPE string
      RAISING
        zcx_ca_jira .

  PRIVATE SECTION.

    CLASS-DATA:
              gt_jira_projects TYPE string_t.

    TYPES:
      BEGIN OF ts_jira_project,
        key  TYPE string,
        name TYPE string,
      END OF ts_jira_project,

      tt_jira_projects TYPE TABLE OF ts_jira_project.

ENDCLASS.


CLASS zcl_ca_jira IMPLEMENTATION.


  METHOD call_jira_rest_method.

    DATA:
      lo_http_client TYPE REF TO if_http_client,
      lv_status_code TYPE i ##NEEDED.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination                = c_rfcdestination
      IMPORTING
        client                     = lo_http_client
      EXCEPTIONS
        argument_not_found         = 1
        destination_not_found      = 2
        destination_no_authority   = 3
        plugin_not_active          = 4
        internal_error             = 5
        oa2c_set_token_error       = 6
        oa2c_missing_authorization = 7
        oa2c_invalid_config        = 8
        oa2c_invalid_parameters    = 9
        oa2c_invalid_scope         = 10
        oa2c_invalid_grant         = 11
        OTHERS                     = 12
    ).

    IF sy-subrc <> 0.
      lo_http_client->close( ).
      FREE lo_http_client.

      RAISE EXCEPTION TYPE zcx_ca_jira
        EXPORTING
          textid = zcx_ca_jira=>zcx_http_client.

    ENDIF.
    lo_http_client->request->set_content_type( content_type = 'application/json' ).
    cl_http_utility=>set_request_uri( request = lo_http_client->request uri = iv_parameters ).

    IF lo_http_client IS BOUND.
*    **set http method get/post
      IF iv_method = 'GET'.

        lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
      ELSEIF iv_method = 'POST'.
        lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      ELSEIF iv_method = 'PUT'.
        lo_http_client->request->set_header_field( EXPORTING name = '~request_method' value = 'PUT' ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    IF iv_data IS NOT INITIAL.
      lo_http_client->request->set_cdata( data = iv_data ).
    ENDIF.

* Turn off logon popup. detect authentication errors.
    lo_http_client->propertytype_logon_popup = 0.

* Send / Receive Token Request
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
*      me->log_data( if_msgnr = '009' if_mtype = 'E' if_msgv1 = CONV #( c_onetime_rfcdest ) ).
    ENDIF.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
*      me->log_data( if_msgnr = '009' if_mtype = 'E' if_msgv1 = CONV #( c_onetime_rfcdest ) ).
    ENDIF.

    lo_http_client->response->get_status( IMPORTING code = lv_status_code ).

    IF lv_status_code <> '200' AND iv_method = 'GET'. "Cannot read the Ticket

      RAISE EXCEPTION TYPE zcx_ca_jira
        EXPORTING
          textid = zcx_ca_jira=>zcx_ticket_not_found.

    ELSEIF lv_status_code <> '204' AND iv_method = 'PUT'
      OR lv_status_code <> '201' AND iv_method = 'POST'.

      RAISE EXCEPTION TYPE zcx_ca_jira
        EXPORTING
          textid = zcx_ca_jira=>zcx_ticket_update.

    ENDIF.

*Json response needs to be converted to abap readable format
    rv_response = lo_http_client->response->get_cdata(  ).

    lo_http_client->close( ).
    FREE lo_http_client.

  ENDMETHOD.



  METHOD get_all_jira_projects.

    DATA:
        lt_data TYPE tt_jira_projects.

    IF gt_jira_projects IS INITIAL.

      TRY.
          DATA(lv_response) = call_jira_rest_method( iv_method = 'GET' iv_parameters = 'project' ).

          /ui2/cl_json=>deserialize(
            EXPORTING
              json = lv_response
            CHANGING
              data = lt_data
          ).

          rt_projects = VALUE #( FOR ls_line IN lt_data ( ls_line-key ) ).

        CATCH zcx_ca_jira.
          "handle exception
      ENDTRY.

    ENDIF.

    gt_jira_projects = rt_projects.

  ENDMETHOD.

ENDCLASS.
