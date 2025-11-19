CLASS zcl_ca_jira_ticket DEFINITION
INHERITING FROM zcl_ca_jira
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_ticket TYPE string
      RAISING
        zcx_ca_jira .

    METHODS get_transport_numbers
      RETURNING
        VALUE(rt_transport_numbers) TYPE string_t .

    METHODS get_summary
      RETURNING
        VALUE(rv_summary) TYPE string .

    METHODS add_transport_number
      IMPORTING
        !iv_transport_number TYPE string
        !iv_type             TYPE trfunction OPTIONAL
        !iv_user             TYPE as4user OPTIONAL
      RAISING
        zcx_ca_jira .
    METHODS add_comment
      IMPORTING iv_comment TYPE string
      RAISING
                zcx_ca_jira .

    METHODS get_component
      RETURNING
        VALUE(rv_component) TYPE string .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:

      BEGIN OF ts_jira_components,
*        id   TYPE string,
        name TYPE string,
*        self TYPE string,
      END OF ts_jira_components,

      tt_jira_components TYPE STANDARD TABLE OF ts_jira_components WITH NON-UNIQUE DEFAULT KEY,

      BEGIN OF ts_jira_subdata,
        components        TYPE tt_jira_components,
        customfield_10453 TYPE string,
        summary           TYPE string,
      END OF ts_jira_subdata,

      BEGIN OF ts_jira_data,
        fields TYPE ts_jira_subdata,
        id     TYPE string,
        key    TYPE string,
        self   TYPE string,
      END OF ts_jira_data.

    DATA:
      gv_ticket            TYPE string,
      gv_summary           TYPE string,
      gv_component         TYPE string,
      gt_transport_numbers TYPE TABLE OF string.


ENDCLASS.


CLASS zcl_ca_jira_ticket IMPLEMENTATION.


  METHOD add_comment.
*{
*  "body": {
*    "content": [
*      {
*        "content": [
*          {
*            "text": the comment which should be added
*            "type": "text"
*          }
*        ],
*        "type": "paragraph"
*      }
*    ],
*    "type": "doc",
*    "version": 1
*  }
*}
    DATA(l_data) = '{ "body": { "content": [ { "content": [ {' &&
                    ' "text": "' && iv_comment && '",' &&
                    ' "type": "text" } ], "type": "paragraph"  }  ],' &&
                    ' "type": "doc", "version": 1 } }'.
    me->call_jira_rest_method( iv_method = 'POST' iv_parameters = 'issue/' && gv_ticket && '/comment' iv_data = l_data ).

  ENDMETHOD.


  METHOD add_transport_number.
*
    DATA:
      lv_ddtext     TYPE      string.

    IF NOT line_exists( gt_transport_numbers[ table_line = iv_transport_number ] ) AND iv_transport_number IS NOT INITIAL.

      CALL FUNCTION 'DDUT_TEXT_FOR_VALUE'
        EXPORTING
          tabname   = 'E070'
          fieldname = 'TRFUNCTION'
          value     = iv_type
*         langu     = 'EN'
        IMPORTING
          text      = lv_ddtext.

      MESSAGE ID 'ZCA' TYPE 'S' NUMBER '011'  WITH lv_ddtext iv_transport_number iv_user INTO DATA(lv_message).  "&1 with number &2 added by user &3.
      me->add_comment( lv_message ).

      gt_transport_numbers = VALUE #( BASE  gt_transport_numbers ( iv_transport_number ) ).

      DATA(lv_transports) = concat_lines_of( table = gt_transport_numbers sep = ',' && | | ).

      DATA(lv_data) = '{ "fields": {' && '"customfield_10453":"' && lv_transports && '"}}'.

      call_jira_rest_method( iv_method = 'PUT' iv_parameters = 'issue/' && gv_ticket iv_data = lv_data ).

    ENDIF.
  ENDMETHOD.



  METHOD constructor.

    DATA:
            ls_data        TYPE ts_jira_data.

    super->constructor( ).

    gv_ticket = iv_ticket.

    DATA(lv_response) = me->call_jira_rest_method( iv_method = 'GET' iv_parameters = 'issue/' && gv_ticket && '?fields=summary, customfield_10453, components' ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_response
      CHANGING
        data = ls_data
    ).

    gv_summary = ls_data-fields-summary.

    SPLIT condense( val = ls_data-fields-customfield_10453 del = ' ' to = '' ) AT ',' INTO TABLE gt_transport_numbers.

    IF line_exists( ls_data-fields-components[ 1 ] ).
      TRY.
          gv_component = substring_before( val = ls_data-fields-components[ 1 ]-name sub = '-' ).
          gv_component = condense( val = gv_component del = ' ' to = '' ).
        CATCH cx_sy_strg_par_val .
          IF strlen( ls_data-fields-components[ 1 ]-name ) GE 3.
            gv_component = substring(  val = ls_data-fields-components[ 1 ]-name len = 3 ).
          ELSE.
            gv_component = ls_data-fields-components[ 1 ]-name.
          ENDIF.
      ENDTRY.
    ENDIF.


  ENDMETHOD.


  METHOD get_transport_numbers.
    rt_transport_numbers = gt_transport_numbers.
  ENDMETHOD.



  METHOD get_summary.
    rv_summary = gv_summary.
  ENDMETHOD.

  METHOD get_component.
    rv_component = gv_component.
  ENDMETHOD.

ENDCLASS.
