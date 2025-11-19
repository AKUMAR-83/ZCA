CLASS zcl_ca_usrpwreset DEFINITION
* LOCAL FRIENDS ltcl_usrpwreset
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
        c_onetime_rfcdest    TYPE rfcdest VALUE 'ONETIME_SONNEN' ##NO_TEXT .
    METHODS:

      constructor
        IMPORTING if_username TYPE xubname
        RAISING   zcx_ca_sap_user,
      "raising user not found

      unlock_user
        IMPORTING if_send_mail      TYPE boolean OPTIONAL
        EXPORTING et_return         TYPE bapiret2_t
        RETURNING VALUE(rf_success) TYPE boolean,

      reset_password_user.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: o_sapuser TYPE REF TO zcl_ca_sap_user.

    METHODS:
      send_response_mail
        IMPORTING if_msgnr  TYPE msgnr OPTIONAL
                  if_mtype  TYPE bapi_mtype OPTIONAL
                  if_pwlink TYPE string OPTIONAL
                  it_return TYPE bapiret2_t OPTIONAL,

      generate_password
        IMPORTING if_length    TYPE i
        RETURNING VALUE(rf_pw) TYPE string,

      log_data
        IMPORTING if_msgnr  TYPE msgnr OPTIONAL
                  if_mtype  TYPE bapi_mtype OPTIONAL ##NEEDED
                  if_msgv1  TYPE symsgv OPTIONAL
                  it_return TYPE bapiret2_t OPTIONAL
                  ix_except TYPE REF TO cx_root OPTIONAL,

      generate_pw_link
        IMPORTING if_pw          TYPE string
        RETURNING VALUE(rf_link) TYPE string  ##NEEDED,

      build_return_log_send_mail
        IMPORTING
                  if_send_mail     TYPE boolean OPTIONAL
                  if_msgnr         TYPE msgnr
                  if_mtype         TYPE bapi_mtype ##NEEDED
                  if_msgv1         TYPE symsgv OPTIONAL
                  it_return        TYPE bapiret2_t OPTIONAL
        RETURNING VALUE(rs_return) TYPE bapiret2.

ENDCLASS.


CLASS zcl_ca_usrpwreset IMPLEMENTATION.

  METHOD constructor.
    TRY.
        o_sapuser = NEW #( if_username ).
      CATCH zcx_ca_sap_user INTO DATA(lx_sap_user).
        me->log_data( ix_except = lx_sap_user ).
        RAISE EXCEPTION NEW zcx_ca_sap_user( previous = lx_sap_user ).
    ENDTRY.
  ENDMETHOD.

  METHOD reset_password_user.
    DATA:
      lt_return    TYPE TABLE OF bapiret2,
      ls_passwordx TYPE bapipwdx.

    IF o_sapuser->is_locked_no_unlock( ) = abap_true.
      "no unlock of user - so also no PW reset
      me->send_response_mail( if_msgnr = '001' if_mtype = 'E' ). "User is locked by administrator (passw. change and unlocking not possib.)
      me->log_data( if_msgnr = '001' if_mtype = 'E' ).
      RAISE EXCEPTION NEW zcx_ca_sap_user( ).
      EXIT.
    ELSEIF o_sapuser->is_locked_unlock( ).
      DATA(lf_unlock_ok) = me->unlock_user( ).
      CHECK lf_unlock_ok = abap_true.
    ENDIF.

    DATA(lf_pw) = me->generate_password( 10 ).

    ls_passwordx-bapipwd = 'X'.

    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username  = o_sapuser->get_username( )
        password  = CONV bapipwd( lf_pw )
        passwordx = ls_passwordx
      TABLES
        return    = lt_return.

    IF line_exists( lt_return[ type = 'E' ] ) OR         "#EC CI_STDSEQ
           line_exists( lt_return[ type = 'A' ] ).       "#EC CI_STDSEQ
      me->send_response_mail( if_msgnr = '006' if_mtype = 'E' it_return = lt_return ). "Resetting password was not successfull. Please contact administrator.
      me->log_data( if_msgnr = '006' if_mtype = 'E' it_return = lt_return ).
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      me->send_response_mail( if_msgnr = '005' if_mtype = 'S' it_return = lt_return if_pwlink = me->generate_pw_link( if_pw = lf_pw ) ). "Password reset successful
      me->log_data( if_msgnr = '005' if_mtype = 'S' ).
    ENDIF.


  ENDMETHOD.

  METHOD unlock_user.
    DATA:
        lt_return TYPE TABLE OF bapiret2.

    CLEAR et_return.

    rf_success = abap_false.                            "#EC CI_CONV_OK
    IF o_sapuser->is_locked_no_unlock( ) = abap_true.
      et_return = VALUE #( ( me->build_return_log_send_mail( if_send_mail = if_send_mail if_msgnr = '001' if_mtype = 'E' ) ) ). "#EC CI_CONV_OK

    ELSEIF o_sapuser->is_locked_unlock( ) = abap_false. "#EC CI_CONV_OK
      "user is not not locked or no pw exists.
      et_return = VALUE #( ( me->build_return_log_send_mail( if_send_mail = if_send_mail if_msgnr = '002' if_mtype = 'E' ) ) ). "#EC CI_CONV_OK
    ELSEIF o_sapuser->is_locked_unlock( ) = abap_true.
      "unlock user and send mail about it
      CALL FUNCTION 'BAPI_USER_UNLOCK'
        EXPORTING
          username = o_sapuser->get_username( )
        TABLES
          return   = lt_return.

      IF line_exists( lt_return[ type = 'E' ] ) OR       "#EC CI_STDSEQ
          line_exists( lt_return[ type = 'A' ] ).        "#EC CI_STDSEQ
        "unlocking of user was not successful
        et_return = VALUE #( ( me->build_return_log_send_mail( if_send_mail = if_send_mail if_msgnr = '003' if_mtype = 'E' ) ) ). "#EC CI_CONV_OK
      ELSE.
        rf_success = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        "User was unlocked successfully
        et_return = VALUE #( ( me->build_return_log_send_mail( if_send_mail = if_send_mail if_msgnr = '004' if_mtype = 'S' ) ) ). "#EC CI_CONV_OK
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD send_response_mail.

    DATA    lf_messages TYPE string.


    TRY.
        DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = 'ZCA_PASSWORD_EMAIL' ).
      CATCH cx_smtg_email_common.
        me->log_data( if_msgnr = '008' if_mtype = 'E' ). "Error sending email
        EXIT.
    ENDTRY.

*    "get text of template and replace with placeholders
    DATA(ls_template) = lo_email_api->get_detail( ).
    IF line_exists( ls_template-content[ langu = o_sapuser->get_userlanguage( ) ] ).
      DATA(lf_text) = ls_template-content[ langu = o_sapuser->get_userlanguage( ) ]-body_html.
      DATA(lf_subject) = ls_template-content[ langu = o_sapuser->get_userlanguage( ) ]-subject.
    ELSE.
      lf_text = ls_template-content[ langu = 'E' ]-body_html.
      lf_subject = ls_template-content[ langu = 'E' ]-subject.
    ENDIF.

    REPLACE FIRST OCCURRENCE OF '{{USER}}' IN lf_text WITH o_sapuser->get_firstname( ).

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      IF <ls_return>-id = ''.
        MESSAGE ID 'ZCA' TYPE <ls_return>-type NUMBER <ls_return>-number  WITH <ls_return>-message_v1 INTO DATA(lf_message).
      ELSE.
        lf_message = <ls_return>-message.
      ENDIF.
      lf_messages = lf_messages && lf_message && |<br>|.
    ENDLOOP.
    MESSAGE ID 'ZCA' TYPE if_mtype  NUMBER if_msgnr INTO lf_message.
    lf_messages = lf_messages && lf_message.

    REPLACE FIRST OCCURRENCE OF '{{TEXTLINES}}' IN lf_text WITH lf_messages.

    REPLACE FIRST OCCURRENCE OF '{{PASSWORD}}' IN lf_text WITH if_pwlink.


    DATA(lt_body_txt) = cl_document_bcs=>string_to_soli( ip_string = lf_text ).

    TRY.
        DATA(lo_document) = cl_document_bcs=>create_document( i_type        = 'HTM'
                                                              i_text        = lt_body_txt
                                                              i_subject     = CONV so_obj_des( lf_subject )
                                                              i_sensitivity = 'P' ).
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).
        lo_send_request->set_message_subject( ip_subject = CONV string( lf_subject ) ).
        lo_send_request->set_document( lo_document ).
        SELECT SINGLE @abap_true FROM usr02
            WHERE bname = 'PSWRD_RESET'
            INTO @DATA(lf_exists).
        IF lf_exists = abap_true.
          lo_send_request->set_sender( cl_sapuser_bcs=>create( 'PSWRD_RESET' ) ).
        ELSE.
          lo_send_request->set_sender( cl_sapuser_bcs=>create( sy-uname ) ).
        ENDIF.

        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( o_sapuser->get_email_address( ) ).
        lo_send_request->add_recipient( i_recipient = lo_recipient i_express = abap_true ).
        lo_send_request->set_send_immediately( abap_true ).

        IF lo_send_request->send( i_with_error_screen = abap_true ) = abap_true.
          me->log_data( if_msgnr = '007' if_mtype = 'S' ). "Email sent
        ELSE.
          me->log_data( if_msgnr = '008' if_mtype = 'E' ). "Error sending email
        ENDIF.

        DATA: lf_in_update_task TYPE sy-subrc.
        CALL FUNCTION 'TH_IN_UPDATE_TASK'
          IMPORTING
            in_update_task = lf_in_update_task.

        IF lf_in_update_task EQ 0.
          COMMIT WORK.
        ENDIF.

      CATCH cx_root ##CATCH_ALL .
        me->log_data( if_msgnr = '008' if_mtype = 'E' ). "Error sending email
    ENDTRY.

  ENDMETHOD.

  METHOD generate_password.

    CONSTANTS:
      lc_pwalpha   TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
      lc_pwnumber  TYPE string VALUE '0123456789',
      lc_pwspecial TYPE string VALUE '_!-+&'.


    DATA(lo_randi_alpha) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 0 max = strlen( lc_pwalpha ) - 1 ).
    DATA(lo_randi_number) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 0 max = strlen( lc_pwnumber ) - 1 ).
    DATA(lo_randi_special) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 0 max = strlen( lc_pwspecial ) - 1 ).

    DO ( if_length - 1 ) TIMES.
      IF sy-index MOD 5 = 0.
        rf_pw = |{ rf_pw }{ substring( val = lc_pwspecial off = lo_randi_special->get_next( ) len = 1 ) }|.
      ELSE.
        rf_pw = |{ rf_pw }{ substring( val = lc_pwalpha off = lo_randi_alpha->get_next( ) len = 1 ) }|.
      ENDIF.
    ENDDO.
    rf_pw = |{ rf_pw }{ substring( val = lc_pwnumber off = lo_randi_number->get_next( ) len = 1 ) }|.
  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD log_data.


    DATA(lf_balnrext) = COND #( WHEN o_sapuser IS BOUND THEN o_sapuser->get_username( )
                                ELSE 'No SAP User exists') ##NO_TEXT ##CONDITIONAL_CONSTR_OK.
    TRY.
        " Create the log
        DATA(lo_appl_log) = NEW cl_bal_logobj( i_log_object        = 'ZCA_PWRESET'
                                               i_default_subobject = 'ZCA_PWRESET'
                                               i_extnumber         = CONV balnrext( lf_balnrext ) ).

        LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
          MESSAGE ID 'ZCA' TYPE <ls_return>-type NUMBER <ls_return>-number  WITH <ls_return>-message_v1 INTO DATA(lf_message).
          IF <ls_return>-type = 'E'.
            lo_appl_log->add_errortext( i_errortext = lf_message ).
          ELSE.
            lo_appl_log->add_statustext( i_statustext = lf_message ).
          ENDIF.
        ENDLOOP.
        IF if_mtype IS NOT INITIAL.
          MESSAGE ID 'ZCA' TYPE if_mtype  NUMBER if_msgnr INTO lf_message.
          IF if_mtype = 'E'.
            lo_appl_log->add_errortext( i_errortext = lf_message ).
          ELSE.
            lo_appl_log->add_statustext( i_statustext = lf_message ).
          ENDIF.
        ENDIF.
        IF ix_except IS NOT INITIAL.
          lo_appl_log->add_exception( i_exception = ix_except ).
        ENDIF.

        " Save the log
        lo_appl_log->save( IMPORTING et_lognumbers = DATA(lt_log_numbers) ) ##NEEDED.

      CATCH cx_bal_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD generate_pw_link.

    DATA:
      lo_http_dest   TYPE REF TO if_http_client,
      lf_status_code TYPE i ##NEEDED,
      lf_response    TYPE string.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination                = c_onetime_rfcdest
      IMPORTING
        client                     = lo_http_dest
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
      lo_http_dest->close( ).
      me->log_data( if_msgnr = '009' if_mtype = 'E' if_msgv1 = CONV #( c_onetime_rfcdest ) ).
    ENDIF.

    IF lo_http_dest IS BOUND.
      lo_http_dest->request->set_method( if_http_entity=>co_request_method_post ).
    ENDIF.

*Set all the form parameters
    lo_http_dest->request->set_form_field( name = 'secret' value = if_pw ) ##NO_TEXT.

* Turn off logon popup. detect authentication errors.
    lo_http_dest->propertytype_logon_popup = 0.

* Send / Receive Token Request
    CALL METHOD lo_http_dest->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      me->log_data( if_msgnr = '009' if_mtype = 'E' if_msgv1 = CONV #( c_onetime_rfcdest ) ).
    ENDIF.

    CALL METHOD lo_http_dest->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      me->log_data( if_msgnr = '009' if_mtype = 'E' if_msgv1 = CONV #( c_onetime_rfcdest ) ).
    ENDIF.

    lo_http_dest->response->get_status( IMPORTING code = lf_status_code ).

*Json response needs to be converted to abap readable format
    lf_response = lo_http_dest->response->get_cdata(  ).
    rf_link = lf_response."SAP-308
*    rf_link = substring_after( val = lf_response sub = 'Link:' ) ##NEEDED ##NO_TEXT.

    DATA(lo_regex_pattern) = cl_abap_regex=>create_pcre( pattern = '[\n\r\s]+' ).
    REPLACE ALL OCCURRENCES OF REGEX  lo_regex_pattern IN rf_link WITH ''.

    lo_http_dest->close( ).

  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD build_return_log_send_mail.
    IF if_send_mail = abap_true.
      me->send_response_mail( if_msgnr = if_msgnr if_mtype = if_mtype ). "user is locked by administrator;
    ENDIF.
    me->log_data( if_msgnr = if_msgnr if_mtype = if_mtype ).
    rs_return = VALUE #( number = if_msgnr type = if_mtype ).

  ENDMETHOD.                                             "#EC CI_VALPAR

ENDCLASS.
