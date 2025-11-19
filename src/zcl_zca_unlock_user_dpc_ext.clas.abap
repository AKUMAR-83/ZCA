CLASS zcl_zca_unlock_user_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zca_unlock_user_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS zca_sap_userset_create_entity
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zca_unlock_user_dpc_ext IMPLEMENTATION.

  METHOD zca_sap_userset_create_entity.

    TYPES: BEGIN OF ts_ZCA_SAP_USER,
             Bname TYPE String,
             Uflag TYPE c1,
           END OF ts_ZCA_SAP_USER.

    CLEAR er_entity.

    DATA:
      lo_message_container TYPE REF TO /iwbep/if_message_container,
      ls_data              TYPE ts_ZCA_SAP_USER,
      lo_pwreset           TYPE REF TO zcl_ca_usrpwreset.

    lo_message_container = me->mo_context->get_message_container( ).

    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    TRY.
        lo_pwreset = NEW #( CONV xubname( ls_data-bname ) ).
        lo_pwreset->unlock_user( if_send_mail = abap_true ).
        er_entity-bname = ls_data-bname.
      CATCH zcx_ca_sap_user INTO DATA(lx_cx_user).
        CALL METHOD lo_message_container->add_message_text_only(
            iv_msg_type               = 'E'
            iv_msg_text               = CONV #( lx_cx_user->get_longtext( ) )
            iv_add_to_response_header = abap_true ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid            = /iwbep/cx_mgw_busi_exception=>business_error
            message_container = lo_message_container.

    ENDTRY.


  ENDMETHOD.
ENDCLASS.
