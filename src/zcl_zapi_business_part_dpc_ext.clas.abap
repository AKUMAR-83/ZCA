CLASS zcl_zapi_business_part_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zapi_business_part_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity
        REDEFINITION .

ENDCLASS.



CLASS zcl_zapi_business_part_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
    DATA lo_custom_provider TYPE REF TO /iwbep/if_mgw_entry_provider.

    lo_custom_provider ?= NEW zcl_business_partner_provider(
        i_data_provider = io_data_provider
        i_entity_name = iv_entity_name
     ).

    TRY.
        CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~create_deep_entity
          EXPORTING
            iv_entity_name          = iv_entity_name
            iv_entity_set_name      = iv_entity_set_name
            iv_source_name          = iv_source_name
            io_data_provider        = lo_custom_provider "io_data_provider "
            it_key_tab              = it_key_tab
            it_navigation_path      = it_navigation_path
            io_expand               = io_expand
            io_tech_request_context = io_tech_request_context
          IMPORTING
            er_deep_entity          = er_deep_entity.
      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
