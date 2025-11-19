CLASS zcl_business_partner_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /iwbep/if_mgw_entry_provider .
    METHODS constructor
      IMPORTING
        i_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider
        i_entity_name   TYPE string.
  PROTECTED SECTION.
    CONSTANTS c_businesspartnertype TYPE string VALUE 'A_BusinessPartnerType' ##NO_TEXT.
    DATA    go_data_provider       TYPE REF TO /iwbep/if_mgw_entry_provider.
    DATA   gf_entity_name TYPE string  .


  PRIVATE SECTION.

    DATA:
      gs_businesspartner TYPE cl_api_business_partne_mpc_ext=>ts_deep_business_partner,
      gt_ca_bp_default   TYPE SORTED TABLE OF zca_bp_default WITH UNIQUE KEY structure field counter.

    METHODS set_default_value_flat
      IMPORTING
        iv_header_struc TYPE typename
        iv_bukrs        TYPE bukrs OPTIONAL
        iv_channel      TYPE vtweg OPTIONAL
        iv_division     TYPE spart OPTIONAL
        iv_country      TYPE land1_gp
        iv_counter      TYPE int1 OPTIONAL
      CHANGING
        cs_structure    TYPE data.

    METHODS read_bp_default.

    METHODS get_country
      RETURNING VALUE(rv_country) TYPE land1_gp.

    METHODS get_default_value_field
      IMPORTING
                iv_fieldname    TYPE abap_compname
                iv_header_struc TYPE typename
                iv_bukrs        TYPE bukrs OPTIONAL
                iv_channel      TYPE vtweg OPTIONAL
                iv_division     TYPE spart OPTIONAL
                iv_country      TYPE land1_gp
                iv_counter      TYPE int1 OPTIONAL
      RETURNING VALUE(rv_value) TYPE zca_bp_default_value.

    METHODS constraint_method
      IMPORTING iv_method       TYPE zca_bp_constraint_method
                iv_bukrs        TYPE bukrs OPTIONAL
      RETURNING VALUE(rv_value) TYPE zca_bp_default_value.



ENDCLASS.

CLASS zcl_business_partner_provider IMPLEMENTATION.


  METHOD constructor.

    me->go_data_provider = i_data_provider.
    me->gf_entity_name = i_entity_name.
    me->read_bp_default( ).

  ENDMETHOD.


  METHOD /iwbep/if_mgw_entry_provider~read_entry_data.

    IF me->go_data_provider IS BOUND.
      me->go_data_provider->read_entry_data(
        IMPORTING
          es_data = es_data
      ).

      gs_businesspartner = CORRESPONDING #(  es_data ).

      IF me->gf_entity_name = c_businesspartnertype.

        gs_businesspartner = CORRESPONDING #(  es_data ).

        "first process main component
**********************************************************************
*       main-data
**********************************************************************
        me->set_default_value_flat( EXPORTING  iv_header_struc = '' iv_country = me->get_country( )
                                    CHANGING cs_structure = gs_businesspartner ). "for general data no cc and sales data

        "theoretically everything could be done dynamically by recursive call of the method but for now
        " we keep it static

        "then statically process TO_BUSINESSPARTNERROLE
**********************************************************************
*       role-data
**********************************************************************
        LOOP AT gt_ca_bp_default ASSIGNING FIELD-SYMBOL(<ls_default_role>)
            WHERE structure = 'TO_BUSINESSPARTNERROLE'.
          IF line_exists( gs_businesspartner-to_businesspartnerrole[ businesspartnerrole = <ls_default_role>-value ] ).
            ASSIGN gs_businesspartner-to_businesspartnerrole[ businesspartnerrole = <ls_default_role>-value ] TO FIELD-SYMBOL(<ls_businesspartnerrole>).
          ELSE.
            APPEND INITIAL LINE TO gs_businesspartner-to_businesspartnerrole ASSIGNING <ls_businesspartnerrole>.
          ENDIF.
          me->set_default_value_flat( EXPORTING iv_header_struc = 'TO_BUSINESSPARTNERROLE' iv_country = me->get_country( ) iv_counter = <ls_default_role>-counter
                                      CHANGING cs_structure = <ls_businesspartnerrole>  ). "for general data no cc and sales data
        ENDLOOP.

        "then statically process TO_BUSINESSPARTNERADDRESS
*        for now we assume, that there is only one address
**********************************************************************
*       address-data
**********************************************************************
        IF line_exists( gs_businesspartner-to_businesspartneraddress[ 1 ] ).
          me->set_default_value_flat( EXPORTING iv_header_struc = 'TO_BUSINESSPARTNERADDRESS' iv_country = me->get_country( ) iv_counter = 0
                                        CHANGING cs_structure = gs_businesspartner-to_businesspartneraddress[ 1 ]  ). "for general data no cc and sales data
        ENDIF.

        "then statically process TO_CUSTOMER
**********************************************************************
*       customer-data
**********************************************************************
        me->set_default_value_flat( EXPORTING iv_header_struc = 'TO_CUSTOMER' iv_country = me->get_country( ) iv_counter = 0
                                       CHANGING cs_structure = gs_businesspartner-to_customer ). "for general data no cc and sales data
        LOOP AT gs_businesspartner-to_customer-to_customersalesarea ASSIGNING FIELD-SYMBOL(<ls_cust_sales_area>).
          DATA lv_count TYPE int1.
          lv_count = sy-tabix - 1.
          me->set_default_value_flat( EXPORTING iv_header_struc = 'TO_CUSTOMERSALESAREA' iv_country = me->get_country( ) iv_counter = lv_count
                                            iv_bukrs = <ls_cust_sales_area>-salesorganization iv_channel = <ls_cust_sales_area>-distributionchannel iv_division = <ls_cust_sales_area>-division "with sales data
                                          CHANGING cs_structure = <ls_cust_sales_area>  ).
          "for each sales area a company code must exist
          IF line_exists( gs_businesspartner-to_customer-to_customercompany[ companycode = <ls_cust_sales_area>-salesorganization ] ).
            ASSIGN gs_businesspartner-to_customer-to_customercompany[ companycode = <ls_cust_sales_area>-salesorganization ] TO FIELD-SYMBOL(<ls_company_code_data>).
          ELSE.
            APPEND INITIAL LINE TO gs_businesspartner-to_customer-to_customercompany ASSIGNING <ls_company_code_data>.
          ENDIF.
          me->set_default_value_flat( EXPORTING iv_header_struc = 'TO_CUSTOMERCOMPANY' iv_country = me->get_country( ) iv_counter = 0
                                            iv_bukrs = <ls_cust_sales_area>-salesorganization iv_channel = <ls_cust_sales_area>-distributionchannel iv_division = <ls_cust_sales_area>-division "with sales data
                                          CHANGING cs_structure = <ls_company_code_data>  ).

        ENDLOOP.

*  CATCH /iwbep/cx_mgw_tech_exception.

        es_data = CORRESPONDING #(  gs_businesspartner ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_default_value_flat.

*   gs_businesspartner = CORRESPONDING #(  es_data ).


    DATA: lr_descr_struc TYPE REF TO data.
    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.

    CREATE DATA lr_descr_struc LIKE cs_structure.
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data_ref( p_data_ref = lr_descr_struc ).
    ASSIGN cs_structure TO FIELD-SYMBOL(<ls_structure>).


    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<lv_component>)
    WHERE ( type_kind <> 'h' AND type_kind <> 'v' ) .
      ASSIGN COMPONENT <lv_component>-name OF STRUCTURE <ls_structure> TO FIELD-SYMBOL(<lv_field>).
      IF <lv_field> IS INITIAL. "only set default value if empty

        <lv_field> = me->get_default_value_field(
                       iv_fieldname    = <lv_component>-name
                       iv_header_struc = iv_header_struc
                       iv_bukrs        = iv_bukrs
                       iv_channel      = iv_channel
                       iv_division     =  iv_division
                       iv_country      = iv_country
                       iv_counter      = iv_counter
                     ).
        IF <lv_field> = ''. "in order to set e.g. an initial date back to '000000' instead of ''
          CLEAR <lv_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD read_bp_default.

    SELECT * FROM zca_bp_default INTO TABLE gt_ca_bp_default.

  ENDMETHOD.

  METHOD get_country.

    TRY.
        rv_country = gs_businesspartner-to_businesspartneraddress[ 1 ]-country.

      CATCH cx_sy_itab_line_not_found.
        rv_country = 'DE'.
    ENDTRY.

  ENDMETHOD.

  METHOD get_default_value_field.
*        iv_fieldname    TYPE abap_compname
*        iv_header_struc TYPE typename
*        iv_bukrs        TYPE bukrs OPTIONAL
*        iv_channel      TYPE vtweg OPTIONAL
*        iv_division     TYPE spart OPTIONAL
*        iv_country      TYPE land1_gp.

    DATA(lt_default_filt) = FILTER #( gt_ca_bp_default WHERE  structure = iv_header_struc AND field = iv_fieldname AND counter = iv_counter ).

    IF line_exists( lt_default_filt[ 1 ]  ).

      "perform access sequence starting from special
*   *** 1st access using CoCd, Dchl, Dv and Country
      IF line_exists( lt_default_filt[ bukrs = iv_bukrs vtweg = iv_channel spart = iv_division country = iv_country ] ).
        DATA(ls_default) =  lt_default_filt[ bukrs = iv_bukrs vtweg = iv_channel spart = iv_division country = iv_country ].
        IF ls_default-constraint_method IS NOT INITIAL.
          rv_value = me->(ls_default-constraint_method).
        ELSE.
          rv_value = ls_default-value.
        ENDIF.

*   *** 2nd access using CoC and Country
      ELSEIF line_exists( lt_default_filt[ bukrs = iv_bukrs country = iv_country vtweg = '' spart = '' ] ).
        ls_default =  lt_default_filt[ bukrs = iv_bukrs country = iv_country vtweg = '' spart = '' ].

*   *** 3rd access using CoC
      ELSEIF line_exists( lt_default_filt[ bukrs = iv_bukrs country = '' vtweg = '' spart = ''  ] ).
        ls_default =  lt_default_filt[ bukrs = iv_bukrs country = '' vtweg = '' spart = '' ].

*   *** 4rth access using Country
      ELSEIF line_exists( lt_default_filt[ country = iv_country bukrs = '' vtweg = '' spart = '' ] ).
        ls_default =  lt_default_filt[ country = iv_country bukrs = '' vtweg = '' spart = '' ].

*   *** last access using only key fields
      ELSEIF line_exists( lt_default_filt[ country = '' bukrs = '' vtweg = '' spart = ''  ] ).
        ls_default =  lt_default_filt[ country = '' bukrs = '' vtweg = '' spart = '' ].
      ENDIF.

      IF ls_default IS NOT INITIAL.
        IF ls_default-constraint_method IS NOT INITIAL.
          rv_value = me->constraint_method( iv_method =  ls_default-constraint_method iv_bukrs = iv_bukrs ).
        ELSEIF ls_default-value IS NOT INITIAL.
          rv_value = ls_default-value.
        ENDIF.

      ENDIF.
    ENDIF.


  ENDMETHOD.

  METHOD constraint_method.

    CASE iv_method.

      WHEN 'SET_SEARCHTERM1'.
        DATA lv_sort1 TYPE bu_sort1.
        lv_sort1 = gs_businesspartner-organizationbpname1.
        rv_value = lv_sort1.

      WHEN 'SET_SEARCHTERM2'.
        DATA lv_sort2 TYPE bu_sort2.
        lv_sort2 = gs_businesspartner-organizationbpname2.
        rv_value = lv_sort2.

      WHEN 'SET_CURRENCY'.
        "read currency from company code customizing
        SELECT SINGLE waers FROM t001 INTO @rv_value WHERE bukrs = @iv_bukrs.

      WHEN 'SET_CUSTOMER_ACCOUNT_ASSIGNMENT_GROUP'.
        DATA(lv_country_bp) = me->get_country( ).
        SELECT SINGLE t001~land1, t005~xegld FROM t001 JOIN t005 ON t001~land1 = t005~land1
        INTO @DATA(ls_landcc) WHERE bukrs = @iv_bukrs.

        SELECT SINGLE xegld FROM t005 INTO @DATA(lv_xegld) WHERE land1 = @lv_country_bp.

        IF ls_landcc-land1 = lv_country_bp.
          rv_value = '01'. "Domestic: country company code = country customer
        ELSEIF lv_xegld = abap_true AND ls_landcc-xegld = abap_true.
          rv_value = '02'. "EU: country company code EU <> country customer also EU
        ELSEIF lv_xegld = abap_false AND ls_landcc-xegld = abap_true.
          rv_value = '03'. "03: country company code EU <> country customer not EU
        ELSE.
          "*04: via partner type  (probably only necessary for changes)
          "05: for "Fachpartner" not relevant yet
        ENDIF.

      WHEN 'SET_INCOTERMSLOCATION1_CH'.
        rv_value = gs_businesspartner-to_businesspartneraddress[ 1 ]-cityname.

      WHEN 'SET_COMPANY_CODE'.
        rv_value = iv_bukrs.


    ENDCASE.


  ENDMETHOD.


ENDCLASS.
