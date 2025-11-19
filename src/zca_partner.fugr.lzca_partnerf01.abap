*----------------------------------------------------------------------*
***INCLUDE LZCA_PARTNERF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_USERDATA
*&---------------------------------------------------------------------*
FORM get_userdata  USING iv_user TYPE usr01-bname.
  DATA: ls_address    LIKE addr3_val
      , ls_usr03      LIKE usr03
      .

  CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
    EXPORTING
      user_name              = iv_user
*     READ_DB_DIRECTLY       = ' '
    IMPORTING
      user_address           = ls_address
      user_usr03             = ls_usr03
*     USER_USR21             =
    EXCEPTIONS
      user_address_not_found = 1
      OTHERS                 = 2.
  IF sy-subrc = 0.
    <gs_partner>-name = ls_address-name_text.
    IF ls_address-tel_extens IS INITIAL.
      MOVE ls_address-tel_number TO <gs_partner>-tel.
    ELSE.
      CONCATENATE ls_address-tel_number '-' ls_address-tel_extens
      INTO <gs_partner>-tel.
    ENDIF.
    CALL FUNCTION 'LWE_GET_EMAIL'
      EXPORTING
        username      = iv_user
      IMPORTING
        email_address = <gs_partner>-email.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_PARTNER_ZS
*&---------------------------------------------------------------------*
FORM fill_partner_zs  USING  is_vbpa TYPE vbpa.
  DATA: ls_addr1_complete TYPE szadr_addr1_complete
      .

  CALL FUNCTION 'ADDR_GET_COMPLETE'
    EXPORTING
      addrnumber              = is_vbpa-adrnr
*     ADDRHANDLE              =
*     ARCHIVE_HANDLE          =
      iv_current_comm_data    = 'X'
*     BLK_EXCPT               =
    IMPORTING
      addr1_complete          = ls_addr1_complete
    EXCEPTIONS
      parameter_error         = 1
      address_not_exist       = 2
      internal_error          = 3
      wrong_access_to_archive = 4
      address_blocked         = 5
      OTHERS                  = 6.
  IF sy-subrc = 0.
    READ TABLE ls_addr1_complete-addr1_tab INTO DATA(ls_addr1) INDEX 1.   "FLGDEFAULT = 'X'?
    CHECK sy-subrc = 0.

    <gs_partner>-title = ls_addr1-data-title.
    <gs_partner>-name  = ls_addr1-data-name1.

    IF <gs_partner>-title IS NOT INITIAL.
      SELECT SINGLE title_medi FROM tsad3t
        INTO <gs_partner>-title_medi
        WHERE title = <gs_partner>-title
          AND langu = gv_langu.
    ENDIF.

    " Telefon
    READ TABLE ls_addr1_complete-adtel_tab INTO DATA(ls_adtel) INDEX 1.
    IF sy-subrc = 0.
      <gs_partner>-tel = ls_adtel-adtel-telnr_long.
    ENDIF.

    " email
    READ TABLE ls_addr1_complete-adsmtp_tab INTO DATA(ls_adsmtp) INDEX 1.
    IF sy-subrc = 0.
      <gs_partner>-email = ls_adsmtp-adsmtp-smtp_addr.
    ENDIF.

  ENDIF.
ENDFORM.
