FUNCTION z_ca_get_partner.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_USER) TYPE  USR01-BNAME OPTIONAL
*"     VALUE(IS_VBPA) TYPE  VBPA OPTIONAL
*"     REFERENCE(IV_LANGU) TYPE  SPRAS OPTIONAL
*"  EXPORTING
*"     VALUE(ES_PARTNER) TYPE  ZCA_PARTNER_S
*"----------------------------------------------------------------------
  MOVE iv_langu TO gv_langu.
  ASSIGN es_partner TO <gs_partner>.

  IF is_vbpa IS NOT INITIAL.
    PERFORM fill_partner_zs USING is_vbpa.
  ELSEIF iv_user IS NOT INITIAL.
    PERFORM get_userdata USING iv_user.
  ENDIF.
ENDFUNCTION.
