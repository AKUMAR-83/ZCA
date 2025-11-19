CLASS zcx_ca_jira DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF zcx_ticket_not_found,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ticket_not_found.

      constants:
        begin of zcx_ticket_update,
          msgid type symsgid value 'ZCA',
          msgno type symsgno value '013',
          attr1 type scx_attrname value 'attr1',
          attr2 type scx_attrname value 'attr2',
          attr3 type scx_attrname value 'attr3',
          attr4 type scx_attrname value 'attr4',
        end of zcx_ticket_update.

        constants:
          begin of zcx_http_client,
            msgid type symsgid value 'ZCA',
            msgno type symsgno value '014',
            attr1 type scx_attrname value 'attr1',
            attr2 type scx_attrname value 'attr2',
            attr3 type scx_attrname value 'attr3',
            attr4 type scx_attrname value 'attr4',
          end of zcx_http_client.


    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ca_jira IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
