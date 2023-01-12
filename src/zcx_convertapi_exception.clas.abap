CLASS zcx_convertapi_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS request_error TYPE sotr_conc VALUE '0800276976981EEDA185C80EE9C60CFB' ##NO_TEXT.
    CONSTANTS classic_exception TYPE sotr_conc VALUE '0800276976981EEDA1A41539A86FCCFB' ##NO_TEXT.
    DATA http_code TYPE integer .
    DATA code TYPE string .
    DATA response TYPE string.
    DATA message TYPE string.
    DATA method TYPE string.
    DATA exception TYPE string.

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !http_code TYPE integer OPTIONAL
        !code      TYPE string OPTIONAL
        !response  TYPE string OPTIONAL
        !message   TYPE string OPTIONAL .
    CLASS-METHODS raise
      IMPORTING
        !message TYPE string
      RAISING
        zcx_convertapi_exception .
    CLASS-METHODS raise_classic
      IMPORTING
        !method    TYPE string
        !exception TYPE string
        !message   TYPE string
      RAISING
        zcx_convertapi_exception .
    CLASS-METHODS raise_response
      IMPORTING
        !http_code TYPE integer
        !response  TYPE string OPTIONAL
      RAISING
        zcx_convertapi_exception .

    METHODS if_message~get_longtext
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_convertapi_exception IMPLEMENTATION.


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

    me->code = code.
    me->http_code = http_code.
    me->message = message.
    me->response = response.

  ENDMETHOD.


  METHOD raise_classic.

    DATA ls_textid LIKE if_t100_message=>t100key.

    ls_textid-msgid = 'ZCONVERTAPI'.
    ls_textid-msgno = '000'.
    ls_textid-attr1 = method.
    ls_textid-attr2 = exception.
    ls_textid-attr3 = message.

    RAISE EXCEPTION TYPE zcx_convertapi_exception
      EXPORTING
        textid  = ls_textid
        message = message.

  ENDMETHOD.

  METHOD raise.

    DATA ls_textid LIKE if_t100_message=>t100key.

    ls_textid-msgid = 'ZCONVERTAPI'.
    ls_textid-msgno = '002'.
    ls_textid-attr1 = message.

    RAISE EXCEPTION TYPE zcx_convertapi_exception
      EXPORTING
        textid = ls_textid.

  ENDMETHOD.

  METHOD raise_response.

    DATA ls_textid LIKE if_t100_message=>t100key.

    DATA:
      BEGIN OF ls_response,
        code    TYPE string,
        message TYPE string,
      END OF ls_response.


    ls_textid-msgid = 'ZCONVERTAPI'.
    ls_textid-msgno = '001'.
    ls_textid-attr1 = http_code.
    ls_textid-attr4 = response.

    IF response IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
           json             = response
           pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data             = ls_response
      ).

      ls_textid-attr2 = ls_response-code.
      ls_textid-attr3 = ls_response-message.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_convertapi_exception
      EXPORTING
        textid = ls_textid
        http_code = http_code
        code = ls_response-code
        message = ls_response-message
        response = response.

  ENDMETHOD.

  METHOD if_message~get_longtext.

    IF me->response IS NOT INITIAL.
      result = me->response.
    ELSEIF message IS NOT INITIAL.
      result = me->message.
    ELSE.
      super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.
