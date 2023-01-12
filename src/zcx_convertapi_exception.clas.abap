class ZCX_CONVERTAPI_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  INTERFACES if_t100_dyn_msg .
  INTERFACES if_t100_message .

  constants REQUEST_ERROR type SOTR_CONC value '0800276976981EEDA185C80EE9C60CFB' ##NO_TEXT.
  constants CLASSIC_EXCEPTION type SOTR_CONC value '0800276976981EEDA1A41539A86FCCFB' ##NO_TEXT.
  data HTTP_CODE type INTEGER .
  data CODE type INTEGER .
  data RESPONSE type STRING.
  data MESSAGE type STRING.
  data METHOD type STRING.
  data EXCEPTION type STRING.

  methods CONSTRUCTOR
    importing
      !textid   LIKE if_t100_message=>t100key OPTIONAL
      !PREVIOUS like PREVIOUS optional
      !HTTP_CODE type INTEGER optional
      !CODE type INTEGER optional
      !RESPONSE type STRING optional .
  class-methods RAISE
    importing
      !MESSAGE   type STRING
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  class-methods RAISE_CLASSIC
    importing
      !METHOD    type STRING
      !EXCEPTION type STRING
      !MESSAGE   type STRING
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  class-methods RAISE_RESPONSE
    importing
      !HTTP_CODE type INTEGER
      !RESPONSE type STRING optional
    raising
      ZCX_CONVERTAPI_EXCEPTION .

    METHODS if_message~get_longtext
        REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CONVERTAPI_EXCEPTION IMPLEMENTATION.


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


  METHOD raise_classic.

    DATA ls_textid LIKE if_t100_message=>t100key.

    ls_textid-msgid = 'ZCONVERAPI'.
    ls_textid-msgno = '001'.
    ls_textid-attr1 = method.
    ls_textid-attr4 = exception.

    RAISE EXCEPTION TYPE zcx_convertapi_exception
      EXPORTING
        textid    = ls_textid.

  ENDMETHOD.

  METHOD RAISE.

    DATA ls_textid LIKE if_t100_message=>t100key.

    ls_textid-msgid = 'ZCONVERAPI'.
    ls_textid-msgno = '002'.
    ls_textid-attr1 = message.

    RAISE EXCEPTION TYPE zcx_convertapi_exception
      EXPORTING
        textid    = ls_textid.

  ENDMETHOD.

  METHOD raise_response.

    DATA ls_textid LIKE if_t100_message=>t100key.

    DATA:
      BEGIN OF ls_response,
        code    TYPE string,
        message TYPE string,
      END OF ls_response.


    ls_textid-msgid = 'ZCONVERAPI'.
    ls_textid-msgno = '000'.
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
        textid    = ls_textid.

  ENDMETHOD.

  METHOD if_message~get_longtext.

    IF me->response IS NOT INITIAL.
      result = me->response.
    ELSE.
      result = super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.
