CLASS zcl_convertapi_conversion DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_convertapi_client.

  PUBLIC SECTION.

    INTERFACES zif_convertapi_conversion.

    TYPES:
      BEGIN OF sty_parameter,
        name  TYPE string,
        value TYPE string,
      END OF sty_parameter.

    TYPES tty_parameters TYPE STANDARD TABLE OF sty_parameter WITH KEY name.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA client TYPE REF TO zcl_convertapi_client.
    DATA result_format TYPE string.
    DATA source_format TYPE string.
    DATA parameters TYPE tty_parameters.

    CLASS-METHODS factory
      IMPORTING
        !i_conversion        TYPE any
        !io_client           TYPE REF TO zcl_convertapi_client
        !iv_source_format    TYPE string OPTIONAL
        !it_parameters       TYPE tty_parameters OPTIONAL
      RETURNING
        VALUE(ro_conversion) TYPE REF TO zif_convertapi_conversion
      RAISING
        zcx_convertapi_exception.

    METHODS constructor
      IMPORTING
        !im_result_format TYPE string.

ENDCLASS.

CLASS zcl_convertapi_conversion IMPLEMENTATION.

  METHOD constructor.

    result_format = im_result_format.

  ENDMETHOD.

  METHOD factory.

    DATA lo_typedescr  TYPE REF TO cl_abap_typedescr.
    DATA lo_classdescr TYPE REF TO cl_abap_classdescr.
    DATA lv_result_format TYPE string.
    DATA lo_conversion TYPE REF TO zcl_convertapi_conversion.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( i_conversion  ).

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        ro_conversion = i_conversion.
      WHEN cl_abap_typedescr=>typekind_csequence
        OR cl_abap_typedescr=>typekind_clike
        OR cl_abap_typedescr=>typekind_string
        OR cl_abap_typedescr=>typekind_char.

        lv_result_format = i_conversion.
        lo_conversion = NEW zcl_convertapi_conversion(
            im_result_format = lv_result_format

        ).
        APPEND LINES OF it_parameters TO lo_conversion->parameters.
        lo_conversion->client        = io_client.
        lo_conversion->source_format = iv_source_format.

        ro_conversion = lo_conversion.
      WHEN OTHERS.
        zcx_convertapi_exception=>raise( message = 'Could not create conversion from data type provided' ).
    ENDCASE.

  ENDMETHOD.

  METHOD zif_convertapi_conversion~convert.

    me->client->convert(
      EXPORTING
        i_source       = i_source
        io_parameters  = me
      IMPORTING
        et_target_files = rt_files
    ).

  ENDMETHOD.

  METHOD zif_convertapi_conversion~clear_parameter.
    DELETE parameters[] WHERE name = iv_name.
  ENDMETHOD.

  METHOD zif_convertapi_conversion~get_parameter.
    FIELD-SYMBOLS: <param> LIKE LINE OF parameters.
    READ TABLE parameters WITH KEY name = iv_name ASSIGNING <param>.
    IF sy-subrc = 0.
      rv_value = <param>-value.
    ENDIF.
  ENDMETHOD.

  METHOD zif_convertapi_conversion~get_parameters.
    CLEAR: rt_parameters.
    APPEND LINES OF me->parameters TO rt_parameters.
  ENDMETHOD.

  METHOD zif_convertapi_conversion~get_result_format.
    rv_result_format = result_format.
  ENDMETHOD.

  METHOD zif_convertapi_conversion~get_source_format.
    DATA lo_file LIKE LINE OF it_files.

    IF me->source_format IS NOT INITIAL.
        rv_source_format = me->source_format.
    ELSEIF it_files[] IS NOT INITIAL.
      READ TABLE it_files[] INDEX 1 INTO lo_file.
      rv_source_format = lo_file->get_ext(  ).
    ELSE.
      zcx_convertapi_exception=>raise( 'Unable to determine source format for conversion' ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_conversion~set_parameter.
    FIELD-SYMBOLS: <param> LIKE LINE OF parameters.
    READ TABLE parameters WITH KEY name = iv_name ASSIGNING <param>.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO parameters ASSIGNING <param>.
      <param>-name = iv_name.
      <param>-value = iv_value.
    ELSEIF iv_overwrite = abap_true.
      <param>-value = iv_value.
    ENDIF.
  ENDMETHOD.

  METHOD zif_convertapi_conversion~set_parameters.
    FIELD-SYMBOLS: <param> LIKE LINE OF it_parameters.
    LOOP AT it_parameters ASSIGNING <param>.
      me->zif_convertapi_conversion~set_parameter(
        EXPORTING
          iv_name      = <param>-name
          iv_value     = <param>-value
          iv_overwrite = iv_overwrite
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
