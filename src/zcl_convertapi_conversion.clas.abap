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

    DATA result_format TYPE string READ-ONLY .
    DATA parameters TYPE tty_parameters READ-ONLY .

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS factory
      IMPORTING
        !i_conversion        TYPE any
        !io_client           TYPE REF TO zcl_convertapi_client
        !it_parameters       TYPE tty_parameters OPTIONAL
      RETURNING
        VALUE(ro_conversion) TYPE REF TO zif_convertapi_conversion.

    METHODS constructor
      IMPORTING
        !io_client        TYPE REF TO zcl_convertapi_client
        !im_result_format TYPE string
        !it_parameters    TYPE tty_parameters OPTIONAL .

ENDCLASS.



CLASS zcl_convertapi_conversion IMPLEMENTATION.


  METHOD constructor.

    result_format = im_result_format.
    APPEND LINES OF it_parameters TO parameters.

  ENDMETHOD.

  METHOD factory.

    DATA: lo_typedescr  TYPE REF TO cl_abap_typedescr.
    DATA: lo_classdescr TYPE REF TO cl_abap_classdescr.
    DATA: lv_result_format TYPE string.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( i_conversion  ).

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        ro_conversion = i_conversion.
      WHEN cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike OR cl_abap_typedescr=>typekind_string.
        lv_result_format = i_conversion.
        ro_conversion = NEW zcl_convertapi_conversion(
            io_client        = io_client
            im_result_format = lv_result_format
            it_parameters    = it_parameters
        ).
    ENDCASE.

  ENDMETHOD.

  METHOD zif_convertapi_conversion~convert.

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
