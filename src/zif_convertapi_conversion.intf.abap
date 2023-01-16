INTERFACE zif_convertapi_conversion
  PUBLIC .

  TYPES:
    BEGIN OF sty_parameter,
      name  TYPE string,
      value TYPE string,
    END OF sty_parameter.

  TYPES tty_parameters TYPE STANDARD TABLE OF sty_parameter WITH KEY name.

  METHODS convert
    IMPORTING
      i_source        TYPE any
    RETURNING
      VALUE(rt_files) TYPE zif_convertapi_client=>tty_files
    RAISING
      zcx_convertapi_exception.

  METHODS get_parameter
    IMPORTING
      !iv_name        TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.

  METHODS get_all_parameters
    RETURNING
      VALUE(rt_parameters) TYPE zif_convertapi_conversion=>tty_parameters.

  METHODS set_parameter
    IMPORTING
      !iv_name      TYPE string
      !iv_value     TYPE string
      !iv_overwrite TYPE abap_bool DEFAULT abap_true.

  METHODS set_parameters
    IMPORTING
      !it_parameters TYPE zif_convertapi_conversion=>tty_parameters
      !iv_overwrite  TYPE abap_bool DEFAULT abap_true.

  METHODS clear_parameter
    IMPORTING
      !iv_name TYPE string.

  METHODS get_result_format
    RETURNING
      VALUE(rv_result_format) TYPE string.

  METHODS get_source_format
    IMPORTING
      it_files TYPE zif_convertapi_client=>tty_files
    RETURNING
      VALUE(rv_source_format) TYPE string
    RAISING
      zcx_convertapi_exception.

ENDINTERFACE.
