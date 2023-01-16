INTERFACE zif_convertapi_client
  PUBLIC .

  TYPES ty_storage_mode TYPE integer .
  TYPES ty_file_id      TYPE string .
  TYPES tty_files       TYPE TABLE OF REF TO zif_convertapi_file WITH EMPTY KEY .

  TYPES:
    BEGIN OF sty_parameter,
      name  TYPE string,
      value TYPE string,
    END OF sty_parameter .

  TYPES tty_parameters  TYPE STANDARD TABLE OF sty_parameter WITH KEY name .

  CONSTANTS version TYPE string VALUE '0.0.1' ##NO_TEXT.

  CONSTANTS:
    BEGIN OF c_param,
      file       TYPE string VALUE 'File',
      files      TYPE string VALUE 'Files',
      store_file TYPE string VALUE 'StoreFile',
      timeout    TYPE string VALUE 'Timeout',
    END OF c_param .

  CONSTANTS:
    BEGIN OF c_storage_mode,
      use_service_storage TYPE ty_storage_mode VALUE 1,
      no_service_storage  TYPE ty_storage_mode VALUE 2,
      manual              TYPE ty_storage_mode VALUE 3,
    END OF c_storage_mode .

  METHODS create_file
    IMPORTING
      !iv_filename   TYPE string
      !iv_content    TYPE xstring
    RETURNING
      VALUE(ro_file) TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .

  METHODS create_file_from_fs
    IMPORTING
      !iv_physical_file TYPE string
      !iv_filename      TYPE string OPTIONAL
    RETURNING
      VALUE(ro_file)    TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .

  METHODS create_file_from_url
    IMPORTING
      !iv_url        TYPE string
      !iv_filename   TYPE string
    RETURNING
      VALUE(ro_file) TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .

  METHODS create_conversion
    IMPORTING
      !iv_target_format    TYPE any
      !iv_source_format    TYPE string OPTIONAL
      !it_parameters       TYPE zif_convertapi_client=>tty_parameters OPTIONAL
    RETURNING
      VALUE(ro_conversion) TYPE REF TO zif_convertapi_conversion
    RAISING
      zcx_convertapi_exception .

  METHODS set_auto_cleanup
    IMPORTING
      !iv_enabled TYPE abap_bool .

  METHODS get_auto_cleanup
    RETURNING
      VALUE(rv_enabled) TYPE abap_bool .

  METHODS cleanup
    RAISING
      zcx_convertapi_exception .

  METHODS get_user_info
    RETURNING
      VALUE(rs_user_info) TYPE zconvertapi_s_user_info
    RAISING
      zcx_convertapi_exception.

  METHODS get_usage_history
    IMPORTING
      iv_date_from    TYPE dats
      iv_date_to      TYPE dats OPTIONAL
    RETURNING
      VALUE(rt_history) TYPE zconvertapi_tt_usage_history
    RAISING
      zcx_convertapi_exception.

ENDINTERFACE.
