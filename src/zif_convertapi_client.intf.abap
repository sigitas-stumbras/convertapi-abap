INTERFACE zif_convertapi_client
  PUBLIC .

  CONSTANTS: version TYPE string VALUE '0.0.1'.

  CONSTANTS:
    BEGIN OF c_param,
      file                           TYPE string VALUE 'File',
      files                          TYPE string VALUE 'Files',
      store_file                     TYPE string VALUE 'StoreFile',
      timeout                        TYPE string VALUE 'Timeout',
    END OF c_param.

  TYPES ty_file_id TYPE string .
  TYPES tty_files TYPE TABLE OF REF TO zif_convertapi_file WITH EMPTY KEY.

  TYPES: BEGIN OF sty_parameter,
           name  TYPE string,
           value TYPE string,
         END OF sty_parameter.

  TYPES tty_parameters TYPE STANDARD TABLE OF sty_parameter WITH KEY name.

  METHODS create_file
    IMPORTING
      iv_name        TYPE string
      iv_content     TYPE xstring
    RETURNING
      VALUE(ro_file) TYPE REF TO zif_convertapi_file.

  METHODS create_file_from_url
    IMPORTING
      iv_url         TYPE string
      iv_name        TYPE string
    RETURNING
      VALUE(ro_file) TYPE REF TO zif_convertapi_file.

  METHODS create_conversion
    IMPORTING
      iv_target_format     TYPE string
      it_parameters        TYPE zif_convertapi_client=>tty_parameters OPTIONAL
    RETURNING
      VALUE(ro_conversion) TYPE REF TO zif_convertapi_conversion.

  METHODS convert
    IMPORTING
      !i_source              TYPE any
      !io_conversion         TYPE REF TO zcl_convertapi_conversion
    RETURNING
      VALUE(rt_target_files) TYPE zif_convertapi_client=>tty_files.

  METHODS set_auto_cleanup
    IMPORTING
      iv_enabled           TYPE abap_bool.

  METHODS get_auto_cleanup
    RETURNING
        VALUE(rv_enabled)  TYPE abap_bool.

  METHODS cleanup.

ENDINTERFACE.
