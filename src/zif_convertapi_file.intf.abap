INTERFACE zif_convertapi_file
  PUBLIC .

  METHODS upload.

  METHODS delete.

  METHODS get_content
    RETURNING
      VALUE(rv_content) TYPE xstring .

  METHODS get_size
    RETURNING
      VALUE(rv_size) TYPE integer.

  METHODS get_id
    RETURNING
      VALUE(rv_id) TYPE string .

  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE string .

  METHODS get_name
    RETURNING
      VALUE(rv_name) TYPE string .

  METHODS get_ext
    RETURNING
      VALUE(rv_ext) TYPE string .

  METHODS convert_to
    IMPORTING
      !i_conversion TYPE any
    RETURNING
      VALUE(ro_file)    TYPE REF TO zif_convertapi_file .

  METHODS has_service_side_copy
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
ENDINTERFACE.
