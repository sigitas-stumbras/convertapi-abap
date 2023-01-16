INTERFACE zif_convertapi_file
  PUBLIC .


  METHODS upload
    RETURNING
      VALUE(ro_same_file) TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .
  METHODS delete_service_side_copy
    RETURNING
      VALUE(ro_same_file) TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .
  METHODS get_content
    RETURNING
      VALUE(rv_content) TYPE xstring
    RAISING
      zcx_convertapi_exception .
  METHODS get_size
    RETURNING
      VALUE(rv_size) TYPE integer .
  METHODS get_id
    RETURNING
      VALUE(rv_id) TYPE string .
  METHODS get_convertapi_url
    RETURNING
      VALUE(rv_url) TYPE string .
  METHODS get_external_url
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
      !i_conversion      TYPE any
    RETURNING
      VALUE(ro_new_file) TYPE REF TO zif_convertapi_file
    RAISING
      zcx_convertapi_exception .
  METHODS has_service_side_copy
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
ENDINTERFACE.
