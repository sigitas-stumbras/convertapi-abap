*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

    TYPES:
      BEGIN OF sty_upload_response_body,
        file_id   TYPE zif_convertapi_client=>ty_file_id,
        file_name TYPE string,
        file_ext  TYPE string,
        file_size TYPE i,
        url       TYPE string,
      END OF sty_upload_response_body .

    TYPES:
      BEGIN OF sty_convert_request_filevalue,
        name TYPE string,
        data TYPE string,
        url  TYPE string,
        id   TYPE zif_convertapi_client=>ty_file_id,
      END OF sty_convert_request_filevalue .

    TYPES:
      BEGIN OF sty_convert_request_parameters,
        name        TYPE string,
        value       TYPE string,
        file_value  TYPE sty_convert_request_filevalue,
        file_values TYPE TABLE OF sty_convert_request_filevalue WITH EMPTY KEY,
      END OF sty_convert_request_parameters .

    TYPES:
      BEGIN OF sty_convert_response_file,
        file_name TYPE string,
        file_ext  TYPE string,
        file_size TYPE i,
        file_id   TYPE string,
        url       TYPE string,
        file_data TYPE string,
      END OF sty_convert_response_file .

    TYPES:
      BEGIN OF sty_convert_request_body,
        parameters TYPE TABLE OF sty_convert_request_parameters WITH EMPTY KEY,
      END OF sty_convert_request_body .

    TYPES:
      BEGIN OF sty_convert_response_body,
        conversion_cost TYPE i,
        files           TYPE TABLE OF sty_convert_response_file WITH EMPTY KEY,
      END OF sty_convert_response_body .

    TYPES:
      BEGIN OF sty_error_response_body,
        code    TYPE i,
        message TYPE string,
      END OF sty_error_response_body .

    TYPES:
      BEGIN OF sty_user_response_body,
        id           TYPE string,
        secret       TYPE string,
        api_key      TYPE string,
        active       TYPE string,
        full_name    TYPE string,
        email        TYPE string,
        seconds_left TYPE string,
        max_workers  TYPE string,
      END OF sty_user_response_body.

    TYPES:
      BEGIN OF sty_user_stats_response_item,
        id                      TYPE string,
        result                  TYPE string,
        date_stamp              TYPE string,
        conversion_time         TYPE string,
        conversion_cost         TYPE string,
        converter               TYPE string,
        source_file_format      TYPE string,
        destination_file_format TYPE string,
        agent                   TYPE string,
        os                      TYPE string,
        ip                      TYPE string,
        source                  TYPE string,
        error                   TYPE string,
        user_id                 TYPE string,
      END OF sty_user_stats_response_item.

    TYPES:
      tty_user_stats_response_body TYPE TABLE OF sty_user_stats_response_item.

    CLASS lcl_fs DEFINITION.

      PUBLIC SECTION.

        CLASS-METHODS get_filename
          IMPORTING
            iv_path            TYPE string
          RETURNING
            VALUE(rv_filename) TYPE string.

        CLASS-METHODS get_filename_from_url
          IMPORTING
            iv_url             TYPE string
          RETURNING
            VALUE(rv_filename) TYPE string.

        CLASS-METHODS iso_date
          IMPORTING
                    iv_date       TYPE dats
          RETURNING VALUE(rv_iso) TYPE string.

        CLASS-METHODS iso_date_to_tzntstmpl
          IMPORTING
                    iv_isotimestamp     TYPE string
          RETURNING VALUE(rv_timestamp) TYPE zconvertapi_datestamp.

        CLASS-METHODS read_physical_file
          IMPORTING
            iv_path           TYPE string
          RETURNING
            VALUE(rv_content) TYPE xstring
          RAISING
            zcx_convertapi_exception.

    ENDCLASS.
