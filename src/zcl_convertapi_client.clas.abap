CLASS zcl_convertapi_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_file_id TYPE string .
    TYPES tty_files TYPE TABLE OF REF TO zif_convertapi_file WITH EMPTY KEY.

    TYPES: BEGIN OF sty_parameter,
             name  TYPE string,
             value TYPE string,
           END OF sty_parameter.

    TYPES tty_parameters TYPE STANDARD TABLE OF sty_parameter WITH KEY name.

    TYPES:
      BEGIN OF sty_upload_response_body,
        file_id   TYPE zcl_convertapi_client=>ty_file_id,
        file_name TYPE string,
        file_ext  TYPE string,
        file_size TYPE integer,
        url       TYPE string,
      END OF sty_upload_response_body .

    TYPES:
      BEGIN OF sty_convert_request_filevalues,
        name TYPE string,
        data TYPE xstring,
        url  TYPE string,
        id   TYPE ty_file_id,
      END OF sty_convert_request_filevalues .

    TYPES:
      BEGIN OF sty_convert_request_parameters,
        name        TYPE string,
        value       TYPE string,
        file_values TYPE TABLE OF zcl_convertapi_client=>sty_convert_request_filevalues WITH EMPTY KEY,
      END OF sty_convert_request_parameters .

    TYPES:
      BEGIN OF sty_convert_response_file,
        file_name TYPE string,
        file_ext  TYPE string,
        file_size TYPE integer,
        file_id   TYPE string,
        url       TYPE string,
        file_data TYPE string,
      END OF sty_convert_response_file .

    TYPES:
      BEGIN OF sty_convert_request_body,
        parameters TYPE TABLE OF zcl_convertapi_client=>sty_convert_request_parameters WITH EMPTY KEY,
      END OF sty_convert_request_body .

    TYPES:
      BEGIN OF sty_convert_response_body,
        conversion_cost TYPE integer,
        files           TYPE TABLE OF zcl_convertapi_client=>sty_convert_response_file WITH EMPTY KEY,
      END OF sty_convert_response_body .

    TYPES:
      BEGIN OF sty_error_response_body,
        code    TYPE integer,
        message TYPE string,
      END OF sty_error_response_body .

    DATA: mo_http_client          TYPE REF TO if_http_client .
    DATA: mv_api_secret           TYPE string .
    DATA: mv_api_key              TYPE string .
    DATA: mv_service_side_storage TYPE abap_bool.

    METHODS constructor
      IMPORTING
        !im_api_secret           TYPE string
        !im_api_key              TYPE string
        !im_service_side_storage TYPE abap_bool DEFAULT abap_true
        !im_http_client          TYPE REF TO if_http_client .

    METHODS upload
      IMPORTING
        !im_file_name    TYPE string
        !im_file_content TYPE xstring
      RETURNING
        VALUE(re_file)   TYPE REF TO zif_convertapi_file
      RAISING
        zcx_convertapi_service .

    METHODS download
      IMPORTING
        !io_file         TYPE REF TO zif_convertapi_file
      EXPORTING
        !ex_file_content TYPE xstring .

    METHODS delete
      IMPORTING
        !io_file TYPE REF TO zif_convertapi_file.

    METHODS convert
      IMPORTING
        !im_source             TYPE any
        !im_parameters         TYPE REF TO zcl_convertapi_conversion
      EXPORTING
        VALUE(et_target_files) TYPE zcl_convertapi_client=>tty_files.


  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS: c_request_method_delete TYPE string VALUE 'DELETE'.

    METHODS check_response_for_errors .
    METHODS process_client_error .

    METHODS parse_json
      IMPORTING
        im_json TYPE string
      CHANGING
        ch_data TYPE data.

    CLASS-METHODS normalize_source
      IMPORTING
        !im_source      TYPE any OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zcl_convertapi_client=>tty_files.

    CLASS-METHODS get_convert_request_body
      IMPORTING
                it_files       TYPE zcl_convertapi_client=>tty_files
                io_conversion  TYPE REF TO zcl_convertapi_conversion
      RETURNING VALUE(rv_json) TYPE string.

    CLASS-METHODS get_convert_request_url
      IMPORTING
                it_files      TYPE zcl_convertapi_client=>tty_files
                io_conversion TYPE REF TO zcl_convertapi_conversion
      RETURNING VALUE(rv_url) TYPE string.

    CLASS-METHODS to_json
      IMPORTING
                im_data        TYPE data
      RETURNING VALUE(rv_json) TYPE string.

ENDCLASS.



CLASS ZCL_CONVERTAPI_CLIENT IMPLEMENTATION.


  METHOD check_response_for_errors.

    mo_http_client->response->get_status(
      IMPORTING
          code   = DATA(status_code)
            reason = DATA(reason)
          ).

    IF status_code >= 400.

      DATA(raw) = mo_http_client->response->get_raw_message( ).

      RAISE EXCEPTION TYPE cx_http_ext_exception
        EXPORTING
          msg = `HTTP ` && status_code && ` - ` && reason.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    mv_api_secret           = im_api_secret.
    mv_api_key              = im_api_key.
    mo_http_client          = im_http_client.
    mv_service_side_storage = im_service_side_storage.
  ENDMETHOD.


  METHOD convert.

    DATA: lt_source_files   TYPE zcl_convertapi_client=>tty_files.
    DATA: lo_conversion     TYPE REF TO zcl_convertapi_conversion.
    DATA: lv_request_body   TYPE string.
    DATA: lv_request_url    TYPE string.
    DATA: lv_response_body  TYPE string.
    DATA: ls_response       TYPE sty_convert_response_body.

    FIELD-SYMBOLS: <file> LIKE LINE OF ls_response-files,
            <source_file> LIKE LINE OF lt_source_files.

    lt_source_files = normalize_source( im_source = im_source ).

    lo_conversion = zcl_convertapi_conversion=>factory(
            im_result_format = im_parameters
        ).

    IF mv_service_side_storage = abap_true.
      LOOP AT lt_source_files ASSIGNING <source_file>.
        IF <source_file>->has_service_side_copy( ) = abap_false.
          <source_file>->upload(  ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    lv_request_body = get_convert_request_body(
            it_files = lt_source_files
            io_conversion = lo_conversion
        ).

    lv_request_url = get_convert_request_url(
            it_files      = lt_source_files
            io_conversion = lo_conversion
        ).

    cl_http_utility=>set_request_uri(
            request = mo_http_client->request
            uri     = lv_request_url
        ).

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = mo_http_client->request->co_request_method_post ).
    mo_http_client->request->set_cdata( data =  lv_request_body ).

    mo_http_client->send( EXCEPTIONS  OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    mo_http_client->receive( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    check_response_for_errors( ).

    mo_http_client->response->get_status(
      IMPORTING
         code   = DATA(response_status_code)
      ).

    lv_response_body = mo_http_client->response->get_cdata( ).

    IF response_status_code = 200.

      me->parse_json(
        EXPORTING
          im_json = lv_response_body
        CHANGING
          ch_data = ls_response
      ).

      LOOP AT ls_response-files[] ASSIGNING <file>.

        APPEND zcl_convertapi_file=>factory(
            client         =  me
            id             = <file>-file_id
            name           = <file>-file_name
            ext            = <file>-file_ext
            size           = <file>-file_size
            url            = <file>-url
            content_base64 = <file>-file_data
        ) TO et_target_files.

      ENDLOOP.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.

      " TODO:  RAISE EXCEPTION TYPE zcx_convertapi_service

    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD delete.

    DATA lv_file_name TYPE string.
    DATA lv_content_disposition_add TYPE string VALUE ''.

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = io_file->get_url(  ) ).

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = c_request_method_delete ).

    mo_http_client->send( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    mo_http_client->receive( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    check_response_for_errors( ).

    mo_http_client->response->get_status(
      IMPORTING
         code   = DATA(response_status_code)
      ).

    IF response_status_code = 200.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      " TODO raise exception
    ELSE.
    ENDIF.


  ENDMETHOD.


  METHOD download.

    DATA lv_file_name TYPE string.
    DATA lv_content_disposition_add TYPE string VALUE ''.

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = io_file->get_url(  ) ).

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = mo_http_client->request->co_request_method_get ).

    mo_http_client->send( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    mo_http_client->receive( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    check_response_for_errors( ).

    mo_http_client->response->get_status(
      IMPORTING
         code   = DATA(response_status_code)
      ).

    IF response_status_code = 200.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      " TODO raise exception
    ELSE.
    ENDIF.

  ENDMETHOD.


  METHOD get_convert_request_body.
    DATA ls_body TYPE sty_convert_request_body.

    FIELD-SYMBOLS:
      <conv_param> LIKE LINE OF io_conversion->parameters[],
      <body_param> LIKE LINE OF ls_body-parameters,
      <file>       LIKE LINE OF it_files,
      <file_value> LIKE LINE OF <body_param>-file_values.

    IF it_files[] IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name = 'Files'.
      LOOP AT it_files[] ASSIGNING <file>.
        APPEND INITIAL LINE TO <body_param>-file_values ASSIGNING <file_value>.
        <file_value>-id = <file>->get_id(  ).
      ENDLOOP.
    ENDIF.

    LOOP AT io_conversion->parameters[] ASSIGNING <conv_param>.
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name = <conv_param>-name.
      <body_param>-value = <conv_param>-value.
    ENDLOOP.

    rv_json = to_json( ls_body ).

  ENDMETHOD.


  METHOD get_convert_request_url.
    DATA: lo_file TYPE REF TO zif_convertapi_file.

    READ TABLE it_files[] INDEX 1 INTO lo_file.
    rv_url = '/convert/' && lo_file->get_ext( ) && '/to/' && io_conversion->result_format.
  ENDMETHOD.


  METHOD normalize_source.

    DATA: lo_typedescr  TYPE REF TO cl_abap_typedescr,
          lo_classdescr TYPE REF TO cl_abap_classdescr.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( im_source ).

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        lo_classdescr ?= cl_abap_objectdescr=>describe_by_object_ref( im_source ).
        lo_classdescr->get_interface_type( 'ZIF_CONVERTAPI_FILE' ).
        APPEND im_source TO rt_files[].
      WHEN cl_abap_typedescr=>typekind_table.
    ENDCASE.
  ENDMETHOD.


  METHOD parse_json.

    /ui2/cl_json=>deserialize(
       EXPORTING
          json             = im_json
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*       assoc_arrays     =
*       assoc_arrays_opt =
*       name_mappings    =
*       conversion_exits =
       CHANGING
         data             = ch_data
     ).

  ENDMETHOD.


  METHOD process_client_error.
    DATA(resp) = mo_http_client->response->get_raw_message( ).

    DATA: str TYPE string.

    " TODO - fix me

    cl_abap_conv_in_ce=>create( input = resp )->read( IMPORTING data = str ).
    cl_demo_output=>display( str ).

    mo_http_client->get_last_error( IMPORTING message = DATA(rmsg) ).
*    cl_demo_output=>display( rmsg ).
*    MESSAGE rmsg TYPE 'E'.
    RAISE EXCEPTION TYPE cx_http_ext_exception
      EXPORTING
*       textid   =
*       previous =
        msg = rmsg.
    MESSAGE e499(sy) WITH rmsg.

  ENDMETHOD.


  METHOD to_json.

    rv_json = /ui2/cl_json=>serialize(
        data          = im_data
        compress      = /ui2/cl_json=>c_bool-true
        pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
    ).

  ENDMETHOD.


  METHOD upload.

    DATA lv_file_name TYPE string.
    DATA lv_content_disposition_add TYPE string VALUE ''.

    " TODO: filename validation - exception or fix&go?

    lv_file_name = im_file_name.
    REPLACE ALL OCCURRENCES OF REGEX '[\x00-\x31\\/:"*?<>|]+' IN lv_file_name WITH 'X'. " striping filename invalid characters

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = `/upload` ).

    DATA(lv_file_name_safe) = lv_file_name.
    REPLACE ALL OCCURRENCES OF REGEX '[^\x32-\x7F]+' IN lv_file_name_safe WITH 'X'. " replacing non-ascii characters with X

    IF lv_file_name_safe <> im_file_name.
      lv_content_disposition_add = ` ;filename*=UTF-8''` && cl_http_utility=>if_http_utility~escape_url( im_file_name ).
    ENDIF.

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = mo_http_client->request->co_request_method_post ).
    mo_http_client->request->set_data( data = im_file_content ).

    mo_http_client->request->set_header_field(
        name  = 'Content-Disposition'
        value = 'inline; filename="' && im_file_name && '"' && lv_content_disposition_add
    ).

    mo_http_client->send( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    mo_http_client->receive( EXCEPTIONS OTHERS = 4 ).

    IF sy-subrc <> 0.
      process_client_error( ).
      RETURN.
    ENDIF.

    check_response_for_errors( ).

    mo_http_client->response->get_status(
      IMPORTING
         code   = DATA(response_status_code)
      ).

    DATA(response_body) = mo_http_client->response->get_cdata( ).

    DATA: response TYPE sty_upload_response_body.

    IF response_status_code = 200.

      me->parse_json(
        EXPORTING
          im_json = response_body
        CHANGING
          ch_data = response
      ).

      re_file = zcl_convertapi_file=>factory(
          client  =  me
          id      = response-file_id
          name    = response-file_name
          ext     = response-file_ext
          size    = response-file_size
          url     = response-url
          content = im_file_content
      ).

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      RAISE EXCEPTION TYPE zcx_convertapi_service
*        EXPORTING
*          textid   =
*          previous =
        .
    ELSE.
    ENDIF.

*    DATA(raw) = mo_http_client->response->get_raw_message( ).

*    cl_demo_output=>display( response ).

  ENDMETHOD.
ENDCLASS.
