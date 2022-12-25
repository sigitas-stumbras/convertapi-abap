CLASS zcl_convertapi_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

    GLOBAL FRIENDS zcl_convertapi_file .

  PUBLIC SECTION.

    INTERFACES zif_convertapi_client.

    CLASS-METHODS create
      IMPORTING
                !iv_api_secret           TYPE string
                !iv_api_key              TYPE string
                !io_http_client          TYPE REF TO if_http_client
                !iv_log_handle           TYPE balloghndl OPTIONAL
                !iv_service_side_storage TYPE abap_bool DEFAULT abap_true
                !iv_auto_cleanup         TYPE abap_bool DEFAULT abap_true

      RETURNING VALUE(ro_client)         TYPE REF TO zif_convertapi_client.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF sty_upload_response_body,
        file_id   TYPE zif_convertapi_client=>ty_file_id,
        file_name TYPE string,
        file_ext  TYPE string,
        file_size TYPE integer,
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
        file_value  TYPE zcl_convertapi_client=>sty_convert_request_filevalue,
        file_values TYPE TABLE OF zcl_convertapi_client=>sty_convert_request_filevalue WITH EMPTY KEY,
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
    DATA: mv_auto_cleanup         TYPE abap_bool.

    DATA: mv_log_handle           TYPE balloghndl.

    CONSTANTS:
      BEGIN OF c_request_method,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        delete TYPE string VALUE 'DELETE',
      END OF c_request_method.

    METHODS upload
      IMPORTING
        io_file TYPE REF TO zcl_convertapi_file
      RAISING
        zcx_convertapi_service .

    METHODS download
      IMPORTING
        !io_file         TYPE REF TO zcl_convertapi_file
      EXPORTING
        !ev_file_content TYPE xstring .

    METHODS delete
      IMPORTING
        !io_file TYPE REF TO zcl_convertapi_file
      RAISING
        zcx_convertapi_service .

    METHODS convert
      IMPORTING
        !im_source             TYPE any
        !im_parameters         TYPE REF TO zif_convertapi_conversion
      EXPORTING
        VALUE(et_target_files) TYPE zif_convertapi_client=>tty_files.

    METHODS check_response_for_errors .
    METHODS process_client_error .

    METHODS parse_json
      IMPORTING
        im_json TYPE string
      CHANGING
        ch_data TYPE data.

    METHODS add_authorization_credentials
      IMPORTING
        io_http_request TYPE REF TO if_http_request.

    CLASS-METHODS normalize_source
      IMPORTING
        !im_source      TYPE any OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_convertapi_client=>tty_files.

    METHODS get_convert_request_body
      IMPORTING
                it_files       TYPE zif_convertapi_client=>tty_files
                io_conversion  TYPE REF TO zif_convertapi_conversion
      RETURNING VALUE(rv_json) TYPE string.

    METHODS get_convert_request_url
      IMPORTING
                it_files      TYPE zif_convertapi_client=>tty_files
                io_conversion TYPE REF TO zif_convertapi_conversion
      RETURNING VALUE(rv_url) TYPE string.

    CLASS-METHODS to_json
      IMPORTING
                im_data        TYPE data
      RETURNING VALUE(rv_json) TYPE string.

    CLASS-METHODS get_filevalue
      IMPORTING
        io_file              TYPE REF TO zif_convertapi_file
      RETURNING
        VALUE(rs_file_value) TYPE zcl_convertapi_client=>sty_convert_request_filevalue.

    METHODS trace_log
      IMPORTING
        msg TYPE string.

ENDCLASS.

CLASS zcl_convertapi_client IMPLEMENTATION.

  METHOD create.

    DATA lo_client TYPE REF TO zcl_convertapi_client.

    lo_client = NEW zcl_convertapi_client( ).

    lo_client->mv_api_secret           = iv_api_secret.
    lo_client->mv_api_key              = iv_api_key.
    lo_client->mo_http_client          = io_http_client.
    lo_client->mv_service_side_storage = iv_service_side_storage.
    lo_client->mv_log_handle           = iv_log_handle.

    ro_client = lo_client.

  ENDMETHOD.

  METHOD zif_convertapi_client~convert.

  ENDMETHOD.

  METHOD zif_convertapi_client~create_conversion.

    ro_conversion = zcl_convertapi_conversion=>factory(
            io_client     = me
            i_conversion  =  iv_target_format
            it_parameters = it_parameters
        ).

  ENDMETHOD.

  METHOD zif_convertapi_client~create_file.

    ro_file = zcl_convertapi_file=>factory(
        client  =  me
        name    = iv_name
        content = iv_content
    ).

    IF me->mv_service_side_storage = abap_true.
      ro_file->upload(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_client~create_file_from_url.

    ro_file = zcl_convertapi_file=>factory(
        client = me
        name   = iv_name
        url    = iv_url
    ).

    IF me->mv_service_side_storage = abap_true.
      ro_file->upload(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_client~get_auto_cleanup.
    rv_enabled = mv_auto_cleanup.
  ENDMETHOD.

  METHOD zif_convertapi_client~set_auto_cleanup.
    mv_auto_cleanup = iv_enabled.
  ENDMETHOD.

  METHOD zif_convertapi_client~cleanup.

  ENDMETHOD.

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

  METHOD convert.

    me->trace_log( `Conversion start`).

    DATA: lt_source_files   TYPE zif_convertapi_client=>tty_files.
    DATA: lo_conversion     TYPE REF TO zif_convertapi_conversion.
    DATA: lv_request_body   TYPE string.
    DATA: lv_request_url    TYPE string.
    DATA: lv_response_body  TYPE string.
    DATA: ls_response       TYPE sty_convert_response_body.

    FIELD-SYMBOLS: <file>        LIKE LINE OF ls_response-files.
    FIELD-SYMBOLS: <source_file> LIKE LINE OF lt_source_files.

    lt_source_files = normalize_source( im_source = im_source ).

    lo_conversion = zcl_convertapi_conversion=>factory(
            io_client    = me
            i_conversion = im_parameters
        ).

    IF mv_service_side_storage = abap_true.
      LOOP AT lt_source_files ASSIGNING <source_file>.
        IF <source_file>->has_service_side_copy( ) = abap_false.
          <source_file>->upload(  ).
        ENDIF.
      ENDLOOP.

      lo_conversion->set_parameter(
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'true'
        iv_overwrite = abap_false ).

    ELSE.
      lo_conversion->set_parameter(
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'false'
        iv_overwrite = abap_false ).
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
    mo_http_client->request->set_method( method = c_request_method-post ).
    mo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    mo_http_client->request->set_cdata( data = lv_request_body ).

    add_authorization_credentials( mo_http_client->request ).

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
    DATA(xstr_req) = mo_http_client->request->get_raw_message(  ).
    DATA(xstr_res) = mo_http_client->response->get_raw_message(  ).

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
            convertapi_url = <file>-url
            content_base64 = <file>-file_data
        ) TO et_target_files.

      ENDLOOP.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.

      " TODO:  RAISE EXCEPTION TYPE zcx_convertapi_service

    ELSE.

    ENDIF.

  ENDMETHOD.

  METHOD delete.

    me->trace_log( `Deleting service copy of ` && io_file->name && `(` && io_file->id && `)`).

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = io_file->convertapi_url ).

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = c_request_method-delete ).

    add_authorization_credentials( mo_http_client->request ).

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

      CLEAR: io_file->id.
      CLEAR: io_file->convertapi_url.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      " TODO raise exception
    ELSE.
    ENDIF.


  ENDMETHOD.

  METHOD download.

    me->trace_log( `Downloading ` && io_file->name && `(` && io_file->id && `)`).

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = io_file->convertapi_url ).

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = c_request_method-get ).

    add_authorization_credentials( mo_http_client->request ).

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

        ev_file_content = mo_http_client->response->get_data( ).

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      " TODO raise exception
    ELSE.
    ENDIF.

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


  METHOD upload.

    DATA lv_file_name TYPE string.
    DATA lv_content_disposition_add TYPE string VALUE ''.

    IF io_file->id IS NOT INITIAL.
      RETURN.
    ENDIF.

    mo_http_client->request->set_version( version = mo_http_client->request->co_protocol_version_1_1 ).
    mo_http_client->request->set_method( method = c_request_method-post ).

    cl_http_utility=>set_request_uri(
      request = mo_http_client->request
      uri     = `/upload` ).


    IF io_file->url IS NOT INITIAL.

      mo_http_client->request->set_form_field(
        EXPORTING
          name  = 'url'
          value = io_file->url
      ).

      IF io_file->name IS NOT INITIAL.

        lv_file_name = io_file->name.

      ELSE.

        " TODO - attempt to extract filename from url
        lv_file_name = io_file->url.

        IF lv_file_name IS INITIAL AND io_file->ext IS NOT INITIAL.

          lv_file_name = 'file.' && io_file->ext.

        ENDIF.

      ENDIF.

      mo_http_client->request->set_form_field(
            EXPORTING
              name  = 'FileName'
              value = lv_file_name
          ).


    ELSE.

      " TODO: filename validation - exception or fix&go?

      lv_file_name = io_file->name.
      REPLACE ALL OCCURRENCES OF REGEX '[\x00-\x31\\/:"*?<>|]+' IN lv_file_name WITH 'X'. " striping filename invalid characters

      DATA(lv_file_name_safe) = lv_file_name.
      REPLACE ALL OCCURRENCES OF REGEX '[^\x32-\x7F]+' IN lv_file_name_safe WITH 'X'. " replacing non-ascii characters with X

      IF lv_file_name_safe <> io_file->name.
        lv_content_disposition_add = ` ;filename*=UTF-8''` && cl_http_utility=>if_http_utility~escape_url( io_file->name ).
      ENDIF.

      mo_http_client->request->set_data( data = io_file->mv_content_bin ).

      add_authorization_credentials( mo_http_client->request ).

      mo_http_client->request->set_header_field(
          name  = 'Content-Disposition'
          value = 'inline; filename="' && io_file->name && '"' && lv_content_disposition_add
      ).
    ENDIF.

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


    DATA response_body TYPE string.

    response_body = mo_http_client->response->get_cdata( ).

    DATA response TYPE sty_upload_response_body.

    IF response_status_code = 200.

      me->parse_json(
        EXPORTING
          im_json = response_body
        CHANGING
          ch_data = response
      ).

      io_file->id             = response-file_id.
      io_file->convertapi_url = response-url.
      io_file->size           = response-file_size.
      io_file->name           = response-file_name.
      io_file->ext            = response-file_ext.

    ELSEIF response_status_code >= 500 AND response_status_code < 599.
      RAISE EXCEPTION TYPE zcx_convertapi_service
*        EXPORTING
*          textid   =
*          previous =
        .
    ELSE.
    ENDIF.

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


  METHOD get_convert_request_body.
    DATA ls_body       TYPE sty_convert_request_body.
    DATA lo_conversion TYPE REF TO zcl_convertapi_conversion.
    DATA lt_parameters TYPE zif_convertapi_conversion=>tty_parameters.
    DATA lo_file       TYPE REF TO zcl_convertapi_file.

    FIELD-SYMBOLS:
      <conv_param> LIKE LINE OF lt_parameters,
      <body_param> LIKE LINE OF ls_body-parameters,
      <file>       LIKE LINE OF it_files,
      <file_value> LIKE LINE OF <body_param>-file_values.

    IF lines( it_files[] ) = 1.
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name = 'File'.
      ASSIGN <body_param>-file_value TO <file_value>.
      <file_value> = get_filevalue( it_files[ 1 ] ).

    ELSEIF lines( it_files[] ) > 1.
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name = 'Files'.
      me->trace_log(`Files:`).
      LOOP AT it_files[] ASSIGNING <file>.
        <file_value> = get_filevalue( <file> ).
      ENDLOOP.
    ENDIF.

    lt_parameters = io_conversion->get_parameters(  ).
    me->trace_log(`Param:`).
    LOOP AT lt_parameters[] ASSIGNING <conv_param>.
      me->trace_log( `   ` && <conv_param>-name && `: ` && <conv_param>-value ).
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name  = <conv_param>-name.
      <body_param>-value = <conv_param>-value.
    ENDLOOP.

    rv_json = to_json( ls_body ).

  ENDMETHOD.

  METHOD get_convert_request_url.
    DATA: lo_file TYPE REF TO zif_convertapi_file.

    READ TABLE it_files[] INDEX 1 INTO lo_file.
    rv_url = '/convert/' && lo_file->get_ext( ) && '/to/' && io_conversion->get_result_format( ).
  ENDMETHOD.

  METHOD to_json.

    rv_json = /ui2/cl_json=>serialize(
        data          = im_data
        compress      = /ui2/cl_json=>c_bool-true
        pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
    ).

  ENDMETHOD.

  METHOD add_authorization_credentials.

*    io_http_request->set_header_field(
*        name  = 'Authorization'
*        value = 'Basic ' && cl_http_utility=>if_http_utility~encode_base64( mv_api_key && ':' && mv_api_secret )
*    ).

    io_http_request->set_authorization(
      EXPORTING
        auth_type = ihttp_auth_type_basic_auth
        username  = mv_api_key    " User Name
        password  = mv_api_secret    " Password
    ).

  ENDMETHOD.

  METHOD trace_log.
    DATA lv_msg TYPE c.

    IF mv_log_handle IS NOT INITIAL.
        lv_msg = msg.

        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING
            i_log_handle        = mv_log_handle    " Log handle
            i_msgty             = 'I'    " Message type (A, E, W, I, S)
            i_probclass         = '4'    " Problem class (1, 2, 3, 4)
            i_text              = lv_msg    " Message data
*            i_s_context         =     " Context information for free text message
*            i_s_params          =     " Parameter set for free text message
*          IMPORTING
*            e_s_msg_handle      =     " Message handle
*            e_msg_was_logged    =     " Message collected
*            e_msg_was_displayed =     " Message output
*          EXCEPTIONS
*            log_not_found       = 1
*            msg_inconsistent    = 2
*            log_is_full         = 3
*            others              = 4
          .
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_filevalue.
    DATA lo_file TYPE REF TO zcl_convertapi_file.
    lo_file ?= io_file.
    rs_file_value-name = lo_file->name.
    IF lo_file->convertapi_url IS NOT INITIAL.     " pre-uploaded file
      rs_file_value-url   = lo_file->convertapi_url.
    ELSEIF lo_file->url IS NOT INITIAL.            " remote url file
      rs_file_value-url  = lo_file->url.
    ELSEIF lo_file->mv_content_bin IS NOT INITIAL. " local file

      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lo_file->mv_content_bin
        IMPORTING
          output = rs_file_value-data.

    ELSE.
      " TODO raise exception
    ENDIF.
  ENDMETHOD.

ENDCLASS.
