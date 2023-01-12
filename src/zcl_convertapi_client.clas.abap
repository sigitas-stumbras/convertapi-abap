CLASS zcl_convertapi_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

    GLOBAL FRIENDS
        zcl_convertapi_file
        zcl_convertapi_conversion.

  PUBLIC SECTION.

    INTERFACES zif_convertapi_client.

    CLASS-METHODS create
      IMPORTING
        !iv_api_secret   TYPE string
        !iv_api_key      TYPE string
        !io_http_client  TYPE REF TO if_http_client
        !iv_log_handle   TYPE balloghndl OPTIONAL
        !iv_storage_mode TYPE zif_convertapi_client=>ty_storage_mode DEFAULT zif_convertapi_client=>c_storage_mode-use_service_storage
        !iv_auto_cleanup TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_client) TYPE REF TO zif_convertapi_client.

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

    DATA: http_client          TYPE REF TO if_http_client .
    DATA: api_secret           TYPE string .
    DATA: api_key              TYPE string .
    DATA: storage_mode         TYPE zif_convertapi_client=>ty_storage_mode.
    DATA: auto_cleanup         TYPE abap_bool.

    DATA: uploaded_files       TYPE zif_convertapi_client=>tty_files.

    DATA: log_handle           TYPE balloghndl.

    CONSTANTS:
      BEGIN OF c_request_method,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        delete TYPE string VALUE 'DELETE',
      END OF c_request_method.

    CONSTANTS:
      BEGIN OF c_http_header,
        content_disposition TYPE string VALUE 'Content-Disposition' ##NO_TEXT,
        content_type        TYPE string VALUE 'Content-Type'  ##NO_TEXT,
      END OF c_http_header.

    CONSTANTS:
      BEGIN OF c_content_type,
        application_json TYPE string VALUE 'application/json',
      END OF c_content_type.

    METHODS upload
      IMPORTING
        io_file TYPE REF TO zcl_convertapi_file
      RAISING
        zcx_convertapi_exception.

    METHODS download
      IMPORTING
        !io_file         TYPE REF TO zcl_convertapi_file
      EXPORTING
        !ev_file_content TYPE xstring
      RAISING
        zcx_convertapi_exception.

    METHODS delete
      IMPORTING
        !io_file TYPE REF TO zcl_convertapi_file
      RAISING
        zcx_convertapi_exception.

    METHODS convert
      IMPORTING
        !i_source       TYPE any
        !io_parameters  TYPE REF TO zif_convertapi_conversion
      EXPORTING
        et_target_files TYPE zif_convertapi_client=>tty_files
      RAISING
        zcx_convertapi_exception.

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
        !i_source       TYPE any OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_convertapi_client=>tty_files
      RAISING
        zcx_convertapi_exception.

    METHODS get_convert_request_body
      IMPORTING
        it_files       TYPE zif_convertapi_client=>tty_files
        io_conversion  TYPE REF TO zif_convertapi_conversion
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS get_convert_request_url
      IMPORTING
        it_files      TYPE zif_convertapi_client=>tty_files
        io_conversion TYPE REF TO zif_convertapi_conversion
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_convertapi_exception.

    CLASS-METHODS to_json
      IMPORTING
        im_data        TYPE data
      RETURNING
        VALUE(rv_json) TYPE string.

    CLASS-METHODS get_filevalue
      IMPORTING
        io_file              TYPE REF TO zif_convertapi_file
      RETURNING
        VALUE(rs_file_value) TYPE zcl_convertapi_client=>sty_convert_request_filevalue.

    METHODS send_request
      RETURNING
        VALUE(rv_response_code) TYPE integer
      RAISING
        zcx_convertapi_exception.

    METHODS trace_log
      IMPORTING
        msg TYPE string.

ENDCLASS.

CLASS zcl_convertapi_client IMPLEMENTATION.

  METHOD create.

    DATA lo_client TYPE REF TO zcl_convertapi_client.

    lo_client = NEW zcl_convertapi_client( ).

    lo_client->api_secret           = iv_api_secret.
    lo_client->api_key              = iv_api_key.
    lo_client->http_client          = io_http_client.
    lo_client->storage_mode         = iv_storage_mode.
    lo_client->log_handle           = iv_log_handle.

    IF iv_storage_mode = zif_convertapi_client=>c_storage_mode-manual.
      lo_client->auto_cleanup = abap_false.
    ELSE.
      lo_client->auto_cleanup = abap_true.
    ENDIF.

    ro_client = lo_client.

  ENDMETHOD.

  METHOD zif_convertapi_client~create_conversion.

    ro_conversion = zcl_convertapi_conversion=>factory(
            io_client        = me
            i_conversion     = iv_target_format
            iv_source_format = iv_source_format
            it_parameters    = it_parameters
        ).

  ENDMETHOD.

  METHOD zif_convertapi_client~create_file.

    ro_file = zcl_convertapi_file=>factory(
        client  = me
        name    = iv_name
        content = iv_content
    ).

    IF me->storage_mode = abap_true.
      ro_file->upload(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_client~create_file_from_fs.

    DATA lv_filename TYPE string.
    DATA lv_content TYPE xstring.

    lv_filename = iv_filename.

    IF iv_filename IS INITIAL.
      lv_filename = lcl_fs=>get_filename( iv_path ).
    ENDIF.

    IF iv_filename IS INITIAL.
      zcx_convertapi_exception=>raise( 'Could not determine the file name' ).
    ENDIF.

    lv_content  = lcl_fs=>read_file( iv_path ).

    ro_file = zcl_convertapi_file=>factory(
        client  = me
        name    = lv_filename
        content = lv_content
    ).

    IF me->storage_mode = abap_true.
      ro_file->upload(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_client~create_file_from_url.

    ro_file = zcl_convertapi_file=>factory(
        client = me
        name   = iv_name
        url    = iv_url
    ).

    IF me->storage_mode = abap_true.
      ro_file->upload(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_client~get_auto_cleanup.
    rv_enabled = auto_cleanup.
  ENDMETHOD.

  METHOD zif_convertapi_client~set_auto_cleanup.
    auto_cleanup = iv_enabled.
  ENDMETHOD.

  METHOD zif_convertapi_client~cleanup.
    DATA lo_file TYPE REF TO zif_convertapi_file.

    LOOP AT uploaded_files[] INTO lo_file.
      lo_file->delete_service_side_copy(  ).
    ENDLOOP.

  ENDMETHOD.

  METHOD convert.

    me->trace_log( `Conversion start` )  ##NO_TEXT.

    DATA lt_source_files   TYPE zif_convertapi_client=>tty_files.
    DATA lo_file           TYPE REF TO zif_convertapi_file.
    DATA lo_conversion     TYPE REF TO zif_convertapi_conversion.
    DATA lv_request_body   TYPE string.
    DATA lv_request_url    TYPE string.
    DATA lv_response_body  TYPE string.
    DATA ls_response       TYPE sty_convert_response_body.
    DATA lv_http_response_code TYPE integer.

    FIELD-SYMBOLS: <file>        LIKE LINE OF ls_response-files.
    FIELD-SYMBOLS: <source_file> LIKE LINE OF lt_source_files.

    lt_source_files = normalize_source( i_source = i_source ).

    lo_conversion = zcl_convertapi_conversion=>factory(
            io_client    = me
            i_conversion = io_parameters
        ).

    IF storage_mode = zif_convertapi_client=>c_storage_mode-use_service_storage.

      LOOP AT lt_source_files ASSIGNING <source_file>.
        IF <source_file>->has_service_side_copy( ) = abap_false.
          <source_file>->upload(  ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage.

      lo_conversion->set_parameter(
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'false'
        iv_overwrite = abap_false ).

    ELSE.

      lo_conversion->set_parameter(
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'true'
        iv_overwrite = abap_false ).

    ENDIF.

    lv_request_body = get_convert_request_body(
            it_files = lt_source_files
            io_conversion = lo_conversion ).

    lv_request_url = get_convert_request_url(
            it_files      = lt_source_files
            io_conversion = lo_conversion ).

    cl_http_utility=>set_request_uri(
            request = http_client->request
            uri     = lv_request_url ).

    http_client->request->set_version( version = http_client->request->co_protocol_version_1_1 ).
    http_client->request->set_method( method = c_request_method-post ).

    http_client->request->set_header_field(
        name = c_http_header-content_type
        value = c_content_type-application_json
    ).

    http_client->request->set_cdata( data = lv_request_body ).

    add_authorization_credentials( http_client->request ).

    lv_http_response_code = me->send_request(  ).

    lv_response_body = http_client->response->get_cdata( ).

    IF lv_http_response_code = 200.

      me->parse_json(
        EXPORTING
          im_json = lv_response_body
        CHANGING
          ch_data = ls_response
      ).

      LOOP AT ls_response-files[] ASSIGNING <file>.

        APPEND zcl_convertapi_file=>factory(
            client         =  me
            convertapi_id  = <file>-file_id
            name           = <file>-file_name
            ext            = <file>-file_ext
            size           = <file>-file_size
            convertapi_url = <file>-url
            content_base64 = <file>-file_data
        ) TO et_target_files.

        IF auto_cleanup = abap_true.
          LOOP AT et_target_files INTO lo_file.
            lo_file->get_content( ).
            lo_file->delete_service_side_copy( ).
          ENDLOOP.
        ENDIF.

      ENDLOOP.

    ELSE.

      RAISE EXCEPTION TYPE zcx_convertapi_exception
        EXPORTING
          http_code = lv_http_response_code
          response  = lv_response_body.

    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA lv_response_body TYPE string.
    DATA lv_http_response_code TYPE integer.

    me->trace_log( `Deleting service copy of ` && io_file->name && `(` && io_file->convertapi_id && `)`)  ##NO_TEXT.

    cl_http_utility=>set_request_uri(
      request = http_client->request
      uri     = io_file->convertapi_url ).

    http_client->request->set_version( version = http_client->request->co_protocol_version_1_1 ).
    http_client->request->set_method( method = c_request_method-delete ).

    add_authorization_credentials( http_client->request ).

    lv_http_response_code = me->send_request(  ).

    IF lv_http_response_code = 200.

      CLEAR: io_file->convertapi_id.
      CLEAR: io_file->convertapi_url.

      DELETE uploaded_files[] WHERE table_line = io_file.

    ELSE.
      lv_response_body = http_client->response->get_cdata( ).

      RAISE EXCEPTION TYPE zcx_convertapi_exception
        EXPORTING
          http_code = lv_http_response_code
          response  = lv_response_body.
    ENDIF.


  ENDMETHOD.

  METHOD download.

    DATA lv_response_body TYPE string.
    DATA lv_http_response_code TYPE integer.

    me->trace_log( `Downloading ` && io_file->name && `(` && io_file->convertapi_id && `)`)  ##NO_TEXT.

    cl_http_utility=>set_request_uri(
      request = http_client->request
      uri     = io_file->convertapi_url ).

    http_client->request->set_version( version = http_client->request->co_protocol_version_1_1 ).
    http_client->request->set_method( method = c_request_method-get ).

    add_authorization_credentials( http_client->request ).

    lv_http_response_code = me->send_request(  ).

    IF lv_http_response_code = 200.

      ev_file_content = http_client->response->get_data( ).

    ELSE.

      lv_response_body = http_client->response->get_cdata( ).

      RAISE EXCEPTION TYPE zcx_convertapi_exception
        EXPORTING
          http_code = lv_http_response_code
          response  = lv_response_body.

    ENDIF.

  ENDMETHOD.

  METHOD upload.

    DATA lv_file_name TYPE string.
    DATA lv_file_name_safe TYPE string.
    DATA lv_content_disposition_add TYPE string VALUE ''.
    DATA lv_response_body TYPE string.
    DATA ls_response TYPE sty_upload_response_body.
    DATA lv_http_response_code TYPE integer.

    IF io_file->convertapi_id IS NOT INITIAL.
      RETURN.
    ENDIF.

    http_client->request->set_version( version = http_client->request->co_protocol_version_1_1 ).
    http_client->request->set_method( method = c_request_method-post ).

    cl_http_utility=>set_request_uri(
      request = http_client->request
      uri     = `/upload` ).

    IF io_file->url IS NOT INITIAL.

      http_client->request->set_form_field(
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

      http_client->request->set_form_field(
          name  = 'FileName'
          value = lv_file_name
      ).

    ELSE.

      " TODO: filename validation - exception or fix&go?

      lv_file_name = io_file->name.
      REPLACE ALL OCCURRENCES OF REGEX '[\x00-\x31\\/:"*?<>|]+' IN lv_file_name WITH 'X'. " striping filename invalid characters

      lv_file_name_safe = lv_file_name.
      REPLACE ALL OCCURRENCES OF REGEX '[^\x32-\x7F]+' IN lv_file_name_safe WITH 'X'  ##NO_TEXT. " replacing non-ascii characters with X

      IF lv_file_name_safe <> io_file->name.
        lv_content_disposition_add = ` ;filename*=UTF-8''` && cl_http_utility=>if_http_utility~escape_url( io_file->name )  ##NO_TEXT.
      ENDIF.

      http_client->request->set_data( data = io_file->content_bin ).
      add_authorization_credentials( http_client->request ).

      http_client->request->set_header_field(
          name  = c_http_header-content_disposition
          value = 'inline; filename="' && io_file->name && '"' && lv_content_disposition_add   ##NO_TEXT
      ).

    ENDIF.

    lv_http_response_code = me->send_request(  ).

    IF lv_http_response_code = 200.

      lv_response_body = http_client->response->get_cdata( ).

      me->parse_json(
        EXPORTING
          im_json = lv_response_body
        CHANGING
          ch_data = ls_response
      ).

      io_file->convertapi_id  = ls_response-file_id.
      io_file->convertapi_url = ls_response-url.
      io_file->size           = ls_response-file_size.
      io_file->name           = ls_response-file_name.
      io_file->ext            = ls_response-file_ext.

      APPEND io_file TO uploaded_files[].

    ELSE.
      RAISE EXCEPTION TYPE zcx_convertapi_exception
        EXPORTING
          http_code = lv_http_response_code
          response  = lv_response_body.
    ENDIF.

  ENDMETHOD.

  METHOD parse_json.

    /ui2/cl_json=>deserialize(
       EXPORTING
          json             = im_json
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
       CHANGING
         data             = ch_data
     ).

  ENDMETHOD.

  METHOD normalize_source.

    DATA: lo_typedescr  TYPE REF TO cl_abap_typedescr,
          lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lo_classdescr TYPE REF TO cl_abap_classdescr,
          lo_line_type  TYPE REF TO cl_abap_refdescr.

    FIELD-SYMBOLS:
        <lt_files> TYPE zif_convertapi_client=>tty_files.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( i_source ).

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        lo_classdescr ?= cl_abap_objectdescr=>describe_by_object_ref( i_source ).
        lo_classdescr->get_interface_type(
          EXPORTING
            p_name              = 'ZIF_CONVERTAPI_FILE' " Name of interface
          EXCEPTIONS
            interface_not_found = 1
            OTHERS              = 2
        ).
        IF sy-subrc <> 0.
          zcx_convertapi_exception=>raise( 'Unsupported object type for source parameter' ).
        ENDIF.
        APPEND i_source TO rt_files[].
      WHEN cl_abap_typedescr=>typekind_table.
        lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( i_source ).
        lo_line_type ?= lo_tabledescr->get_table_line_type( ).
        CASE lo_line_type->type_kind.
          WHEN cl_abap_typedescr=>typekind_oref.
            ASSIGN i_source TO <lt_files>.
            APPEND LINES OF <lt_files> TO rt_files[].
            UNASSIGN <lt_files>.
          WHEN OTHERS.
            zcx_convertapi_exception=>raise( 'Unsupported table type for source parameter' ).
        ENDCASE.

      WHEN OTHERS.
        zcx_convertapi_exception=>raise( 'Unsupported data type for source parameter' ).
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
      <body_param>-name = zif_convertapi_client=>c_param-file.
      ASSIGN <body_param>-file_value TO <file_value>.
      <file_value> = get_filevalue( it_files[ 1 ] ).

    ELSEIF lines( it_files[] ) > 1.
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name = zif_convertapi_client=>c_param-files.
      me->trace_log(`Files:`) ##NO_TEXT.
      LOOP AT it_files[] ASSIGNING <file>.
        APPEND INITIAL LINE TO <body_param>-file_values ASSIGNING <file_value>.
        <file_value> = get_filevalue( <file> ).
      ENDLOOP.
    ENDIF.

    lt_parameters = io_conversion->get_parameters(  ).
    me->trace_log(`Param:`) ##NO_TEXT.
    LOOP AT lt_parameters[] ASSIGNING <conv_param>.
      me->trace_log( `   ` && <conv_param>-name && `: ` && <conv_param>-value ).
      APPEND INITIAL LINE TO ls_body-parameters ASSIGNING <body_param>.
      <body_param>-name  = <conv_param>-name.
      <body_param>-value = <conv_param>-value.
    ENDLOOP.

    rv_json = to_json( ls_body ).

  ENDMETHOD.

  METHOD get_convert_request_url.
    DATA lo_file TYPE REF TO zif_convertapi_file.
    rv_url = '/convert/' && io_conversion->get_source_format( it_files ) && '/to/' && io_conversion->get_result_format( ).
  ENDMETHOD.

  METHOD to_json.

    rv_json = /ui2/cl_json=>serialize(
        data          = im_data
        compress      = /ui2/cl_json=>c_bool-true
        pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
    ).

  ENDMETHOD.

  METHOD add_authorization_credentials.

    io_http_request->set_authorization(
        auth_type = ihttp_auth_type_basic_auth
        username  = api_key
        password  = api_secret
    ).

  ENDMETHOD.

  METHOD trace_log.
    DATA lv_msg TYPE c.

    IF log_handle IS NOT INITIAL.
      lv_msg = msg.

      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle     = log_handle    " Log handle
          i_msgty          = 'I'    " Message type (A, E, W, I, S)
          i_probclass      = '4'    " Problem class (1, 2, 3, 4)
          i_text           = lv_msg    " Message data
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
    ELSEIF lo_file->content_bin IS NOT INITIAL. " local file

      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lo_file->content_bin
        IMPORTING
          output = rs_file_value-data.

    ELSE.
      " TODO raise exception
    ENDIF.
  ENDMETHOD.

  METHOD send_request.

    DATA lv_error_msg TYPE string.
    DATA lv_exception TYPE string.

    http_client->send(
*      EXPORTING
*        timeout                    = CO_TIMEOUT_DEFAULT    " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    IF sy-subrc <> 0.

      CASE sy-subrc.
        WHEN 1. lv_exception = 'http_communication_failure'.
        WHEN 2. lv_exception = 'http_invalid_state'.
        WHEN 3. lv_exception = 'http_processing_failed'.
        WHEN 4. lv_exception =  'http_invalid_timeout'.
        WHEN OTHERS. lv_exception = 'others'.
      ENDCASE.

      http_client->get_last_error( IMPORTING message = lv_error_msg ).

      zcx_convertapi_exception=>raise_classic(
          method                   = 'if_http_client->send'
          exception                = lv_exception
          message                  = lv_error_msg
      ).

    ENDIF.

    http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 99
    ).

    IF sy-subrc <> 0.

      CASE sy-subrc.
        WHEN 1. lv_exception = 'http_communication_failure'.
        WHEN 2. lv_exception = 'http_invalid_state'.
        WHEN 3. lv_exception = 'http_processing_failed'.
        WHEN OTHERS. lv_exception = 'others'.
      ENDCASE.

      http_client->get_last_error( IMPORTING message = lv_error_msg ).

      zcx_convertapi_exception=>raise_classic(
          method                   = 'if_http_client->receive'
          exception                = lv_exception
          message                  = lv_error_msg
      ).

    ENDIF.

    http_client->response->get_status(
      IMPORTING
         code   = rv_response_code
      ).

  ENDMETHOD.

ENDCLASS.
