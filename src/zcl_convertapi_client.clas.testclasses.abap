CLASS ltc_client_test DEFINITION DEFERRED.
CLASS zcl_convertapi_client DEFINITION LOCAL FRIENDS ltc_client_test.

CLASS ltc_client_test DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PUBLIC SECTION.
    METHODS: conversion_parameters_test     FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: upload_download_delete         FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: covert_w_remote_storing        FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: covert_no_remote_storing       FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: multi_input_w_remote_storing   FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: multi_input_no_remote_storing  FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: multi_output_w_remote_storing  FOR TESTING RAISING zcx_convertapi_exception.
    METHODS: multi_output_no_remote_storing FOR TESTING RAISING zcx_convertapi_exception.


  PRIVATE SECTION.
    CLASS-DATA: m_api_key      TYPE string.
    CLASS-DATA: m_api_secret   TYPE string.
    CLASS-DATA: http_client TYPE REF TO if_http_client.
    CLASS-DATA: m_log_handle   TYPE balloghndl.

    CONSTANTS c_pixel_gif   TYPE xstring VALUE '47494638376101000100800100000000FFFFFF2C00000000010001000002024C01003B'.                   " 1 white pixel GIF image
    CONSTANTS c_pixel_webp  TYPE xstring VALUE '52494646240000005745425056503820180000003001009D012A0100010002003425A400037000FEFB940000'. " 1 white pixel WEBP image
    CONSTANTS c_webp_header TYPE xstring VALUE '52494646'.                                                                                 " WEBP image header
    CONSTANTS c_zip_header  TYPE xstring VALUE '504B0304'.                                                                                 " ZIP file header

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.

    METHODS: setup.
    METHODS: teardown.

    METHODS: direct_download
      IMPORTING
        io_convertapi_client  TYPE REF TO zcl_convertapi_client
        iv_url                TYPE string
      RETURNING
        VALUE(rv_status_code) TYPE integer.

    METHODS: covert_1px_gif_to_webp_priv
      IMPORTING
        io_client     TYPE REF TO zif_convertapi_client
        iv_msg_prefix TYPE string
      RAISING
        zcx_convertapi_exception.

    METHODS: covert_1_to_n
      IMPORTING
        io_client     TYPE REF TO zif_convertapi_client
        iv_msg_prefix TYPE string
      RAISING
        zcx_convertapi_exception.

    METHODS: covert_2xgif_to_zip
      IMPORTING
        io_client     TYPE REF TO zif_convertapi_client
        iv_msg_prefix TYPE string
      RAISING
        zcx_convertapi_exception.

ENDCLASS.                    "ltc_client_test DEFINITION


CLASS ltc_client_test IMPLEMENTATION.
  METHOD class_setup.

    DATA: ls_log TYPE bal_s_log.
    "----------------------------------------------------------------------------------------------
    m_api_key    = '947226456'.
    m_api_secret = '5EI6bEJ7TKH4J4PQ'.
    "----------------------------------------------------------------------------------------------

    cl_http_client=>create_by_url(
      EXPORTING
        url               =  'https://v2.convertapi.com:443'
        ssl_id            = 'ANONYM'
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE 'Could not create HTTP client - RC=' && sy-subrc TYPE 'E'.
    ENDIF.

    ls_log-object    = 'ZCONVERTAPI'.
    ls_log-subobject = 'UNITTEST'.
    ls_log-aluser    = sy-uname.
    ls_log-alprog    = sy-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = m_log_handle    " Log handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD class_teardown.
    DATA: lt_log_handle TYPE bal_t_logh.

    APPEND m_log_handle TO lt_log_handle.

    http_client->close( ).
    IF m_log_handle IS NOT INITIAL.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
*         i_client         = SY-MANDT    " Client in which the new log is to be saved
*         i_in_update_task = SPACE    " Save in UPDATE TASK
          i_save_all       = 'X'    " Save all logs in memory
          i_t_log_handle   = lt_log_handle    " Table of log handles
*         i_2th_connection = SPACE    " FALSE: No secondary connection
*         i_2th_connect_commit = SPACE    " FALSE: No COMMIT in module
*         i_link2job       = 'X'    " Boolean Variable (X=true, -=false, space=unknown)
*       IMPORTING
*         e_new_lognumbers =     " Table of new log numbers
*         e_second_connection  =     " Name of Secondary Connection
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD setup.
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD conversion_parameters_test.
    DATA lo_client TYPE REF TO zif_convertapi_client.
    DATA lo_conversion TYPE REF TO zif_convertapi_conversion.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage
        iv_log_handle = m_log_handle
      ).

    lo_conversion = lo_client->create_conversion(
      iv_target_format = 'pdf'
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_conversion->get_result_format(  )
        exp                  = 'pdf'
        msg                  = 'Conversion target format has unexpected value'
    ).

    lo_conversion->set_parameter(
      EXPORTING
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'true'
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_conversion->get_parameter( zif_convertapi_client=>c_param-store_file )
        exp                  = 'true'
        msg                  = 'Conversion - set/get single parameter fail'
    ).

    lo_conversion->set_parameter(
      EXPORTING
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'false'
        iv_overwrite = abap_false
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_conversion->get_parameter( zif_convertapi_client=>c_param-store_file )
        exp                  = 'true'
        msg                  = 'Conversion - set single parameter no-overwriting fail'
    ).

    lo_conversion->set_parameter(
      EXPORTING
        iv_name      = zif_convertapi_client=>c_param-store_file
        iv_value     = 'false'
        iv_overwrite = abap_true
    ).

    cl_abap_unit_assert=>assert_equals(
       EXPORTING
         act                  = lo_conversion->get_parameter( zif_convertapi_client=>c_param-store_file )
         exp                  = 'false'
         msg                  = 'Conversion - set single parameter overwriting fail'
     ).

    lo_conversion->clear_parameter( zif_convertapi_client=>c_param-store_file ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_conversion->get_parameter( zif_convertapi_client=>c_param-store_file )
        exp                  = ''
        msg                  = 'Conversion clear single parameter fail'
    ).

    lo_conversion->set_parameters(
      EXPORTING
        it_parameters = VALUE #(
            (  name = zif_convertapi_client=>c_param-store_file value = 'true' )
            (  name = zif_convertapi_client=>c_param-timeout    value = '1' )
         )
    ).

    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE zif_convertapi_conversion=>sty_parameter( name = zif_convertapi_client=>c_param-store_file value = 'true'  )
        table            = lo_conversion->get_parameters( )
        msg              = 'Conversion - set/get multiple parameters fail'
    ).

    lo_conversion->set_parameters(
       EXPORTING
         it_parameters = VALUE #(
             ( name = zif_convertapi_client=>c_param-store_file value = 'false' )
           )
         iv_overwrite = abap_false
     ).

    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE zif_convertapi_conversion=>sty_parameter( name = zif_convertapi_client=>c_param-store_file value = 'true'  )
        table            = lo_conversion->get_parameters( )
        msg              = 'Conversion - set multiple parameters no-overwriting fail'
    ).

    lo_conversion->set_parameters(
       EXPORTING
         it_parameters = VALUE #(
             (  name = zif_convertapi_client=>c_param-timeout    value = '2' )
           )
         iv_overwrite = abap_true
     ).

    cl_abap_unit_assert=>assert_table_contains(
      EXPORTING
        line             = VALUE zif_convertapi_conversion=>sty_parameter(  name = zif_convertapi_client=>c_param-timeout    value = '2' )
        table            = lo_conversion->get_parameters( )
        msg              =  'Conversion - set multiple parameters overwriting fail'
    ).

  ENDMETHOD.

  METHOD upload_download_delete.

    DATA lo_client TYPE REF TO zif_convertapi_client.
    DATA lv_file_id TYPE zif_convertapi_client=>ty_file_id.
    DATA lv_file_name TYPE string.
    DATA lv_file_ext TYPE string.
    DATA lv_downloaded_content TYPE xstring.
    DATA lv_downloaded_content_2 TYPE xstring.
    DATA lv_downloaded_content_3 TYPE xstring.
    DATA lo_file TYPE REF TO zif_convertapi_file.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage
        iv_log_handle = m_log_handle
      ).

    lo_file ?= lo_client->create_file(
        iv_name    = 'píẍẽl.gif'
        iv_content = c_pixel_gif
    ).

    DATA(lo_file_2) = lo_client->create_file(
        iv_name    = 'píxel.gif'
        iv_content = c_pixel_gif
    ).

    DATA(lo_file_3) = lo_client->create_file_from_url(
       iv_url  = 'https://www.convertapi.com/static/img/logo.svg'
       iv_name = 'logo.svg'
    ).

    lo_file->upload(  ).

    lo_file_3->upload(  ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
         act = lo_file->get_convertapi_url(  )
         msg = 'Should return file url after create upload'
    ).

    lv_downloaded_content = lo_file->get_content( ).

    cl_abap_unit_assert=>assert_equals(
        act = c_pixel_gif
        exp = lv_downloaded_content
        msg = 'Downloaded content not equal to uploaded'
    ).

    lv_downloaded_content_2 = lo_file_2->get_content( ).

    lv_downloaded_content_3 = lo_file_3->get_content( ).

    DATA lv_url TYPE string.
    DATA lv_status_code TYPE integer.
    DATA lo_client_inst TYPE REF TO zcl_convertapi_client.

    lv_url = lo_file->get_convertapi_url(  ).

    lo_file->delete_service_side_copy( ).

    lo_client_inst ?= lo_client.
    lv_status_code = direct_download( io_convertapi_client = lo_client_inst iv_url = lv_url ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_status_code
        exp = 404
        msg = 'Get did not return 404 after file deletion'
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD covert_w_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-use_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_1px_gif_to_webp_priv(
        io_client     = lo_client
        iv_msg_prefix = `REMOTE:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD covert_no_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_1px_gif_to_webp_priv(
        io_client     = lo_client
        iv_msg_prefix = `LOCAL:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD multi_input_no_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_2xgif_to_zip(
        io_client     = lo_client
        iv_msg_prefix = `REMOTE:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD multi_input_w_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-use_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_2xgif_to_zip(
        io_client     = lo_client
        iv_msg_prefix = `REMOTE:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD multi_output_no_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-no_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_1_to_n(
        io_client     = lo_client
        iv_msg_prefix = `REMOTE:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD multi_output_w_remote_storing.

    DATA lo_client TYPE REF TO zif_convertapi_client.

    lo_client = zcl_convertapi_client=>create(
        iv_api_key = m_api_key
        iv_api_secret = m_api_secret
        io_http_client = http_client
        iv_storage_mode = zif_convertapi_client=>c_storage_mode-use_service_storage
        iv_log_handle = m_log_handle
      ).

    covert_1_to_n(
        io_client     = lo_client
        iv_msg_prefix = `REMOTE:`
    ).

    lo_client->cleanup(  ).

  ENDMETHOD.

  METHOD covert_1px_gif_to_webp_priv.

    DATA lo_input_file    TYPE REF TO zif_convertapi_file.
    DATA lo_result_file   TYPE REF TO zif_convertapi_file.
    DATA lv_result        TYPE xstring.
    DATA lv_result_header TYPE xstring.
    DATA lo_convertapi_request_error TYPE REF TO zcx_convertapi_exception.

    lo_input_file = io_client->create_file(
        iv_name    = 'pixel.gif'
        iv_content = c_pixel_gif
    ).

    lo_result_file = lo_input_file->convert_to( 'webp' ).

    cl_abap_unit_assert=>assert_bound(
        act = lo_result_file
        msg = iv_msg_prefix && 'file->convert_to did not return file object'
    ).

    lv_result = lo_result_file->get_content(  ).

    cl_abap_unit_assert=>assert_not_initial(
        act = lv_result
        msg = iv_msg_prefix && '1px GIF->WEBP: empty result'
    ).

    lv_result_header = lv_result+0(4).

    cl_abap_unit_assert=>assert_equals(
        act = lv_result_header
        exp = c_webp_header
        msg = iv_msg_prefix && '1px GIF->WEBP: result unrecognized as WEBP'
        ).

  ENDMETHOD.

  METHOD covert_2xgif_to_zip.

    DATA lo_conversion TYPE REF TO zif_convertapi_conversion.
    DATA lt_source     TYPE zif_convertapi_client=>tty_files.
    DATA lt_result     TYPE zif_convertapi_client=>tty_files.

    APPEND io_client->create_file(
        iv_name    = 'pixel1.gif'
        iv_content = c_pixel_gif
    ) TO lt_source.

    APPEND io_client->create_file(
        iv_name    = 'pixel2.gif'
        iv_content = c_pixel_gif
    ) TO lt_source.

    lo_conversion = io_client->create_conversion(
        iv_source_format = 'any'
        iv_target_format = 'zip'
    ).

    lt_result = lo_conversion->convert( i_source = lt_source ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = lt_result[]
        msg              = iv_msg_prefix && '2xGIF->ZIP: empty result'
    ).

    cl_abap_unit_assert=>assert_equals(
        act = lt_result[ 1 ]->get_content(  )
        exp = c_zip_header
        msg = iv_msg_prefix && '2xGIF->ZIP: result unrecognized as ZIP'
    ).

  ENDMETHOD.

  METHOD covert_1_to_n.


  ENDMETHOD.

  METHOD direct_download.

    cl_http_utility=>set_request_uri(
      request = io_convertapi_client->http_client->request
      uri     = iv_url ).

    io_convertapi_client->http_client->request->set_version( version = http_client->request->co_protocol_version_1_1 ).
    io_convertapi_client->http_client->request->set_method( method = zcl_convertapi_client=>c_request_method-get ).
    io_convertapi_client->add_authorization_credentials( io_convertapi_client->http_client->request ).

    io_convertapi_client->http_client->send( EXCEPTIONS OTHERS = 4 ).
    CHECK sy-subrc = 0.

    io_convertapi_client->http_client->receive( EXCEPTIONS OTHERS = 4 ).
    CHECK sy-subrc = 0.

    io_convertapi_client->http_client->response->get_status( IMPORTING code = rv_status_code ).
  ENDMETHOD.


ENDCLASS.                    "ltc_client_test IMPLEMENTATION
