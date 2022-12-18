CLASS lcl_client_test DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PUBLIC SECTION.
    METHODS: upload_download_delete FOR TESTING.
    METHODS: covert_1px_gif_to_webp FOR TESTING.

  PRIVATE SECTION.
    CLASS-DATA: m_api_key      TYPE string.
    CLASS-DATA: m_api_secret   TYPE string.
    CLASS-DATA: mo_http_client TYPE REF TO IF_HTTP_CLIENT.

    CONSTANTS c_pixel_gif   TYPE xstring VALUE '47494638376101000100800100000000FFFFFF2C00000000010001000002024C01003B'.                   " 1 white pixel GIF image
    CONSTANTS c_pixel_webp  TYPE xstring VALUE '52494646240000005745425056503820180000003001009D012A0100010002003425A400037000FEFB940000'. " 1 white pixel WEBP image
    CONSTANTS c_webp_header TYPE xstring VALUE '52494646'.                                                                                 " WEBP image header

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.

    METHODS: setup.
    METHODS: teardown.

ENDCLASS.                    "lcl_client_test DEFINITION


CLASS lcl_client_test IMPLEMENTATION.
  METHOD class_setup.
"----------------------------------------------------------------------------------------------
    m_api_key    = '947226456'.
    m_api_secret = '5EI6bEJ7TKH4J4PQ'.
"----------------------------------------------------------------------------------------------

    cl_http_client=>create_by_url(
      EXPORTING
        url               =  'https://v2.convertapi.com:443'
        ssl_id            = 'ANONYM'
      IMPORTING
        client             = mo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        others             = 4
    ).
    IF sy-subrc <> 0.
     MESSAGE 'Could not create HTTP client - RC=' && sy-subrc TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD class_teardown.
    mo_http_client->close( ).
  ENDMETHOD.

  METHOD setup.
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD upload_download_delete.

    DATA lo_client TYPE REF TO zcl_convertapi_client.
    DATA lv_file_id TYPE zcl_convertapi_client=>ty_file_id.
    DATA lv_file_name TYPE string.
    DATA lv_file_ext TYPE string.
    DATA lv_downloaded_content TYPE xstring.

    lo_client = NEW zcl_convertapi_client(
        im_api_key = m_api_key
        im_api_secret = m_api_secret
        im_http_client = mo_http_client
        im_service_side_storage = abap_true
      ).

    DATA(lo_file) = lo_client->upload(
        im_file_name    = 'píẍẽl.gif'
        im_file_content = c_pixel_gif
    ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
         act = lo_file->get_id( )
         msg = 'Should return file id after upload'
    ).

    lv_downloaded_content = lo_file->get_content( ).

    cl_abap_unit_assert=>assert_equals(
        act = c_pixel_gif
        exp = lv_downloaded_content
        msg = 'Downloaded content not equal to uploaded'
    ).

    lo_file->delete( ).

    TRY.
      lv_downloaded_content = lo_file->get_content( ).
      cl_abap_unit_assert=>fail(
          msg    = 'Should not be able to download deleted file'
      ).
    CATCH zcx_convertapi_service.
    CATCH zcx_convertapi_client.
    ENDTRY.

  ENDMETHOD.

  METHOD covert_1px_gif_to_webp.

    DATA lo_client TYPE REF TO zcl_convertapi_client.

    DATA lv_result        TYPE xstring.
    DATA lv_result_header TYPE xstring.

    lo_client = NEW zcl_convertapi_client(
        im_api_secret = m_api_secret
        im_api_key    = m_api_key
        im_http_client = mo_http_client
      ).


    DATA(lo_file) = lo_client->upload(
        im_file_name    = 'pixel.gif'
        im_file_content = c_pixel_gif
    ).

    DATA(lo_to_webp_params) = NEW zcl_convertapi_conversion( 'webp' ).

*    DATA(lo_to_webp_params) = NEW zcl_convertapi_conversion(
*        im_result_format = 'webp'
*        im_parameters = VALUE #(
*            ( name = 'ColorSpace'         value = 'gray' )
*            ( name = 'ImageQuality '      value = '100' )
*            ( name = 'ImageInterpolation' value = 'true' )
*          )
*        ).
*
*    DATA(lo_converted) = lo_file->convert_to( lo_to_webp_params ).
*
    lo_client->convert(
        EXPORTING
          im_source = lo_file
          im_parameters = lo_to_webp_params
*        IMPORTING
*          ex_result_file =
*          ex_result_files =
     ).


*
*    lo_client->convert(
*      EXPORTING
*        im_source_format = 'gif'
*        im_result_format = 'webp'
*        im_source_content = c_pixel_gif
*      IMPORTING
*        ex_result_content = lv_result
*    ).
*
*    cl_abap_unit_assert=>assert_not_initial(
*        act = lv_result
*        msg = '1px GIF->WEBP: Convertion result empty'
*    ).
*
*    lv_result_header = lv_result+0(4).
*
*    cl_abap_unit_assert=>assert_equals(
*        act = lv_result_header
*        exp = c_webp_header
*        msg = '1px GIF->WEBP: Convertion result unrecognized'
*    ).

  ENDMETHOD.

ENDCLASS.                    "lcl_client_test IMPLEMENTATION
