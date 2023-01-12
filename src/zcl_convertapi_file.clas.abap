CLASS zcl_convertapi_file DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_convertapi_client.

  PUBLIC SECTION.

    INTERFACES zif_convertapi_file.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA client                 TYPE REF TO zcl_convertapi_client.
    DATA has_service_side_copy  TYPE abap_bool.
    DATA content_bin            TYPE xstring.

    DATA convertapi_id              TYPE string.
    DATA name            TYPE string.
    DATA ext             TYPE string.
    DATA url             TYPE string.
    DATA convertapi_url  TYPE string.
    DATA size            TYPE integer VALUE -1.

    CLASS-METHODS factory
      IMPORTING
        !client         TYPE REF TO zcl_convertapi_client
        !name           TYPE string OPTIONAL
        !ext            TYPE string OPTIONAL
        !size           TYPE integer DEFAULT -1
        !url            TYPE string OPTIONAL
        !convertapi_id  TYPE string OPTIONAL
        !convertapi_url TYPE string OPTIONAL
        !content        TYPE xstring OPTIONAL
        !content_base64 TYPE string OPTIONAL
      RETURNING
        VALUE(ro_file)  TYPE REF TO zif_convertapi_file.

ENDCLASS.


CLASS zcl_convertapi_file IMPLEMENTATION.

  METHOD factory.
    DATA: lo_file TYPE REF TO zcl_convertapi_file.

    IF convertapi_id IS INITIAL AND name IS INITIAL AND ext IS INITIAL.

    ENDIF.

    lo_file =  NEW zcl_convertapi_file( ).

    lo_file->client                = client.

    IF content IS NOT INITIAL.
      lo_file->content_bin           = content.
    ELSEIF content_base64 IS NOT INITIAL.

      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = content_base64
        IMPORTING
          output = lo_file->content_bin
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      IF sy-subrc <> 0.
        " TODO raise exception
      ENDIF.

    ENDIF.

    lo_file->convertapi_id   = convertapi_id.
    lo_file->name = name.
    lo_file->ext  = ext.
    lo_file->url  = url.
    lo_file->size = xstrlen( lo_file->content_bin ) .
    lo_file->convertapi_url  = convertapi_url.

    ro_file = lo_file.

  ENDMETHOD.

  METHOD zif_convertapi_file~delete_service_side_copy.
    client->delete( io_file = me ).
    ro_same_file = me.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_content.

    IF content_bin IS INITIAL AND convertapi_id IS NOT INITIAL.

      me->client->download(
        EXPORTING
          io_file         = me
        IMPORTING
          ev_file_content = content_bin
      ).

    ENDIF.
    rv_content = content_bin.

  ENDMETHOD.

  METHOD zif_convertapi_file~get_ext.
    DATA lt_name_components TYPE TABLE OF string.
    IF me->ext IS NOT INITIAL.
      rv_ext = me->ext.
    ELSEIF me->name IS NOT INITIAL.
      SPLIT me->name AT '.' INTO TABLE lt_name_components.
      IF sy-subrc = 0.
        rv_ext = lt_name_components[ lines( lt_name_components )  ].
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_id.
    rv_id = me->convertapi_id.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_name.
    rv_name = me->name.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_external_url.
    rv_url = me->url.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_convertapi_url.
    rv_url = me->convertapi_url.
  ENDMETHOD.

  METHOD zif_convertapi_file~convert_to.

    DATA: lo_conversion TYPE REF TO zif_convertapi_conversion.
    DATA: lt_result_files TYPE zif_convertapi_client=>tty_files.

    lo_conversion = client->zif_convertapi_client~create_conversion(
            iv_target_format = i_conversion
        ).

    client->convert(
      EXPORTING
        i_source       =  me
        io_parameters   =  lo_conversion
      IMPORTING
        et_target_files = lt_result_files
    ).

    CASE lines( lt_result_files[] ).
    WHEN 0.
        zcx_convertapi_exception=>raise( 'Conversion did not return any files' ).
    WHEN 1.
      ro_new_file = lt_result_files[ 1 ].
    WHEN OTHERS.
      zcx_convertapi_exception=>raise( 'Conversion returned more than one file' ).
    ENDCASE.

  ENDMETHOD.

  METHOD zif_convertapi_file~has_service_side_copy.
    IF me->convertapi_id IS INITIAL.
      rv_result = abap_false.
    ELSE.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD zif_convertapi_file~upload.
    client->upload( io_file = me ).
    ro_same_file = me.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_size.
    rv_size = me->size.
  ENDMETHOD.

ENDCLASS.
