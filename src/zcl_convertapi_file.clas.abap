CLASS zcl_convertapi_file DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_convertapi_client.

  PUBLIC SECTION.

    INTERFACES zif_convertapi_file.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_client                 TYPE REF TO zcl_convertapi_client.
    DATA mv_has_client_side_copy   TYPE abap_bool.
    DATA mv_has_service_side_copy  TYPE abap_bool.
    DATA mv_content_bin            TYPE xstring.

    DATA id              TYPE string.
    DATA name            TYPE string.
    DATA ext             TYPE string.
    DATA url             TYPE string.
    DATA convertapi_url  TYPE string.
    DATA size            TYPE integer VALUE -1.

    CLASS-METHODS factory
      IMPORTING
        !client         TYPE REF TO zcl_convertapi_client
        !id             TYPE string OPTIONAL
        !name           TYPE string OPTIONAL
        !ext            TYPE string OPTIONAL
        !size           TYPE integer DEFAULT -1
        !url            TYPE string OPTIONAL
        !convertapi_url TYPE string OPTIONAL
        !content        TYPE xstring OPTIONAL
        !content_base64 TYPE string OPTIONAL
      RETURNING
        VALUE(ro_file)  TYPE REF TO zif_convertapi_file.

    METHODS recalc_state.

ENDCLASS.


CLASS zcl_convertapi_file IMPLEMENTATION.

  METHOD factory.
    DATA: lo_file TYPE REF TO zcl_convertapi_file.

    IF id IS INITIAL AND name IS INITIAL AND ext IS INITIAL.

    ENDIF.

    lo_file =  NEW zcl_convertapi_file( ).

    lo_file->mo_client                = client.

    IF content IS NOT INITIAL.
      lo_file->mv_content_bin           = content.
    ELSEIF content_base64 IS NOT INITIAL.

      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = content_base64
        IMPORTING
          output = lo_file->mv_content_bin
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      IF sy-subrc <> 0.
        " TODO raise exception
      ENDIF.

    ENDIF.

    lo_file->id   = id.
    lo_file->name = name.
    lo_file->ext  = ext.
    lo_file->url  = url.
    lo_file->size = xstrlen( lo_file->mv_content_bin ) .
    lo_file->convertapi_url  = convertapi_url.

    ro_file = lo_file.

  ENDMETHOD.

  METHOD zif_convertapi_file~delete_service_side_copy.
    mo_client->delete( io_file = me ).
  ENDMETHOD.

  METHOD zif_convertapi_file~get_content.

    IF mv_content_bin IS INITIAL AND id IS NOT INITIAL.

      me->mo_client->download(
        EXPORTING
          io_file         = me
        IMPORTING
          ev_file_content = mv_content_bin
      ).

    ENDIF.
    rv_content = mv_content_bin.

  ENDMETHOD.

  METHOD recalc_state.
    IF me->id IS INITIAL.
      mv_has_service_side_copy = abap_false.
    ELSE.
      mv_has_service_side_copy = abap_true.
    ENDIF.
    IF me->mv_content_bin IS INITIAL.
      mv_has_client_side_copy = abap_false.
    ELSE.
      mv_has_client_side_copy = abap_true.
    ENDIF.
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
    rv_id = me->id.
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

    lo_conversion = mo_client->zif_convertapi_client~create_conversion(
            iv_target_format = i_conversion
        ).

    mo_client->convert(
      EXPORTING
        im_source       =  me
        im_parameters   =  lo_conversion
      IMPORTING
        et_target_files = lt_result_files
    ).

    IF lt_result_files[] IS NOT INITIAL.
      ro_file = lt_result_files[ 1 ].
    ELSE.
      "TODO raise exception
    ENDIF.

  ENDMETHOD.

  METHOD zif_convertapi_file~has_service_side_copy.
    rv_result = mv_has_service_side_copy.
  ENDMETHOD.

  METHOD zif_convertapi_file~upload.
    mo_client->upload( io_file = me ).
  ENDMETHOD.

  METHOD zif_convertapi_file~get_size.
    rv_size = me->size.
  ENDMETHOD.

ENDCLASS.
