CLASS zcl_convertapi_file DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_convertapi_client.

  PUBLIC SECTION.

    INTERFACES zif_convertapi_file.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mo_client TYPE REF TO zcl_convertapi_client.
    DATA: mv_has_client_side_copy TYPE abap_bool.
    DATA: mv_has_service_side_copy TYPE abap_bool.
    DATA: mv_content_bin TYPE xstring.

    CLASS-METHODS factory
      IMPORTING
        !client         TYPE REF TO zcl_convertapi_client
        !id             TYPE string OPTIONAL
        !name           TYPE string OPTIONAL
        !ext            TYPE string OPTIONAL
        !size           TYPE integer OPTIONAL
        !url            TYPE string OPTIONAL
        !content        TYPE xstring OPTIONAL
        !content_base64 TYPE string OPTIONAL
      RETURNING
        VALUE(ro_file) TYPE REF TO zif_convertapi_file.

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

    lo_file->zif_convertapi_file~id   = id.
    lo_file->zif_convertapi_file~name = name.
    lo_file->zif_convertapi_file~ext  = ext.
    lo_file->zif_convertapi_file~url  = url.

    ro_file = lo_file.

  ENDMETHOD.

  METHOD zif_convertapi_file~delete.

  ENDMETHOD.

  METHOD zif_convertapi_file~get_content_base64.

  ENDMETHOD.

  METHOD zif_convertapi_file~get_content.
    IF mv_has_client_side_copy = abap_true.
        content = mv_content_bin.
    ELSEIF mv_has_service_side_copy = abap_true.

        me->mo_client->download(
          EXPORTING
            io_file     = me
          IMPORTING
            ex_file_content = mv_content_bin
        ).

        content = mv_content_bin.
        recalc_state( ).
    ELSE.
        " TODO: RAISE EXCEPTION
    ENDIF.

  ENDMETHOD.

  METHOD recalc_state.
    IF me->zif_convertapi_file~id IS INITIAL.
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
    ext = me->zif_convertapi_file~ext.
  ENDMETHOD.

  METHOD zif_convertapi_file~get_id.
    id = me->zif_convertapi_file~id.
  ENDMETHOD.


  METHOD zif_convertapi_file~get_name.
    name = me->zif_convertapi_file~name.
  ENDMETHOD.


  METHOD zif_convertapi_file~get_url.
    url = me->zif_convertapi_file~url.
  ENDMETHOD.

  METHOD zif_convertapi_file~convert_to.

    DATA: lo_conversion TYPE REF TO zcl_convertapi_conversion.

    lo_conversion = zcl_convertapi_conversion=>factory(
        im_result_format = im_result_format
        it_parameters    = it_parameters
    ).

  ENDMETHOD.

  METHOD zif_convertapi_file~has_service_side_copy.
    rv_result = mv_has_service_side_copy.
  ENDMETHOD.

  METHOD zif_convertapi_file~upload.

  ENDMETHOD.

ENDCLASS.
