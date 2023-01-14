*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_fs IMPLEMENTATION.

  METHOD get_filename.

    DATA lv_separator       TYPE c.
    DATA lt_path_components TYPE TABLE OF string.

    IF sy-opsys CS 'WINDOWS'.
      lv_separator = '\'.
    ELSE.
      lv_separator  = '/'.
    ENDIF.

    SPLIT iv_path AT lv_separator INTO TABLE lt_path_components.

    IF lt_path_components[] IS NOT INITIAL.
      rv_filename = lt_path_components[ lines( lt_path_components[] ) ].
    ENDIF.

  ENDMETHOD.

  METHOD get_filename_from_url.

    DATA lt_url_components TYPE TABLE OF string.
    DATA lv_file TYPE string.
    DATA lv_dummy TYPE string.

    lv_file = iv_url.

    SPLIT lv_file AT '?' INTO lv_file lv_dummy. " drop query parameters
    SPLIT lv_file AT '#' INTO lv_file lv_dummy. " hash marks

    SPLIT lv_file AT '/' INTO TABLE lt_url_components.

    IF lt_url_components[] IS NOT INITIAL.
      lv_file = lt_url_components[ lines( lt_url_components[] ) ].
      IF lv_file CP '*.+*'. " consider as proper filename only if it has extension
        rv_filename = lv_file.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD read_physical_file.
    OPEN DATASET iv_path IN BINARY MODE FOR INPUT.
    READ DATASET iv_path INTO rv_content.
    CLOSE DATASET iv_path.
  ENDMETHOD.

  METHOD iso_date_to_tzntstmpl.
    DATA lv_date TYPE string.

    lv_date = iv_isotimestamp.
    REPLACE ALL OCCURRENCES OF REGEX '[T:-]+' IN lv_date WITH ''.

    rv_timestamp = lv_date.

  ENDMETHOD.

  METHOD iso_date.
    rv_iso = iv_date+0(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).
  ENDMETHOD.

ENDCLASS.
