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

  METHOD read_file.
    OPEN DATASET iv_path IN BINARY MODE FOR INPUT.
    READ DATASET iv_path INTO rv_content.
    CLOSE DATASET iv_path.
  ENDMETHOD.

ENDCLASS.
