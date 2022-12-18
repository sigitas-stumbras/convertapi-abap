CLASS zcl_convertapi_conversion DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA result_format TYPE string READ-ONLY .
    DATA parameters TYPE zcl_convertapi_client=>tty_parameters READ-ONLY .

    CLASS-METHODS factory
      IMPORTING
        !im_result_format    TYPE any
        !it_parameters       TYPE zcl_convertapi_client=>tty_parameters OPTIONAL
      RETURNING
        VALUE(ro_conversion) TYPE REF TO zcl_convertapi_conversion.

    METHODS constructor
      IMPORTING
        !im_result_format TYPE string
        !it_parameters    TYPE zcl_convertapi_client=>tty_parameters OPTIONAL .

    METHODS get_parameter
      IMPORTING
        !iv_name TYPE string
      RETURNING
        VALUE(rv_value) TYPE string.

    METHODS set_parameter
      IMPORTING
        !iv_name TYPE string
        !iv_value TYPE string
        !iv_overwrite TYPE abap_bool DEFAULT abap_true.

    METHODS clear_parameter
      IMPORTING
        !iv_name TYPE string.

   METHODS is_parameter_set
      IMPORTING
        !iv_name TYPE string
      RETURNING VALUE(rv_is_set) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_convertapi_conversion IMPLEMENTATION.


  METHOD constructor.

    result_format = im_result_format.
    APPEND LINES OF it_parameters TO parameters.

  ENDMETHOD.

  METHOD factory.

    DATA: lo_typedescr  TYPE REF TO cl_abap_typedescr,
          lo_classdescr TYPE REF TO cl_abap_classdescr.

    lo_typedescr = cl_abap_typedescr=>describe_by_data( im_result_format ).

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        ro_conversion = im_result_format.
      WHEN cl_abap_typedescr=>typekind_clike OR cl_abap_typedescr=>typekind_string.
        ro_conversion = NEW zcl_convertapi_conversion(
            im_result_format = im_result_format
            it_parameters    = it_parameters
        ).
    ENDCASE.

  ENDMETHOD.

  METHOD get_parameter.
    FIELD-SYMBOLS: <param> LIKE LINE OF parameters.
    READ TABLE parameters WITH KEY name = iv_name ASSIGNING <param>.
    IF SY-SUBRC = 0.
        rv_value = <param>-value.
    ENDIF.
  ENDMETHOD.

  METHOD is_parameter_set.
    READ TABLE parameters WITH KEY name = iv_name TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
        rv_is_set = abap_true.
    ELSE.
        rv_is_set = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD set_parameter.
    FIELD-SYMBOLS: <param> LIKE LINE OF parameters.
    READ TABLE parameters WITH KEY name = iv_name ASSIGNING <param>.
    IF SY-SUBRC <> 0.
        APPEND INITIAL LINE TO parameters ASSIGNING <param>.
        <param>-name = iv_name.
    ENDIF.
    IF iv_overwrite = abap_true.
      <param>-value = iv_value.
    ENDIF.
  ENDMETHOD.

  METHOD clear_parameter.
    DELETE parameters[] WHERE name = iv_name.
  ENDMETHOD.

ENDCLASS.
