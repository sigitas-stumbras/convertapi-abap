interface ZIF_CONVERTAPI_CLIENT
  public .


  types TY_STORAGE_MODE type INTEGER .
  types TY_FILE_ID type STRING .
  types:
    tty_files      TYPE TABLE OF REF TO zif_convertapi_file WITH EMPTY KEY .
  types:
    BEGIN OF sty_parameter,
           name  TYPE string,
           value TYPE string,
         END OF sty_parameter .
  types:
    tty_parameters TYPE STANDARD TABLE OF sty_parameter WITH KEY name .

  constants VERSION type STRING value '0.0.1' ##NO_TEXT.
  constants:
    BEGIN OF c_param,
      file       TYPE string VALUE 'File',
      files      TYPE string VALUE 'Files',
      store_file TYPE string VALUE 'StoreFile',
      timeout    TYPE string VALUE 'Timeout',
    END OF c_param .
  constants:
    BEGIN OF c_storage_mode,
      use_service_storage TYPE ty_storage_mode VALUE 1,
      no_service_storage  TYPE ty_storage_mode VALUE 2,
      manual              TYPE ty_storage_mode VALUE 3,
    END OF c_storage_mode .

  methods CREATE_FILE
    importing
      !IV_NAME type STRING
      !IV_CONTENT type XSTRING
    returning
      value(RO_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods CREATE_FILE_FROM_FS
    importing
      !IV_PHYSICAL_FILE type STRING
      !IV_FILENAME type STRING optional
    returning
      value(RO_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods CREATE_FILE_FROM_URL
    importing
      !IV_URL type STRING
      !IV_NAME type STRING
    returning
      value(RO_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods CREATE_CONVERSION
    importing
      !IV_TARGET_FORMAT type STRING
      !IV_SOURCE_FORMAT type STRING optional
      !IT_PARAMETERS type ZIF_CONVERTAPI_CLIENT=>TTY_PARAMETERS optional
    returning
      value(RO_CONVERSION) type ref to ZIF_CONVERTAPI_CONVERSION
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods SET_AUTO_CLEANUP
    importing
      !IV_ENABLED type ABAP_BOOL .
  methods GET_AUTO_CLEANUP
    returning
      value(RV_ENABLED) type ABAP_BOOL .
  methods CLEANUP
    raising
      ZCX_CONVERTAPI_EXCEPTION .
endinterface.
