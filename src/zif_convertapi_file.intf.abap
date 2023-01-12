interface ZIF_CONVERTAPI_FILE
  public .


  methods UPLOAD
    returning
      value(RO_SAME_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods DELETE_SERVICE_SIDE_COPY
    returning
      value(RO_SAME_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods GET_CONTENT
    returning
      value(RV_CONTENT) type XSTRING
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods GET_SIZE
    returning
      value(RV_SIZE) type INTEGER .
  methods GET_ID
    returning
      value(RV_ID) type STRING .
  methods GET_CONVERTAPI_URL
    returning
      value(RV_URL) type STRING .
  methods GET_EXTERNAL_URL
    returning
      value(RV_URL) type STRING .
  methods GET_NAME
    returning
      value(RV_NAME) type STRING .
  methods GET_EXT
    returning
      value(RV_EXT) type STRING .
  methods CONVERT_TO
    importing
      !I_CONVERSION type ANY
    returning
      value(RO_NEW_FILE) type ref to ZIF_CONVERTAPI_FILE
    raising
      ZCX_CONVERTAPI_EXCEPTION .
  methods HAS_SERVICE_SIDE_COPY
    returning
      value(RV_RESULT) type ABAP_BOOL .
endinterface.
