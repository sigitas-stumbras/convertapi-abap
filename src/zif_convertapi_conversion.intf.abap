interface ZIF_CONVERTAPI_CONVERSION
  public .

  METHODS convert
    IMPORTING
        it_files TYPE zif_convertapi_client=>tty_files
    RETURNING
        VALUE(rt_files) TYPE zif_convertapi_client=>tty_files.

endinterface.
