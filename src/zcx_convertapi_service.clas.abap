class ZCX_CONVERTAPI_SERVICE definition
  public
  inheriting from CX_STATIC_CHECK
  create public

  global friends ZCL_CONVERTAPI_CLIENT .

public section.

  data MSG type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CONVERTAPI_SERVICE IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MSG = MSG .
  endmethod.
ENDCLASS.
