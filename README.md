# ConvertAPI client for ABAP

Use **ConvertAPI.com** cloud service to convert Microsoft Office, Apple iWork, OpenOfice document formats, PDFs, images, CAD drawings, archives and more in your **SAP NetWeaver AS ABAP** as easy as:


```abap
lo_client = zcl_convertapi_client=>create( ... ).

lo_docx_file = lo_client->create_file(
    iv_name    = 'Document.docx'  
    iv_content = lv_docx_content
).

lv_pdf_content = lo_docx_file->convert_to( 'pdf' )->get_content( ).
```

For a complete up-to-date list of supported file formats, conversions and other capabilities please see: https://www.convertapi.com/doc

> **Note:** We would like to make it very clear that this is not a file convertion library. This is a client library to consume a publicly accessible file conversion service, i.e., the content being converted will travel outside your ABAP system and your network. 

## Prerequisites

* User account at ConvertAPI.com
* SAP NetWeaver 7.4 or later
* The SSL certificate of Root Certification Authority of https://v2.convertapi.com must be imported into relevant *SSL Client Identity* for ConvertAPI client to be able to connect to the sevice (transaction STRUST, identity ANONYM or a custom one should be used).

> **Note:** Please make sure to add the root CA certificate of https://v2.convertapi.com sepecifically and not https://www.convertapi.com as they may have different root CAs.

* Additional security settings may be required based on your particular SAP system, landscape and network setup. Please consult with your BASIS team regarding the requirments for your ABAP application server to access a public HTTPS services.

# ConvertAPI client API

## Creating ConvertAPI client instance

### **zcl_convertapi_client=>create**
Static factory method to create new ConvertAPI client instance.

| Parameter |  Description |
|---|---|
| iv_api_secret | Value of ConvertAPI user credential *API Secret* - can be found in your ConvertAPI user profile *Authentication* section. | 
| iv_api_key | Value of ConvertAPI user credential *API Key* - can be found in your ConvertAPI user profile *Authentication* section. |
| io_http_client | Reference to *if_http_client* interface of HTTP client instance. It may be created by one of the static factory methods of class *cl_http_client*: *create*, *create_by_url* or *create_by_destination*. See example below. |
| iv_service_side_storage | Optional. *ABAP_TRUE* (default) - create server side copies of all files for more efficient file transfers, reuse and conversion flows. *ABAP_FALSE* - do not store any files in the ConvertAPI service at any time, i.e, all source files are submited with the request and all result files received in the response.
| ro_client | Returned reference to an interface *zif_convertapi_client* of newly created ConvertAPI client instance. | 


## EXAMPLES

### EXAMPLE - Creating HTTP client instance using *cl_http_client=>create_by_url*

```abap
DATA: lo_http_client TYPE REF TO if_http_client.

cl_http_client=>create_by_url(
    EXPORTING
        url          = 'https://v2.convertapi.com:443'    " ConvertAPI service host
        ssl_id       = 'ANONYM'                           " SSL client identity used (STRUST)
    IMPORTING
        client       = lo_http_client
).
```

### EXAMPLE - Creating ConvertAPI client instance
```abap
DATA lo_client TYPE REF TO zif_convertapi_client.

lo_client = zcl_convertapi_client=>create(
    iv_api_key              = lv_api_key
    iv_api_secret           = lv_api_secret
    io_http_client          = lo_http_client
    iv_service_side_storage = abap_false
).
```

### EXAMPLE - Multiple Input Files
Two PDF file merger
```abap
DATA: lt_sources    TYPE zif_convertapi_client=>tty_files. 
DATA: lt_results    TYPE zif_convertapi_client=>tty_files. 
DATA: lo_client     TYPE REF TO zif_convertapi_client.
DATA: lo_conversion TYPE REF TO zif_convertapi_conversion.

lo_client = zcl_convertapi_client=>create( ... ).

APPEND lo_client->create_file(
    iv_name    = 'title.pdf'  
    iv_content = lv_title_content
) TO lt_sources.

APPEND lo_client->create_file(
    iv_name    = 'main.pdf'  
    iv_content = lv_pdf_content
) TO lt_sources.

lo_client(lo_conversion) = lo_client->create_conversion(
    iv_target_format = 'merge'
    it_parameters = VALUE #(
        ( name = 'ColorSpace'         value = 'gray' )
        ( name = 'ImageQuality'       value = '100' )
        ( name = 'ImageInterpolation' value = 'true' )
    )
).

lt_results = lo_conversion->convert( lt_sources ).

IF lt_results[] IS NOT INITIAL.
    lv_pdf_content = lt_results[ 1 ]->get_content( ).
ENDIF.

lo_client->clean_up( ).
```