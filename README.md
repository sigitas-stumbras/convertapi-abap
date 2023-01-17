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

> **Note:** We would like to make it very clear that this is not a file convertion library. This is a client library to consume a publicly accessible file conversion service - the content being converted will travel outside your ABAP system and your network. 

## Prerequisites

* User account at ConvertAPI.com
* SAP NetWeaver 7.4 or later
* abapGit is required for ConvertAPI client installation and updates. Please visit https://abapgit.org/ for more information.
* The SSL certificate of Root Certification Authority of https://v2.convertapi.com must be imported into relevant *SSL Client Identity* for ConvertAPI client to be able to connect to the sevice (transaction STRUST, identity ANONYM or a custom one should be used).

> **Note:** Please make sure to add the root CA certificate of https://v2.convertapi.com sepecifically and not https://www.convertapi.com as they may have different root CAs.

* Additional security settings may be required based in your particular SAP system, landscape and network setup. Please consult with your BASIS team regarding the requirments for your ABAP application server to access a public HTTPS service.

## Installing ConvertAPI client

Installation of ConvertAPI client to your ABAP system can be perfomed using abapGit tool, which is a git source code version control client for ABAP. The entire installation of the client consists of just simply cloning this very git repository to your ABAP sandbox,  evaluation or development system in customer/Z namespace. From there it can further be distributed through out your SAP landscape according your standard transport paths.

We strongly recommend to use an exclusively dedicated package for ConvertAPI client installation. The package is named ZCONVERTAPI by default.

## Updating ConvertAPI client

Updating the client is performed in very similar way as installation - by syncing/pulling the latest version of the client from git repository to your initial ABAP system with abapGit tool. Followed by further distribution through out your SAP landscape using your standard transport paths.

## Uninstalling ConvertAPI client

Uninstall can only be performed manually and in your initial installation ABAP system. Empty the package by deleting it's contents if you used a dedicated package for client installation. Otherwise - find and delete ConvertAPI client objects individually. In both cases take a great care to only deleted the objects that follow `Z*CONVERTAPI*` naming pattern. Distribute changes across the landscape afterwards.


## Overview of using ConvertAPI client 

### Step 1 - Create HTTP client instance 
See [Creating HTTP client instance](#creating-http-client-instance)
### Step 2 - Create ConvertAPI client instance 
* [`zcl_convertapi_client=>create( )`](#static-method-create)
### Step 3 - Create the source file instance(s)
The alternatives are:
- [`zif_convertapi_client->create_file( )`](#method-create_file) - by directly providing name and contens of the file.
- [`zif_convertapi_client->create_file_from_fs( )`](#method-create_file_from_fs) - by providing full path to file on the application server.
- [`zif_convertapi_client->create_file_from_url( )`](#method-create_file_from_url) - by providing URL to the file, where in can be downloaded by the ConvertAPI service directly.

> **Note**: creating an instance of file does not automaticaly upload the file to ConvertAPI service. It is just a local in-memory copy of the file. Or in case of create_by_url - just a URL link.
### Step 4 - Create conversion parameter instance(s)
* [`zif_convertapi_client->create_conversion( )`](#method-create_conversion)

All the required conversion parameter values may be set in the instance creation. To set them at a later point use:

* [`zif_convertapi_conversion->set_parameter( )`](#method-set_parameter) - set single parameter.
* [`zif_convertapi_conversion->set_parameters( )`](#method-set_parameters) - set multiple paramters.

For a complete up-to-date list of parameters supported by particular conversion, please, visit: https://www.convertapi.com/doc
### Step 5 - Submit the conversion(s) to service
* [`zif_convertapi_file->convert_to( )`](#method-convert_to) - one source file to one result file conversion only.
* [`zif_convertapi_conversion->convert( )`](#method-convert) - all conversions supported - one or more to one or more.
### Step 6 - Get conversion result(s)
* [`zif_convertapi_file->get_content( )`](#method-get_content)
### Step 7 - Cleanup (for manual mode only)
* [`zif_convertapi_client->cleanup( )`](#method-cleanup)



## Creating HTTP client instance
When creating ConvertAPI client instance you will need to pass HTTP client instance (with interface if_http_client) as a paramter. There are two options of creating a http client instance.

### Option 1 - by RFC destination. Recommended
* Use `cl_http_client=>create_by_destination( )`
* Requires a dedicated RFC destination (SM59) with the following configuration:

| Section | Parameter | Value |
|---|---|---|
| | RFC Destination | `CONVERTAPI` or arbitrary |
| | Connection Type | `G` |
| | Description | `ConvertAPI.com API` |
| Technical Settings | Target Host | `v2.convertapi.com` |
| Technical Settings | Service No | `443` |
| Logon & Security | Logon with user | `Basic Authenticaton` |
| Logon & Security | User | API key in ConvertAPI user profile |
| Logon & Security | Password | API secret in ConvertAPI user profile |
| Logon & Security | SSL | `Active` |
| Logon & Security | SSL Certificate | `ANONYM` or custom/dedicated profile |
| Special Options | HTTP Vesion | `HTTP 1.1` |

Additional settings might be required for you particular case. Please, consult with your BASIS team.

### Option 2 - by URL
* Use `cl_http_client=>create_by_url( )` with parameter values:
  - `url` = `https://v2.convertapi.com:443`
  - `ssl_id` = `ANONYM` or custom/dedicated profile
* You will need to pass ConvertAPI user credentials in `iv_api_key` and `iv_api_secret` parameters when creating ConvertAPI client instance.



## Modes of ConvertAPI client operation

ConvertAPI client has several modes of operation that can be used to better balance the network-use efficiency and security concerns for your particular use case.

* **Service-side storage** - each file being converted would be first uploaded in a separate HTTP request and stored in ConvertAPI file storage service to be used for conversion. Result files are stored in ConvertAPI service as well where they are retrieved from in a separate request.
* **No service-side storage** - the file(s) being converted are submited and the result file(s) are retrieved in a single service request. ConvertAPI file storage service is not being used.
* **Manual** - This modes enables reuse of service-side copies of files for complex conversions to make more efficient use of network.

None of the modes are mutually exclusive in any way - switching the modes should not break the code. In fact from the point of view of client usage the two modes *Service-side storage* and *No service-side storage* are indistinguishable. *Manual* mode requires certain additional coding to make full and good use of the mode.

|  | Service-side storage | No service-side storage | Manual |
|---|---|---|---|
| Upload of file(s) to be converted  | Implicitly right before the conversion request | With the conversion request | By either explicit *file->upload()* method call or implicitly with the conversion request (for the file(s) that was not uploaded at the moment of the request) |
| Download of result file(s) | Implicitly immediately after the conversion response | In the conversion response | *file->get_content()* method call |
| **Auto-cleanup** after each conversion (delete service-side copies of all the files involved)| On | On | Off 
| On run-time exception | Auto-cleanup is triggered | Auto-cleanup is triggered | No auto-cleanup |
| Network traffic efficency | Good for unique single-step conversions | Least efficient (~40% worse than with service-side storage)  | Best for complex conversions |



# ConvertAPI client API

## Class **`zcl_convertapi_client`**

### Static method **`create()`**
Static factory method to create new ConvertAPI client instance.

| Parameter |  Description |
|---|---|
| `iv_api_secret` | Value of ConvertAPI user credential *API Secret* - can be found in your ConvertAPI user profile *Authentication* section. Optional in some cases. See [Creating HTTP client instance](#creating-http-client-instance). | 
| `iv_api_key` | Value of ConvertAPI user credential *API Key* - can be found in your ConvertAPI user profile *Authentication* section. Optional in some cases. See [Creating HTTP client instance](#creating-http-client-instance). |
| `io_http_client` | HTTP client instance. See [Creating HTTP client instance](#creating-http-client-instance). |
| `iv_storage_mode` | Optional. See [Modes of operation](#modes-of-operation). Use `zif_convertapi_client=>c_storage_mode-*` for possible parameter values.
| `ro_client` | Returned reference to an interface `zif_convertapi_client` of newly created ConvertAPI client instance. | 

## Interface `zif_convertapi_client`

### Method **`create_conversion`**
Factory method to create ConvertAPI conversion parameters instance.

### Method **`create_file( )`**
Factory method to create ConvertAPI file instance directly by providing file name and file contents.

| Parameter |  Description |
|---|---|
| `iv_filename` | Name of the file | 
| `iv_content` | Contents of the file (xstring) | 
| `ro_file` | Returned reference to an interface `zif_convertapi_file` of newly created ConvertAPI file instance | 

### Method **`create_file_from_fs( )`**
Factory method to create ConvertAPI file instance by providing full path to file on the file system of the application server.

| Parameter |  Description |
|---|---|
| `iv_physical_file` | Full path (absoulte or relative) to a file on the file system of the application server | 
| `iv_filename` | Optional. Name of the file to override physical filename  |
| `ro_file` | Returned reference to an interface `zif_convertapi_file` of newly created ConvertAPI file instance | 

### Method **`create_file_from_url( )`**
Factory method to create ConvertAPI file instance by providing full path to file on the file system of the application server.

| Parameter |  Description |
|---|---|
| `iv_url` | URL of a file that would be accessible to ConvertAPI services directly | 
| `iv_filename` | Optional. Name of the file to override filename in the url |
| `ro_file` | Returned reference to an interface `zif_convertapi_file` of newly created ConvertAPI file instance | 

### Method **`cleanup( )`**
Deletes all existing ConvertAPI service copies of files (both source and result) involved with current client instance.

### Method **`get_auto_cleanup( )`**
Get current auto-cleanup setting.

### Method **`set_auto_cleanup( )`**
Set auto-cleanup setting.

### Method **`get_user_info( )`**
Retrieve user profile information.
 
### Method **`get_usage_history( )`**
Retrieve conversion history of the user for given period.


## Interface **`zif_convertapi_conversion`**

### Method **`convert`**
Submit the source file(s) with conversion parameters to ConvertAPI service and receive the results

### Method **`get_parameter`**
Get single conversion paramter value

### Method **`get_all_parameters`**
Get all values of all set conversion paramters

### Method **`get_result_format`**
Get result format of the conversion

### Method **`get_source_format`**
Get source format of the conversion, which was set explicitly or derived from the source file name

### Method **`set_parameter`**
Set single paramter values

### Method **`set_parameters`**
Set multiple paramter values

### Method **`clear_parameter`**
Clear single conversion parameter

## Interface **`zif_convertapi_file`**

### Method **`convert_to`**
Submit the file with conversion parameters to ConvertAPI service and receive single result file

### Method **`delete_service_side_copy`**
Delete ConvertAPI service side copy of the file if it exists

### Method **`get_content`**
Get file content

### Method **`get_convertapi_url`**
Get URL of ConvertAPI service side copy of the file if it exists

### Method **`get_ext`**
Get file extention

### Method **`get_external_url`**
Get URL of the file, where it can be downlaoded by ConvertAPI service directly

### Method **`get_name`**
Get the name of the file (incl. extention)

### Method **`get_size`**
Get the size of the file in bytes

### Method **`has_service_side_copy`**
Check if there is ConvertAPI service side copy of the file

### Method **`upload`**
Create a ConvertAPI service side copy of the file

# Examples

## Creating HTTP client instance

The example of creating HTTP client instance using `cl_http_client=>create_by_destination`

```abap
DATA: lo_http_client TYPE REF TO if_http_client.

cl_http_client=>create_by_destionation(
    EXPORTING
        destination   = 'CONVERTAPI'  " Logical destination (specified in function call)
    IMPORTING
        client       = lo_http_client
).
```


The example of creating HTTP client instance using `cl_http_client=>create_by_url`

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
    iv_api_key       = lv_api_key
    iv_api_secret    = lv_api_secret
    io_http_client   = lo_http_client
    iv_storage_mode  = zif_convertapi_client=>c_storage_mode-no_service_storage
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