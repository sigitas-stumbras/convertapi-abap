*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_fs DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get_filename
      IMPORTING
        iv_path            TYPE string
      RETURNING
        VALUE(rv_filename) TYPE string.


    CLASS-METHODS read_file
      IMPORTING
        iv_path             TYPE string
      RETURNING
        VALUE(rv_content) TYPE xstring.

ENDCLASS.
