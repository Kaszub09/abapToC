CLASS zcx_zabap_exception DEFINITION INHERITING FROM cx_static_check PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING message TYPE string OPTIONAL,
      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA message TYPE string.
ENDCLASS.


CLASS zcx_zabap_exception IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->message = message.
  ENDMETHOD.

  METHOD get_text.
    result = me->message.
  ENDMETHOD.
ENDCLASS.
