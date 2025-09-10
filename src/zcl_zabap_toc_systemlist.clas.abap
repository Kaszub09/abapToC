CLASS zcl_zabap_toc_systemlist DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF system_line,
        sysnam TYPE tmscsys-sysnam,
        systxt TYPE tmscsys-systxt,
        colors TYPE lvc_t_scol,
      END OF system_line,
      system_lines TYPE STANDARD TABLE OF system_line WITH EMPTY KEY.
    CLASS-METHODS get
      RETURNING
        VALUE(result) TYPE system_lines.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zabap_toc_systemlist IMPLEMENTATION.
  METHOD get.
    DATA: systems_raw TYPE STANDARD TABLE OF tmscsys.
    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_QUEUE'
      EXPORTING
        iv_count_only      = 'X'
      TABLES
        tt_system          = systems_raw
      EXCEPTIONS
        read_config_failed = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      result =
        VALUE #(
          ( systxt = 'Use default system'(001)
            colors = VALUE #( ( fname = '' color = VALUE #( col = col_positive ) ) ) )
          ( LINES OF CORRESPONDING #( systems_raw ) ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
