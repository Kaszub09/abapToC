FUNCTION zabap_toc_unpack.
*"----------------------------------------------------------------------
*"*"Lokalny interfejs:
*"  IMPORTING
*"     VALUE(TOC) TYPE  TRKORR
*"     VALUE(TARGET_SYSTEM) TYPE  TR_TARGET
*"  EXPORTING
*"     VALUE(ERROR) TYPE  STRING
*"     VALUE(RET_CODE) TYPE  TRRETCODE
*"----------------------------------------------------------------------
  TRY.
      "Import
      DATA system TYPE tmssysnam .
      DATA client TYPE mandt.
      SPLIT target_system AT '.' INTO system client.
      IF client IS INITIAL.
        client = sy-mandt.
      ENDIF.

      "Refresh Import queue
      DATA exception TYPE stmscalert.
      CALL FUNCTION 'TMS_MGR_REFRESH_IMPORT_QUEUES'
        EXPORTING
          iv_system    = system
          iv_monitor   = abap_true
          iv_verbose   = abap_true
        IMPORTING
          es_exception = exception
        EXCEPTIONS
          OTHERS       = 99.

      CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system                  = system
          iv_request                 = toc
          iv_client                  = client
        IMPORTING
          ev_tp_ret_code             = ret_code
        EXCEPTIONS
          read_config_failed         = 1
          table_of_requests_is_empty = 2
          OTHERS                     = 3.

    CATCH cx_root INTO DATA(cx).
      error = cx->get_longtext( ).
  ENDTRY.

ENDFUNCTION.
