CLASS zcl_zabap_toc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING toc_description TYPE REF TO zcl_zabap_toc_description,
      create IMPORTING source_transport TYPE trkorr source_description TYPE string target_system TYPE tr_target
             RETURNING VALUE(toc) TYPE trkorr
             RAISING zcx_zabap_exception zcx_zabap_user_cancel,
      release IMPORTING toc TYPE trkorr RAISING zcx_zabap_exception,
      import IMPORTING toc TYPE trkorr target_system TYPE tr_target max_wait_time_in_sec TYPE i DEFAULT 30
                       ignore_version type abap_bool default abap_true
             RETURNING VALUE(ret_code) TYPE trretcode RAISING zcx_zabap_exception,
      import_objects IMPORTING source_transport TYPE trkorr destination_transport TYPE trkorr RAISING zcx_zabap_exception,
      check_status_in_system IMPORTING toc TYPE trkorr system TYPE tr_target EXPORTING imported TYPE abap_bool rc TYPE i RAISING zcx_zabap_exception.

protected section.
  PRIVATE SECTION.
    CONSTANTS c_toc_doesnt_exists_retcode TYPE trretcode VALUE '0152'.
    CONSTANTS c_transport_type_toc TYPE trfunction VALUE 'T'.
    DATA toc_description TYPE REF TO zcl_zabap_toc_description.
ENDCLASS.



CLASS ZCL_ZABAP_TOC IMPLEMENTATION.


  METHOD check_status_in_system.
    DATA:
      settings TYPE ctslg_settings,
      cofiles  TYPE ctslg_cofile.

    APPEND system TO settings-systems.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr   = toc
        is_settings = settings
      IMPORTING
        es_cofile   = cofiles.

    IF cofiles-exists = abap_false.
      RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = CONV #( TEXT-e05 ) .
    ENDIF.

    imported = cofiles-imported.
    rc = cofiles-rc.
  ENDMETHOD.


  METHOD constructor.
    me->toc_description = toc_description.
  ENDMETHOD.


  METHOD create.
    TRY.
        cl_adt_cts_management=>create_empty_request( EXPORTING iv_type = 'T'
            iv_text = CONV #( toc_description->get_toc_description( original_transport = source_transport original_desciption = source_description ) )
            iv_target = target_system IMPORTING es_request_header = DATA(transport_header) ).
        import_objects( source_transport = source_transport destination_transport = transport_header-trkorr ).
        toc = transport_header-trkorr.

      CATCH zcx_zabap_user_cancel INTO DATA(zcx).
        RAISE EXCEPTION TYPE zcx_zabap_user_cancel.

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = replace( val = TEXT-e01 sub = '&1' with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD import.
    DATA error TYPE string.
    GET TIME. "Update sy-uzeit before calculating wait_until
    DATA(wait_until) = CONV t( sy-uzeit + max_wait_time_in_sec ).
    ret_code = c_toc_doesnt_exists_retcode.

    WHILE ret_code = c_toc_doesnt_exists_retcode AND sy-uzeit <= wait_until.
      CALL FUNCTION 'ZABAP_TOC_UNPACK' DESTINATION target_system
        EXPORTING
          toc           = toc
          target_system = target_system
          ignore_version = ignore_version
        IMPORTING
          ret_code      = ret_code
          error         = error.

      GET TIME. "Update sy-uzeit before comparing time
    ENDWHILE.

    IF strlen( error ) > 0.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING
          message = replace( val = TEXT-e03 sub = '&1' with = error ).
    ENDIF.
  ENDMETHOD.


  METHOD import_objects.
    DATA request_headers TYPE trwbo_request_headers.
    DATA requests        TYPE trwbo_requests.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = source_transport
      IMPORTING
        et_request_headers = request_headers
        et_requests        = requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_zabap_exception
        EXPORTING
          message = replace( val = replace( val = TEXT-e01 sub = '&1' with = |{ sy-subrc }| ) sub = '&2' with = 'TR_READ_REQUEST_WITH_TASKS' ).
    ENDIF.

    LOOP AT request_headers REFERENCE INTO DATA(request_header) WHERE trkorr = source_transport OR strkorr = source_transport.
      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = abap_false
          wi_trkorr_from           = request_header->trkorr
          wi_trkorr_to             = destination_transport
          wi_without_documentation = abap_false
        EXCEPTIONS
          db_access_error          = 1                " Database access error
          trkorr_from_not_exist    = 2                " first correction does not exist
          trkorr_to_is_repair      = 3                " Target correction is repair
          trkorr_to_locked         = 4                " Command file TRKORR_TO blocked, (SM12)
          trkorr_to_not_exist      = 5                " second correction does not exist
          trkorr_to_released       = 6                " second correction already released
          user_not_owner           = 7                " User is not owner of first request
          no_authorization         = 8                " No authorization for this function
          wrong_client             = 9                " Different clients (source - target)
          wrong_category           = 10               " Different category (source - target)
          object_not_patchable     = 11
          OTHERS                   = 12.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_zabap_exception
          EXPORTING
            message = replace( val = replace( val = TEXT-e01 sub = '&1' with = |{ sy-subrc }| ) sub = '&2' with = 'TR_COPY_COMM' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD release.
    TRY.
        DATA(cts_api) = cl_cts_rest_api_factory=>create_instance( ).
        cts_api->release( iv_trkorr = toc iv_ignore_locks = abap_true ).

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = replace( val = TEXT-e02 sub = '&1' with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
