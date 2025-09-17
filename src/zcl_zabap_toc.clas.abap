CLASS zcl_zabap_toc DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING toc_description TYPE REF TO zcl_zabap_toc_description,
      create IMPORTING source_transport TYPE trkorr source_description TYPE string target_system TYPE tr_target
             RETURNING VALUE(toc) TYPE trkorr
             RAISING zcx_zabap_exception zcx_zabap_user_cancel,
      release IMPORTING toc TYPE trkorr RAISING zcx_zabap_exception,
      import IMPORTING toc TYPE trkorr target_system TYPE tr_target max_wait_time_in_sec TYPE i DEFAULT 30
                       ignore_version TYPE abap_bool DEFAULT abap_true
             RETURNING VALUE(ret_code) TYPE trretcode RAISING zcx_zabap_exception,
      import_objects IMPORTING source_transport TYPE trkorr destination_transport TYPE trkorr RAISING zcx_zabap_exception,
      check_status_in_system IMPORTING toc TYPE trkorr system TYPE tr_target EXPORTING imported TYPE abap_bool rc TYPE i RAISING zcx_zabap_exception.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_ret_code,
        doesnt_exists     TYPE trretcode VALUE '0152',
        unpacked_ok       TYPE trretcode VALUE '0000',
        unpacked_warnings TYPE trretcode VALUE '0004',
      END OF c_ret_code,
      c_transport_type_toc TYPE trfunction VALUE 'T'.

    TYPES:
      tt_systems TYPE STANDARD TABLE OF tr_target WITH EMPTY KEY,
      BEGIN OF t_systems_cache,
        target_system TYPE tr_target,
        systems       TYPE tt_systems,
      END OF t_systems_cache,
      tt_systems_cache TYPE SORTED TABLE OF t_systems_cache WITH UNIQUE KEY target_system.

    METHODS:
      get_systems IMPORTING target_system TYPE tr_target RETURNING VALUE(systems) TYPE tt_systems.

    DATA:
      toc_description TYPE REF TO zcl_zabap_toc_description,
      systems_cache   TYPE tt_systems_cache.
ENDCLASS.

CLASS zcl_zabap_toc IMPLEMENTATION.
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
      RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = CONV #( TEXT-e05 ).
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

      CATCH zcx_zabap_user_cancel INTO DATA(zcx). " TODO: variable is assigned but never used (ABAP cleaner)
        RAISE EXCEPTION TYPE zcx_zabap_user_cancel.

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = replace( val = TEXT-e01 sub = '&1' with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD import.
    DATA:
      error            TYPE string,
      unpacked_systems TYPE string.

    LOOP AT get_systems( target_system ) REFERENCE INTO DATA(system).
      GET TIME. "Update sy-uzeit before calculating wait_until
      DATA(wait_until) = CONV t( sy-uzeit + max_wait_time_in_sec ).
      ret_code = c_ret_code-doesnt_exists.

      WHILE ret_code = c_ret_code-doesnt_exists AND sy-uzeit <= wait_until.
        CALL FUNCTION 'ZABAP_TOC_UNPACK' DESTINATION system->*
          EXPORTING
            toc            = toc
            target_system  = system->*
            ignore_version = ignore_version
          IMPORTING
            ret_code       = ret_code
            error          = error.

        GET TIME. "Update sy-uzeit before comparing time
      ENDWHILE.

      IF ( ret_code = c_ret_code-unpacked_ok OR ret_code = c_ret_code-unpacked_warnings ) AND strlen( error ) = 0.
        unpacked_systems = |{ unpacked_systems } { replace( val = TEXT-001 sub = '&1' with = system->* ) }|.
      ELSE.
        error = COND #( WHEN error IS INITIAL THEN ret_code ELSE error ).
        RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = |{ unpacked_systems
            } { replace( val = replace( val = TEXT-e03 sub = '&1' with = system->* ) sub = '&2' with = error ) }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD import_objects.
    DATA request_headers TYPE trwbo_request_headers.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = source_transport
      IMPORTING
        et_request_headers = request_headers
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

  METHOD get_systems.
    IF target_system(1) <> '/'.
      systems = VALUE #( ( target_system ) ).
      RETURN.
    ENDIF.

    DATA(systems_ref) = REF #( systems_cache[ target_system = target_system ] OPTIONAL ).
    IF systems_ref IS BOUND.
      systems = systems_ref->systems.
      RETURN.
    ENDIF.

    INSERT VALUE #( target_system = target_system ) INTO TABLE systems_cache REFERENCE INTO systems_ref.

    SELECT concat( tarsystem, concat( '.', tarclient ) ) FROM tcetarg
    WHERE version IN ( SELECT MAX( version ) AS version FROM tcetarg WHERE targ_group = @target_system )
        AND targ_group = @target_system
    INTO TABLE @systems_ref->systems.

    systems = systems_ref->systems.
  ENDMETHOD.
ENDCLASS.
