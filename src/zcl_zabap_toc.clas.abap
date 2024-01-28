CLASS zcl_zabap_toc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS c_abap_version TYPE string VALUE `1.0.1`.

    METHODS:
      create IMPORTING source_transport TYPE trkorr source_description TYPE as4text target_system TYPE tr_target RETURNING VALUE(toc) TYPE trkorr RAISING zcx_zabap_exception,
      release IMPORTING toc TYPE trkorr RAISING zcx_zabap_exception,
      import IMPORTING toc TYPE trkorr target_system TYPE tr_target RETURNING VALUE(ret_code) TYPE trretcode RAISING zcx_zabap_exception,
      import_objects IMPORTING source_transport TYPE trkorr destination_transport TYPE trkorr RAISING zcx_zabap_exception,
      check_status_in_system IMPORTING toc TYPE trkorr system TYPE tr_target EXPORTING imported TYPE abap_bool rc TYPE i RAISING zcx_zabap_exception,
      check_release_status IMPORTING request               TYPE trkorr
                           RETURNING VALUE(release_status) TYPE char03.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA c_transport_type_toc TYPE trfunction VALUE 'T'.

    "METHODS get_toc_description IMPORTING source_transport TYPE trkorr RETURNING VALUE(description) TYPE string.
    METHODS get_toc_description IMPORTING source_description TYPE as4text RETURNING VALUE(description) TYPE string.
ENDCLASS.



CLASS zcl_zabap_toc IMPLEMENTATION.


  METHOD check_release_status.

    DATA: es_cofile  TYPE ctslg_cofile,
          ev_user    TYPE e070-as4user,
          ev_project TYPE trkorr.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr  = request
      IMPORTING
        es_cofile  = es_cofile
        ev_user    = ev_user
        ev_project = ev_project.

*     If es_cofile-exists = '' (and consequently no entries in systm) -> not released yet
*     if es_cofile-exists = 'X' but systm is empty -> is being released
*     if es_cofile-exists = 'X' and systm DEV -> released with retcode from systm-rc
*     export step from DEV system: E
*     import steps into QUA system: I and V?
*     https://github.com/mmisnan/ABAP/blob/master/ZTRCHECK

    CLEAR: release_status.

*   if no entries in systm -> not released
*   otherwise check the status

    IF es_cofile-exists = 'X'.
      DATA(systm) = es_cofile-systems.

*     Check release status from source system
      IF line_exists( systm[ systemid = sy-sysid ] ).
        IF line_exists( systm[ systemid = sy-sysid ]-steps[ stepid = 'E' ] ).
          release_status = 'REL'.
        ELSE.
          release_status = 'RUN'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


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


  METHOD create.
    TRY.
        cl_adt_cts_management=>create_empty_request( EXPORTING iv_type = 'T' iv_text = CONV #( get_toc_description( source_description ) )
                                               iv_target = target_system IMPORTING es_request_header = DATA(transport_header) ).
        import_objects( source_transport = source_transport destination_transport = transport_header-trkorr ).
        toc = transport_header-trkorr.

      CATCH cx_root INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_zabap_exception EXPORTING message = replace( val = TEXT-e01 sub = '&1' with = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_toc_description.
    description = replace( val = TEXT-t01 sub = '&1' with = source_description ).
  ENDMETHOD.


  METHOD import.
    DATA error TYPE string.

    CALL FUNCTION 'ZABAP_TOC_UNPACK' DESTINATION target_system
      EXPORTING
        toc           = toc
        target_system = target_system
      IMPORTING
        ret_code      = ret_code
        error         = error.

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
