CLASS zcl_zabap_toc_report DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_range_of_transport   TYPE RANGE OF trkorr,
      tt_range_of_owner       TYPE RANGE OF tr_as4user,
      tt_range_of_description TYPE RANGE OF as4text.

    METHODS:
      constructor IMPORTING report_id TYPE sy-repid ,
      set_toc_description IMPORTING toc_description TYPE REF TO zcl_zabap_toc_description,
      gather_transports IMPORTING tranports TYPE tt_range_of_transport OPTIONAL owners TYPE tt_range_of_owner OPTIONAL
                        descriptions TYPE tt_range_of_description OPTIONAL
                        include_released TYPE abap_bool DEFAULT abap_true include_tocs TYPE abap_bool DEFAULT abap_false
                        include_subtransports TYPE abap_bool DEFAULT abap_false,
      display IMPORTING layout_name TYPE slis_vari OPTIONAL,
      get_layout_from_f4_selection RETURNING VALUE(layout) TYPE slis_vari.

  PRIVATE SECTION.
    TYPES:
      t_icon TYPE c LENGTH 4,
      BEGIN OF t_report,
        transport                 TYPE trkorr,
        type                      TYPE trfunction,
        target_system             TYPE tr_target,
        owner                     TYPE tr_as4user,
        creation_date             TYPE as4date,
        description               TYPE as4text,
        create_toc                TYPE t_icon,
        create_release_toc        TYPE t_icon,
        create_release_import_toc TYPE t_icon,
        toc_number                TYPE trkorr,
        toc_status                TYPE string,
        color                     TYPE lvc_t_scol,
      END OF t_report,
      tt_report TYPE STANDARD TABLE OF t_report WITH KEY transport WITH NON-UNIQUE SORTED KEY toc COMPONENTS toc_number.

    CONSTANTS:
      BEGIN OF c_icon,
        create                TYPE t_icon VALUE '@EZ@',
        create_release        TYPE t_icon VALUE '@4A@',
        create_release_import TYPE t_icon VALUE '@K5@',
      END OF c_icon,
      BEGIN OF c_toc_columns,
        create_toc                TYPE string VALUE 'CREATE_TOC',
        create_release_toc        TYPE string VALUE 'CREATE_RELEASE_TOC',
        create_release_import_toc TYPE string VALUE 'CREATE_RELEASE_IMPORT_TOC',
      END OF c_toc_columns,
      BEGIN OF c_status_color,
        green  TYPE i VALUE 5,
        yellow TYPE i VALUE 3,
        red    TYPE i VALUE 6,
      END OF c_status_color.

    DATA:
      alv_table   TYPE REF TO cl_salv_table,
      toc_manager TYPE REF TO zcl_zabap_toc,
      layout_key  TYPE salv_s_layout_key,
      report_data TYPE tt_report.

    METHODS:
      set_column_hotspot_icon IMPORTING column TYPE lvc_fname,
      set_fixed_column_text   IMPORTING column TYPE lvc_fname text TYPE scrtext_l,
      set_status_color IMPORTING row TYPE i color TYPE i,
      set_entry_color IMPORTING entry TYPE REF TO t_report color TYPE i,
      prepare_alv_table IMPORTING layout_name TYPE slis_vari OPTIONAL,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_added_function FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function ,
      show_transport_details IMPORTING transport TYPE trkorr.
ENDCLASS.


CLASS zcl_zabap_toc_report IMPLEMENTATION.
  METHOD constructor.
    layout_key = VALUE salv_s_layout_key( report = report_id ).
    toc_manager = NEW #( NEW zcl_zabap_toc_description( zcl_zabap_toc_description=>c_toc_description-toc ) ).
  ENDMETHOD.

  METHOD set_toc_description.
    toc_manager = NEW #( toc_description ).
  ENDMETHOD.

  METHOD gather_transports.
    SELECT FROM e070
                LEFT JOIN e07t ON e07t~trkorr = e070~trkorr
                LEFT JOIN e070 AS sup ON sup~trkorr = e070~strkorr
      FIELDS e070~trkorr AS transport, e070~trfunction AS type, e070~as4user AS owner, e070~as4date AS creation_date,
          CASE WHEN e070~tarsystem <> @space THEN e070~tarsystem ELSE sup~tarsystem END AS target_system,
          e07t~as4text AS description,
          @c_icon-create AS create_toc, @c_icon-create_release AS create_release_toc, @c_icon-create_release_import AS create_release_import_toc
      WHERE e070~trkorr IN @tranports AND e070~as4user IN @owners AND as4text IN @descriptions
        AND ( @include_subtransports = @abap_true OR e070~strkorr     = @space )
        AND ( @include_released      = @abap_true OR e070~trstatus   IN ( 'L', 'D' ) )
        AND ( @include_tocs          = @abap_true OR e070~trfunction <> 'T' )
      ORDER BY e070~trkorr DESCENDING, e070~as4date DESCENDING
      INTO CORRESPONDING FIELDS OF TABLE @report_data.

    DELETE ADJACENT DUPLICATES FROM report_data COMPARING transport.
  ENDMETHOD.

  METHOD get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key = layout_key restrict = if_salv_c_layout=>restrict_none )-layout.
  ENDMETHOD.

  METHOD display.
    prepare_alv_table( layout_name ).
    alv_table->display( ).
  ENDMETHOD.

  METHOD set_column_hotspot_icon.
    DATA(col) = CAST cl_salv_column_table( me->alv_table->get_columns( )->get_column( column ) ).
    col->set_icon( if_salv_c_bool_sap=>true ).
    col->set_cell_type( if_salv_c_cell_type=>hotspot ).
    alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
     alv_table->set_screen_status( report = 'ZTOC' pfstatus = 'MAIN' ).
  ENDMETHOD.

  METHOD set_fixed_column_text.
    DATA(col) = alv_table->get_columns( )->get_column( column ).
    IF strlen( text ) > 20.
      col->set_long_text( text ).
      col->set_fixed_header_text( 'L' ).
    ELSEIF strlen( text ) > 10.
      col->set_long_text( text ).
      col->set_medium_text( CONV #( text ) ).
      col->set_fixed_header_text( 'M' ).
    ELSE.
      col->set_long_text( text ).
      col->set_medium_text( CONV #( text ) ).
      col->set_short_text( CONV #( text ) ).
      col->set_fixed_header_text( 'S' ).
    ENDIF.
  ENDMETHOD.

  METHOD prepare_alv_table.
    cl_salv_table=>factory( IMPORTING r_salv_table = alv_table CHANGING  t_table = report_data ).

    " Set columns as icons
    set_column_hotspot_icon( CONV #( c_toc_columns-create_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_import_toc ) ).

    " Set column texts
    set_fixed_column_text( column = CONV #( c_toc_columns-create_toc ) text = CONV #( TEXT-c01 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_toc ) text = CONV #( TEXT-c02 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_import_toc ) text = CONV #(  TEXT-c03 ) ).
    set_fixed_column_text( column = 'TOC_NUMBER' text =  CONV #( TEXT-c04 ) ).
    set_fixed_column_text( column = 'TOC_STATUS' text =  CONV #( TEXT-c05 ) ).

    " Set handlers
    DATA(event) = alv_table->get_event( ).
    SET HANDLER me->on_link_click FOR event.
    SET HANDLER me->on_double_click FOR event.
    SET HANDLER me->on_added_function FOR event.

    " Set layouts
    alv_table->get_layout( )->set_key( layout_key ).
    alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
    alv_table->get_layout( )->set_default( abap_true ).
    IF layout_name IS NOT INITIAL.
      alv_table->get_layout( )->set_initial_layout( layout_name ).
    ENDIF.

    " Enable standard report functions
    alv_table->get_functions( )->set_all( ).

    " Color
    alv_table->get_columns( )->set_color_column( 'COLOR' ).
  ENDMETHOD.

  METHOD set_status_color.
    DATA(color_cell) = REF #( report_data[ row ]-color ).
    CLEAR color_cell->*.
    APPEND VALUE #( fname = 'TOC_STATUS' color = VALUE #( col = color ) ) TO color_cell->*.
  ENDMETHOD.

  METHOD set_entry_color.
    CLEAR entry->color.
    APPEND VALUE #( fname = 'TOC_STATUS' color = VALUE #( col = color ) ) TO entry->color.
  ENDMETHOD.

  METHOD on_link_click.
    DATA(selected) = REF #( report_data[ row ] ).
    CLEAR selected->color.

    TRY.
        CASE column.
            "--------------------------------------------------
          WHEN c_toc_columns-create_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = CONV #( selected->description ) ).
            selected->toc_status = TEXT-s01.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          WHEN c_toc_columns-create_release_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = CONV #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            selected->toc_status = TEXT-s02.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          WHEN c_toc_columns-create_release_import_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = CONV #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            DATA(rc) = CONV i( toc_manager->import( toc = selected->toc_number target_system = selected->target_system ) ).
            selected->toc_status = TEXT-s03.
            selected->toc_status = replace( val = TEXT-s04 sub = '&1' with = |{ rc }| ).
            set_status_color( row = row color = COND #( WHEN rc = 0 THEN c_status_color-green
                                                        WHEN rc = 4 THEN c_status_color-yellow
                                                        ELSE             c_status_color-red ) ).

            "--------------------------------------------------
          WHEN OTHERS.
        ENDCASE.

      CATCH zcx_zabap_exception INTO DATA(exception).
        selected->toc_status = exception->get_text( ).
        set_status_color( row = row color = c_status_color-red ).

      CATCH zcx_zabap_user_cancel INTO DATA(user_canceled).
        selected->toc_status = TEXT-e01.
        set_status_color( row = row color = c_status_color-red ).

    ENDTRY.

    alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD on_double_click.
    DATA(selected) = REF #( report_data[ row ] ).

    CASE column.
      WHEN 'TRANSPORT'.
        show_transport_details( selected->transport ).

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD on_added_function.

  ENDMETHOD.

  METHOD show_transport_details.
    DATA batch_input TYPE TABLE OF bdcdata.

    APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=TSSN' ) TO batch_input.
    APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_SUBSCR' fval = 'RDDM0001                                0210COMMONSUBSCREEN' ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'TRDYSE01SN-TR_TRKORR' ) TO batch_input.
    APPEND VALUE #( fnam = 'TRDYSE01SN-TR_TRKORR' fval = transport ) TO batch_input.

    DATA(call_options) = VALUE ctu_params( dismode = 'E' updmode  = 'A' nobinpt = abap_true nobiend = abap_true ).
    CALL TRANSACTION 'SE01' USING batch_input OPTIONS FROM call_options.
  ENDMETHOD.



ENDCLASS.
