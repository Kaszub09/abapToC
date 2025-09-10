CLASS zcl_zabap_toc_report DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_range_of_transport   TYPE RANGE OF trkorr,
      tt_range_of_owner       TYPE RANGE OF tr_as4user,
      tt_range_of_description TYPE RANGE OF as4text.

    METHODS:
      constructor IMPORTING report_id TYPE sy-repid ignore_version TYPE abap_bool,
      set_max_wait_time IMPORTING seconds TYPE i,
      set_toc_description IMPORTING toc_description TYPE REF TO zcl_zabap_toc_description,
      gather_transports IMPORTING tranports TYPE tt_range_of_transport OPTIONAL owners TYPE tt_range_of_owner OPTIONAL
                        descriptions TYPE tt_range_of_description OPTIONAL
                        include_released TYPE abap_bool DEFAULT abap_true include_tocs TYPE abap_bool DEFAULT abap_false
                        include_subtransports TYPE abap_bool DEFAULT abap_false,
      display IMPORTING layout_name TYPE slis_vari OPTIONAL,
      get_layout_from_f4_selection RETURNING VALUE(layout) TYPE slis_vari,
      pbo.

  PRIVATE SECTION.
    TYPES:
      t_icon TYPE c LENGTH 4,
      BEGIN OF t_report,
        released                  TYPE t_icon,
        main_transport            TYPE trkorr,
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
        create_date_main          TYPE as4date,
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
        transport                 TYPE string VALUE 'TRANSPORT',
        main_transport            TYPE string VALUE 'MAIN_TRANSPORT',
        released                  TYPE string VALUE 'RELEASED',
        creation_date             TYPE string VALUE 'CREATION_DATE',
        create_date_main          TYPE string VALUE 'CREATE_DATE_MAIN',
        toc_number                TYPE string VALUE 'TOC_NUMBER',
        type                      TYPE string VALUE 'TYPE',
      END OF c_toc_columns,
      BEGIN OF c_status_color,
        green  TYPE i VALUE 5,
        yellow TYPE i VALUE 3,
        red    TYPE i VALUE 6,
      END OF c_status_color.

    DATA:
      alv_table             TYPE REF TO cl_salv_table,
      toc_manager           TYPE REF TO zcl_zabap_toc,
      layout_key            TYPE salv_s_layout_key,
      report_data           TYPE tt_report,
      max_wait_time_in_sec  TYPE i,
      ignore_version        TYPE abap_bool,
      include_subtransports TYPE abap_bool,
      main_container        TYPE REF TO cl_gui_custom_container,
      alv_container         TYPE REF TO cl_gui_container,
      sys_container         TYPE REF TO cl_gui_container,
      layout_name           TYPE slis_vari,
      systems_list          TYPE REF TO cl_salv_table,
      systems               TYPE zcl_zabap_toc_systemlist=>system_lines,
      selected_system       TYPE tmssysnam,
      alv_functions         TYPE REF TO cl_salv_functions_list.

    METHODS:
      set_column_hotspot_icon IMPORTING column TYPE lvc_fname RAISING cx_salv_error,
      set_fixed_column_text   IMPORTING column TYPE lvc_fname text TYPE scrtext_l raising cx_salv_error,
      set_status_color IMPORTING row TYPE i color TYPE i,
      set_entry_color IMPORTING entry TYPE REF TO t_report color TYPE i,
      prepare_alv_table IMPORTING layout_name TYPE slis_vari RAISING cx_salv_error,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_added_function FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      show_transport_details IMPORTING transport TYPE trkorr,
      setup_sorts RAISING cx_salv_error,
      hide_main_transport raising cx_salv_error,
      on_systems_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row,
      prepare_systems_list raising cx_salv_error,
      add_function IMPORTING code TYPE ui_func icon TYPE icon_l2 text TYPE text132 RAISING cx_salv_error.

ENDCLASS.

CLASS zcl_zabap_toc_report IMPLEMENTATION.
  METHOD constructor.
    me->ignore_version = ignore_version.
    layout_key = VALUE salv_s_layout_key( report = report_id ).
    toc_manager = NEW #( NEW zcl_zabap_toc_description( zcl_zabap_toc_description=>c_toc_description-toc ) ).
  ENDMETHOD.

  METHOD display.
    me->layout_name = layout_name.
    CALL FUNCTION 'Z_ABAP_TOC_DISPLAY'
      EXPORTING
        controller = me.
  ENDMETHOD.

  METHOD gather_transports.
    me->include_subtransports = include_subtransports.
    SELECT FROM e070
                LEFT JOIN e07t ON e07t~trkorr = e070~trkorr
                LEFT JOIN e070 AS sup ON sup~trkorr = e070~strkorr
      FIELDS
        CASE WHEN e070~trstatus = 'L' OR e070~trstatus = 'D' THEN @icon_status_open ELSE @icon_status_booked END AS released,
        CASE WHEN sup~trkorr IS NULL THEN e070~trkorr ELSE sup~trkorr END AS main_transport,
        e070~trkorr AS transport, e070~trfunction AS type, e070~as4user AS owner, e070~as4date AS creation_date,
        CASE WHEN e070~tarsystem <> @space THEN e070~tarsystem ELSE sup~tarsystem END AS target_system,
        e07t~as4text AS description,
        @c_icon-create AS create_toc, @c_icon-create_release AS create_release_toc, @c_icon-create_release_import AS create_release_import_toc,
        CASE WHEN sup~trkorr IS NULL THEN e070~as4date ELSE sup~as4date END AS create_date_main
      WHERE e070~trkorr IN @tranports AND e070~as4user IN @owners AND as4text IN @descriptions
        AND ( @include_subtransports = @abap_true OR e070~strkorr = @space )
        AND ( @include_released = @abap_true OR e070~trstatus IN ( 'L', 'D' ) OR sup~trstatus IN ( 'L', 'D' ) )
        AND ( @include_tocs = @abap_true OR e070~trfunction <> 'T' )
      ORDER BY e070~trkorr DESCENDING, e070~as4date DESCENDING
      INTO CORRESPONDING FIELDS OF TABLE @report_data.

    DELETE ADJACENT DUPLICATES FROM report_data COMPARING transport.
  ENDMETHOD.

  METHOD get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key = layout_key restrict = if_salv_c_layout=>restrict_none )-layout.
  ENDMETHOD.

  METHOD on_added_function.
    DATA selected_row TYPE REF TO i.

    CASE e_salv_function.
      WHEN 'TOC_C'.
        LOOP AT alv_table->get_selections( )->get_selected_rows( ) REFERENCE INTO selected_row.
          on_link_click( row = selected_row->* column = CONV #( c_toc_columns-create_toc ) ).
        ENDLOOP.

      WHEN 'TOC_CR'.
        LOOP AT alv_table->get_selections( )->get_selected_rows( ) REFERENCE INTO selected_row.
          on_link_click( row = selected_row->* column = CONV #( c_toc_columns-create_release_toc ) ).
        ENDLOOP.

      WHEN 'TOC_CRI'.
        LOOP AT alv_table->get_selections( )->get_selected_rows( ) REFERENCE INTO selected_row.
          on_link_click( row = selected_row->* column = CONV #( c_toc_columns-create_release_import_toc ) ).
        ENDLOOP.

      WHEN 'STMS'.
        CALL TRANSACTION 'STMS' WITH AUTHORITY-CHECK.

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    DATA(selected) = REF #( report_data[ row ] ).

    CASE column.
      WHEN c_toc_columns-transport.
        show_transport_details( selected->transport ).
      WHEN c_toc_columns-toc_number.
        IF selected->toc_number IS NOT INITIAL.
          show_transport_details( selected->toc_number ).
        ENDIF.
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
    DATA(selected) = REF #( report_data[ row ] ).
    CLEAR selected->color.

    DATA(target_system) = COND #(
      WHEN selected_system IS INITIAL THEN selected->target_system ELSE selected_system ).

    TRY.
        CASE column.
            "--------------------------------------------------
          WHEN c_toc_columns-create_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = target_system
                                                        source_description = CONV #( selected->description ) ).
            selected->toc_status = TEXT-s01.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          WHEN c_toc_columns-create_release_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = target_system
                                                        source_description = CONV #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            selected->toc_status = TEXT-s02.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          WHEN c_toc_columns-create_release_import_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = target_system
                                                        source_description = CONV #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            DATA(rc) = CONV i( toc_manager->import( toc = selected->toc_number target_system = target_system
                  max_wait_time_in_sec = max_wait_time_in_sec ignore_version = ignore_version ) ).
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

      CATCH zcx_zabap_user_cancel INTO DATA(user_canceled). " TODO: variable is assigned but never used (ABAP cleaner)
        selected->toc_status = TEXT-e01.
        set_status_color( row = row color = c_status_color-red ).

    ENDTRY.

    alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.

  METHOD prepare_alv_table.
    cl_salv_table=>factory(
      EXPORTING
        r_container = alv_container
      IMPORTING
        r_salv_table = alv_table
      CHANGING
        t_table = report_data ).

    " Set columns as icons
    set_column_hotspot_icon( CONV #( c_toc_columns-create_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_toc ) ).
    set_column_hotspot_icon( CONV #( c_toc_columns-create_release_import_toc ) ).

    " Set column texts
    set_fixed_column_text( column = CONV #( c_toc_columns-create_toc ) text = CONV #( TEXT-c01 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_toc ) text = CONV #( TEXT-c02 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-create_release_import_toc ) text = CONV #(  TEXT-c03 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-main_transport ) text = CONV #(  TEXT-c06 ) ).
    set_fixed_column_text( column = CONV #( c_toc_columns-released ) text = CONV #(  TEXT-c07 ) ).
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

    alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    alv_functions = alv_table->get_functions( ).

    add_function( code = 'TOC_C' icon = icon_transport text = 'Create ToCs'(008) ).
    add_function( code = 'TOC_CR' icon = icon_transport text = 'Create/Release ToCs'(009) ).
    add_function( code = 'TOC_CRI' icon = icon_transport text = 'Create/Release/Import ToCs'(010) ).
    add_function( code = 'STMS' icon = icon_transport text = 'STMS' ).

    hide_main_transport( ).

    setup_sorts( ).
  ENDMETHOD.

  METHOD add_function.

    alv_functions->add_function(
          EXPORTING
            name     = code
            icon     = CONV #( icon )
            text     = CONV #( text )
            tooltip  = CONV #( text )
            position = if_salv_c_function_position=>right_of_salv_functions ).

  ENDMETHOD.



  METHOD hide_main_transport.
    IF include_subtransports = abap_false.
      alv_table->get_columns( )->get_column( |{ c_toc_columns-main_transport }| )->set_technical( ).
    ENDIF.
  ENDMETHOD.

  METHOD setup_sorts.
    alv_table->get_sorts( )->add_sort(
          columnname = |{ c_toc_columns-create_date_main }|
          position = 1
          sequence = if_salv_c_sort=>sort_down ).
    alv_table->get_sorts( )->add_sort(
      columnname = |{ c_toc_columns-main_transport }|
      position = 2
      sequence = if_salv_c_sort=>sort_up ).
    alv_table->get_sorts( )->add_sort(
          columnname = |{ c_toc_columns-type }|
          position = 3
          sequence = if_salv_c_sort=>sort_up ).
    alv_table->get_sorts( )->add_sort(
      columnname = |{ c_toc_columns-creation_date }|
      position = 4
      sequence = if_salv_c_sort=>sort_down ).
  ENDMETHOD.

  METHOD set_column_hotspot_icon.
    DATA(col) = CAST cl_salv_column_table( me->alv_table->get_columns( )->get_column( column ) ).
    col->set_icon( if_salv_c_bool_sap=>true ).
    col->set_cell_type( if_salv_c_cell_type=>hotspot ).
  ENDMETHOD.

  METHOD set_entry_color.
    CLEAR entry->color.
    APPEND VALUE #( fname = 'TOC_STATUS' color = VALUE #( col = color ) ) TO entry->color.
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

  METHOD set_max_wait_time.
    me->max_wait_time_in_sec = seconds.
  ENDMETHOD.

  METHOD set_status_color.
    DATA(color_cell) = REF #( report_data[ row ]-color ).
    CLEAR color_cell->*.
    APPEND VALUE #( fname = 'TOC_STATUS' color = VALUE #( col = color ) ) TO color_cell->*.
  ENDMETHOD.

  METHOD set_toc_description.
    toc_manager = NEW #( toc_description ).
  ENDMETHOD.

  METHOD show_transport_details.
    DATA batch_input TYPE STANDARD TABLE OF bdcdata WITH EMPTY KEY.

    APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=TSSN' ) TO batch_input.
    APPEND VALUE #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_SUBSCR' fval = 'RDDM0001                                0210COMMONSUBSCREEN' ) TO batch_input.
    APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'TRDYSE01SN-TR_TRKORR' ) TO batch_input.
    APPEND VALUE #( fnam = 'TRDYSE01SN-TR_TRKORR' fval = transport ) TO batch_input.

    DATA(call_options) = VALUE ctu_params( dismode = 'E' updmode  = 'A' nobinpt = abap_true nobiend = abap_true ).
    CALL TRANSACTION 'SE01' WITH AUTHORITY-CHECK USING batch_input OPTIONS FROM call_options.
  ENDMETHOD.

  METHOD on_systems_click.
    LOOP AT systems INTO DATA(line).
      IF sy-tabix = row.
        line-colors = VALUE #( ( color = VALUE #( col = col_positive ) ) ).
        selected_system = line-sysnam.
      ELSE.
        CLEAR line-colors.
      ENDIF.
      MODIFY systems FROM line.
    ENDLOOP.
    systems_list->refresh( ).
  ENDMETHOD.

  METHOD pbo.
    TRY.
        IF main_container IS NOT BOUND.
          main_container = NEW #( 'MAIN_AREA' ).
          DATA(split) = NEW cl_gui_splitter_container(
             parent  = main_container
             rows    = 2
             columns = 1 ).
          alv_container = split->get_container( row = 1 column = 1 ).
          sys_container = split->get_container( row = 2 column = 1 ).
          prepare_alv_table( layout_name ).
          alv_table->display( ).
          prepare_systems_list( ).
          systems_list->display( ).
          selected_system = space.
        ENDIF.
      CATCH cx_salv_error INTO DATA(e).
        MESSAGE e TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD prepare_systems_list.
    systems = zcl_zabap_toc_systemlist=>get( ).
    cl_salv_table=>factory(
      EXPORTING
        r_container    = sys_container
      IMPORTING
        r_salv_table   = systems_list
      CHANGING
        t_table        = systems
    ).
    systems_list->get_display_settings( )->set_list_header( 'Select a destination system'(011) ).
    systems_list->get_display_settings( )->set_list_header_size( cl_salv_display_settings=>c_header_size_small ).
    systems_list->get_columns( )->set_color_column( 'COLORS' ).
    CAST cl_salv_column_table( systems_list->get_columns( )->get_column( 'SYSTXT' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
    SET HANDLER on_systems_click FOR systems_list->get_event( ).

    systems_list->display( ).

  ENDMETHOD.

ENDCLASS.
