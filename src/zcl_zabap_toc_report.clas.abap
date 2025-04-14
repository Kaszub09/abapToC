class zcl_zabap_toc_report definition public final create public.

  public section.
    types:
      tt_range_of_transport   type range of trkorr,
      tt_range_of_owner       type range of tr_as4user,
      tt_range_of_description type range of as4text.

    methods:
      constructor
        importing
            report_id type sy-repid
            ignore_version type abap_bool,
      set_max_wait_time importing seconds type i,
      set_toc_description importing toc_description type ref to zcl_zabap_toc_description,
      gather_transports importing tranports type tt_range_of_transport optional owners type tt_range_of_owner optional
                        descriptions type tt_range_of_description optional
                        include_released type abap_bool default abap_true include_tocs type abap_bool default abap_false
                        include_subtransports type abap_bool default abap_false,
      display importing layout_name type slis_vari optional raising cx_salv_error,
      get_layout_from_f4_selection returning value(layout) type slis_vari.

  protected section.
  private section.
    types:
      t_icon type c length 4,
      begin of t_report,
        released                  type t_icon,
        main_transport            type trkorr,
        transport                 type trkorr,
        type                      type trfunction,
        target_system             type tr_target,
        owner                     type tr_as4user,
        creation_date             type as4date,
        description               type as4text,
        create_toc                type t_icon,
        create_release_toc        type t_icon,
        create_release_import_toc type t_icon,
        toc_number                type trkorr,
        toc_status                type string,
        create_date_main          type as4date,
        color                     type lvc_t_scol,
      end of t_report,
      tt_report type standard table of t_report with key transport with non-unique sorted key toc components toc_number.

    constants:
      begin of c_icon,
        create                type t_icon value '@EZ@',
        create_release        type t_icon value '@4A@',
        create_release_import type t_icon value '@K5@',
      end of c_icon,
      begin of c_toc_columns,
        create_toc                type string value 'CREATE_TOC',
        create_release_toc        type string value 'CREATE_RELEASE_TOC',
        create_release_import_toc type string value 'CREATE_RELEASE_IMPORT_TOC',
        transport                 type string value 'TRANSPORT',
        main_transport            type string value 'MAIN_TRANSPORT',
        released                  type string value 'RELEASED',
        creation_date             type string value 'CREATION_DATE',
        create_date_main          type string value 'CREATE_DATE_MAIN',
        toc_number                type string value 'TOC_NUMBER',
      end of c_toc_columns,
      begin of c_status_color,
        green  type i value 5,
        yellow type i value 3,
        red    type i value 6,
      end of c_status_color.

    data:
      alv_table            type ref to cl_salv_table,
      toc_manager          type ref to zcl_zabap_toc,
      layout_key           type salv_s_layout_key,
      report_data          type tt_report,
      max_wait_time_in_sec type i.
    data ignore_version type abap_bool.
    data include_subtransports type abap_bool.

    methods:
      set_column_hotspot_icon importing column type lvc_fname raising cx_salv_error,
      set_fixed_column_text   importing column type lvc_fname text type scrtext_l raising cx_salv_error,
      set_status_color importing row type i color type i,
      set_entry_color importing entry type ref to t_report color type i,
      prepare_alv_table importing layout_name type slis_vari optional raising cx_salv_error,
      on_link_click for event link_click of cl_salv_events_table importing row column,
      on_double_click for event double_click of cl_salv_events_table importing row column,
      on_added_function for event added_function of cl_salv_events importing e_salv_function ,
      show_transport_details importing transport type trkorr,
      provide_main_transport
        importing
          lines         type tt_report
        returning
          value(result) type tt_report,
      set_sort_date
        importing
          lines         type tt_report
        returning
          value(result) type tt_report,
      setup_sorts
        raising
          cx_salv_error,
      hide_main_transport
        raising
          cx_salv_not_found.

endclass.



class zcl_zabap_toc_report implementation.


  method constructor.
    me->ignore_version = ignore_version.
    layout_key = value salv_s_layout_key( report = report_id ).
    toc_manager = new #( new zcl_zabap_toc_description( zcl_zabap_toc_description=>c_toc_description-toc ) ).
  endmethod.


  method display.
    prepare_alv_table( layout_name ).
    alv_table->display( ).
  endmethod.


  method gather_transports.
    me->include_subtransports = include_subtransports.
    select from e070
                left join e07t on e07t~trkorr = e070~trkorr
                left join e070 as sup on sup~trkorr = e070~strkorr
      fields
        case
          when e070~trstatus = 'L' or e070~trstatus = 'D'  then @icon_status_open
          else @icon_status_booked
        end as released,
        sup~trkorr as main_transport, e070~trkorr as transport, e070~trfunction as type, e070~as4user as owner, e070~as4date as creation_date,
        case when e070~tarsystem <> @space then e070~tarsystem else sup~tarsystem end as target_system,
        e07t~as4text as description,
        @c_icon-create as create_toc, @c_icon-create_release as create_release_toc, @c_icon-create_release_import as create_release_import_toc
      where e070~trkorr in @tranports and e070~as4user in @owners and as4text in @descriptions
        and ( @include_subtransports = @abap_true or e070~strkorr     = @space )
        and ( @include_released      = @abap_true or e070~trstatus   in ( 'L', 'D' ) or sup~trstatus in ( 'L', 'D' ) )
        and ( @include_tocs          = @abap_true or e070~trfunction <> 'T' )
      order by e070~trkorr descending, e070~as4date descending
      into corresponding fields of table @report_data.


    delete adjacent duplicates from report_data comparing transport.

    report_data =
      provide_main_transport(
        set_sort_date( report_data ) ).


  endmethod.

  method provide_main_transport.
    result =
      value #(
        for line in lines
        ( value #(
            base line
            main_transport =
              cond #(
                when line-main_transport is initial
                then line-transport
                else line-main_transport ) ) ) ).
  endmethod.




  method get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key = layout_key restrict = if_salv_c_layout=>restrict_none )-layout.
  endmethod.


  method on_added_function.
    data selected_row type ref to i.

    case e_salv_function.
      when 'TOC_C'.
        loop at alv_table->get_selections( )->get_selected_rows( ) reference into selected_row.
          on_link_click( row = selected_row->* column = conv #( c_toc_columns-create_toc ) ).
        endloop.

      when 'TOC_CR'.
        loop at alv_table->get_selections( )->get_selected_rows( ) reference into selected_row.
          on_link_click( row = selected_row->* column = conv #( c_toc_columns-create_release_toc ) ).
        endloop.

      when 'TOC_CRI'.
        loop at alv_table->get_selections( )->get_selected_rows( ) reference into selected_row.
          on_link_click( row = selected_row->* column = conv #( c_toc_columns-create_release_import_toc ) ).
        endloop.

      when 'STMS'.
        call transaction 'STMS'.

      when others.

    endcase.
  endmethod.


  method on_double_click.
    data(selected) = ref #( report_data[ row ] ).

    case column.
      when c_toc_columns-transport.
        show_transport_details( selected->transport ).
      when c_toc_columns-toc_number.
        show_transport_details( selected->toc_number ).


      when others.

    endcase.
  endmethod.


  method on_link_click.
    data(selected) = ref #( report_data[ row ] ).
    clear selected->color.

    try.
        case column.
            "--------------------------------------------------
          when c_toc_columns-create_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = conv #( selected->description ) ).
            selected->toc_status = text-s01.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          when c_toc_columns-create_release_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = conv #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            selected->toc_status = text-s02.
            set_status_color( row = row color = c_status_color-green ).

            "--------------------------------------------------
          when c_toc_columns-create_release_import_toc.
            selected->toc_number = toc_manager->create( source_transport = selected->transport target_system = selected->target_system
                                                        source_description = conv #( selected->description ) ).
            toc_manager->release( selected->toc_number ).
            data(rc) =
              conv i(
                toc_manager->import(
                  toc = selected->toc_number
                  target_system = selected->target_system
                  max_wait_time_in_sec = max_wait_time_in_sec
                  ignore_version = ignore_version ) ).
            selected->toc_status = text-s03.
            selected->toc_status = replace( val = text-s04 sub = '&1' with = |{ rc }| ).
            set_status_color( row = row color = cond #( when rc = 0 then c_status_color-green
                                                        when rc = 4 then c_status_color-yellow
                                                        else             c_status_color-red ) ).

            "--------------------------------------------------
          when others.
        endcase.

      catch zcx_zabap_exception into data(exception).
        selected->toc_status = exception->get_text( ).
        set_status_color( row = row color = c_status_color-red ).

      catch zcx_zabap_user_cancel into data(user_canceled).
        selected->toc_status = text-e01.
        set_status_color( row = row color = c_status_color-red ).

    endtry.

    alv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  endmethod.


  method prepare_alv_table.
    cl_salv_table=>factory( importing r_salv_table = alv_table changing  t_table = report_data ).

    " Set columns as icons
    set_column_hotspot_icon( conv #( c_toc_columns-create_toc ) ).
    set_column_hotspot_icon( conv #( c_toc_columns-create_release_toc ) ).
    set_column_hotspot_icon( conv #( c_toc_columns-create_release_import_toc ) ).

    " Set column texts
    set_fixed_column_text( column = conv #( c_toc_columns-create_toc ) text = conv #( text-c01 ) ).
    set_fixed_column_text( column = conv #( c_toc_columns-create_release_toc ) text = conv #( text-c02 ) ).
    set_fixed_column_text( column = conv #( c_toc_columns-create_release_import_toc ) text = conv #(  text-c03 ) ).
    set_fixed_column_text( column = conv #( c_toc_columns-main_transport ) text = conv #(  text-c06 ) ).
    set_fixed_column_text( column = conv #( c_toc_columns-released ) text = conv #(  text-c07 ) ).
    set_fixed_column_text( column = 'TOC_NUMBER' text =  conv #( text-c04 ) ).
    set_fixed_column_text( column = 'TOC_STATUS' text =  conv #( text-c05 ) ).

    " Set handlers
    data(event) = alv_table->get_event( ).
    set handler me->on_link_click for event.
    set handler me->on_double_click for event.
    set handler me->on_added_function for event.

    " Set layouts
    alv_table->get_layout( )->set_key( layout_key ).
    alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
    alv_table->get_layout( )->set_default( abap_true ).
    if layout_name is not initial.
      alv_table->get_layout( )->set_initial_layout( layout_name ).
    endif.

    " Enable standard report functions
    alv_table->get_functions( )->set_all( ).

    " Color
    alv_table->get_columns( )->set_color_column( 'COLOR' ).


    alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    alv_table->set_screen_status( report = 'ZTOC' pfstatus = 'MAIN' ).

    hide_main_transport( ).

    setup_sorts( ).
  endmethod.

  method hide_main_transport.
    if include_subtransports = abap_false.
      alv_table->get_columns( )->get_column( |{ c_toc_columns-main_transport }| )->set_technical( ).
    endif.
  endmethod.

  method setup_sorts.
    alv_table->get_sorts( )->add_sort(
          columnname = |{ c_toc_columns-create_date_main }|
          position = 1
          sequence = if_salv_c_sort=>sort_down ).
    alv_table->get_sorts( )->add_sort(
      columnname = |{ c_toc_columns-main_transport }|
      position = 2
      sequence = if_salv_c_sort=>sort_up ).
    alv_table->get_sorts( )->add_sort(
      columnname = |{ c_toc_columns-creation_date }|
      position = 3
      sequence = if_salv_c_sort=>sort_down ).
  endmethod.


  method set_column_hotspot_icon.
    data(col) = cast cl_salv_column_table( me->alv_table->get_columns( )->get_column( column ) ).
    col->set_icon( if_salv_c_bool_sap=>true ).
    col->set_cell_type( if_salv_c_cell_type=>hotspot ).
  endmethod.


  method set_entry_color.
    clear entry->color.
    append value #( fname = 'TOC_STATUS' color = value #( col = color ) ) to entry->color.
  endmethod.


  method set_fixed_column_text.
    data(col) = alv_table->get_columns( )->get_column( column ).
    if strlen( text ) > 20.
      col->set_long_text( text ).
      col->set_fixed_header_text( 'L' ).
    elseif strlen( text ) > 10.
      col->set_long_text( text ).
      col->set_medium_text( conv #( text ) ).
      col->set_fixed_header_text( 'M' ).
    else.
      col->set_long_text( text ).
      col->set_medium_text( conv #( text ) ).
      col->set_short_text( conv #( text ) ).
      col->set_fixed_header_text( 'S' ).
    endif.
  endmethod.


  method set_max_wait_time.
    me->max_wait_time_in_sec = seconds.
  endmethod.


  method set_status_color.
    data(color_cell) = ref #( report_data[ row ]-color ).
    clear color_cell->*.
    append value #( fname = 'TOC_STATUS' color = value #( col = color ) ) to color_cell->*.
  endmethod.


  method set_toc_description.
    toc_manager = new #( toc_description ).
  endmethod.


  method show_transport_details.
    data batch_input type table of bdcdata.

    append value #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) to batch_input.
    append value #( fnam = 'BDC_OKCODE' fval = '=TSSN' ) to batch_input.
    append value #( program = 'RDDM0001' dynpro = '0200' dynbegin = 'X'  ) to batch_input.
    append value #( fnam = 'BDC_SUBSCR' fval = 'RDDM0001                                0210COMMONSUBSCREEN' ) to batch_input.
    append value #( fnam = 'BDC_CURSOR' fval = 'TRDYSE01SN-TR_TRKORR' ) to batch_input.
    append value #( fnam = 'TRDYSE01SN-TR_TRKORR' fval = transport ) to batch_input.

    data(call_options) = value ctu_params( dismode = 'E' updmode  = 'A' nobinpt = abap_true nobiend = abap_true ).
    call transaction 'SE01' using batch_input options from call_options.
  endmethod.

  method set_sort_date.
    result =
      value #(
        for line in lines
        ( value #(
            base line
            create_date_main =
              cond #(
                when line-main_transport is initial
                then line-creation_date
                else value #(
                  lines[ transport = line-main_transport ]-creation_date optional ) ) ) ) ).
  endmethod.

endclass.
