*&---------------------------------------------------------------------*
*& Report ztoc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report ztoc.

" -----------------------------------------------------------------------
tables: e070, e07t.

selection-screen begin of block b01 with frame title text-b01.
select-options so_trnum for e070-trkorr. " Transport numbers
select-options so_owner for e070-as4user default sy-uname. " Transport owners
select-options so_descr for e07t-as4text.
parameters p_reltr as checkbox. " Include released transports
parameters p_tocs as checkbox. " Include ToCs
parameters p_sub as checkbox. " Include subtransports
selection-screen end of block b01.

selection-screen begin of block b02 with frame title text-b02.
parameters p_destoc radiobutton group desc default 'X'.
parameters p_desori radiobutton group desc.
parameters p_descus radiobutton group desc.
selection-screen end of block b02.

selection-screen begin of block b03 with frame title text-b03.
parameters p_maxwai type i default '30'.
parameters vignore as checkbox default abap_true.
" -----------------------------------------------------------------------
selection-screen end of block b03.

selection-screen begin of block b04 with frame title text-b04.
parameters p_layout type disvariant-variant.
selection-screen end of block b04.


initialization.
  data(report) =
    new zcl_zabap_toc_report(
      report_id = sy-repid
      ignore_version = vignore ).

  " -----------------------------------------------------------------------

start-of-selection.
  report->set_max_wait_time( p_maxwai ).
  report->set_toc_description( toc_description = new #( cond #( when p_destoc = abap_true then zcl_zabap_toc_description=>c_toc_description-toc
      when p_desori = abap_true then zcl_zabap_toc_description=>c_toc_description-original
      when p_descus = abap_true then zcl_zabap_toc_description=>c_toc_description-custom ) ) ).
  report->gather_transports( tranports = so_trnum[] owners = so_owner[] descriptions = so_descr[]
                             include_released = p_reltr include_tocs = p_tocs include_subtransports = p_sub ).
  try.
      report->display( p_layout ).
    catch cx_salv_error into data(e).
      message e type 'I' display like 'E'.
  endtry.

  " -----------------------------------------------------------------------

at selection-screen on value-request for p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
