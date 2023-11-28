*&---------------------------------------------------------------------*
*& Report ztoc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztoc.

" -----------------------------------------------------------------------
TABLES e070.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS so_trnum FOR e070-trkorr. " Transport numbers
  SELECT-OPTIONS so_owner FOR e070-as4user. " Transport owners
  PARAMETERS p_reltr AS CHECKBOX. " Include released transports
  PARAMETERS p_tocs AS CHECKBOX. " Include ToCs
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

" -----------------------------------------------------------------------
INITIALIZATION.
  DATA(report) = NEW zcl_zabap_toc_report( report_id = sy-repid ).

  " -----------------------------------------------------------------------
START-OF-SELECTION.
  report->gather_transports( tranports = so_trnum[] owners = so_owner[] include_released = p_reltr include_tocs = p_tocs ).
  report->display( p_layout ).

  " -----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = report->get_layout_from_f4_selection( ).
