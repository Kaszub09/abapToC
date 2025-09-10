FUNCTION z_abap_toc_display.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CONTROLLER) TYPE REF TO  ZCL_ZABAP_TOC_REPORT
*"----------------------------------------------------------------------
  .

  g_controller = controller.

  CALL SCREEN 1.


ENDFUNCTION.
