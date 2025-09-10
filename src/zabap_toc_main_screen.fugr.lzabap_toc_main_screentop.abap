FUNCTION-POOL zabap_toc_main_screen.        "MESSAGE-ID ..


DATA g_controller TYPE REF TO zcl_zabap_toc_report.
DATA okcode TYPE sy-ucomm.
* INCLUDE LZABAP_TOC_MAIN_SCREEND...         " Local class definition
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'MAIN'.
  g_controller->pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  IF okcode = 'EXIT'.
    SET SCREEN 0.
  ENDIF.
ENDMODULE.
