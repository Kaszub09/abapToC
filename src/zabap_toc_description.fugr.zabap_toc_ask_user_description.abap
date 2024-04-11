FUNCTION zabap_toc_ask_user_description.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(USER_CONFIRMED) TYPE  ABAP_BOOL
*"  CHANGING
*"     REFERENCE(DESCRIPTION) TYPE  STRING
*"     REFERENCE(ASK_USER) TYPE  ABAP_BOOL
*"     REFERENCE(USE_COUNTER) TYPE  ABAP_BOOL
*"     REFERENCE(COUNTER) TYPE  I
*"----------------------------------------------------------------------
  description_input = description.
  ask_user_checkbox = ask_user.
  use_counter_checkbox = use_counter.
  counter_input = counter.

  CALL SCREEN 1 STARTING AT 1 1.

  IF user_command = 'CONFIRM'.
    user_confirmed = abap_true.
    description = description_input.
    ask_user = ask_user_checkbox.
    use_counter = use_counter_checkbox.
    counter = counter_input.

  ELSE.
    user_confirmed = abap_false.

  ENDIF.
ENDFUNCTION.
