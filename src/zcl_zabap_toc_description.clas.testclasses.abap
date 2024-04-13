*"* use this source file for your ABAP unit test classes
CLASS ltcl_toc_description DEFINITION DEFERRED.
CLASS zcl_zabap_toc_description DEFINITION LOCAL FRIENDS ltcl_toc_description.
CLASS ltcl_toc_description DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      increment_counter FOR TESTING,
      counter_description FOR TESTING,
      custom_description FOR TESTING,
      original_description FOR TESTING,
      toc_description FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_zabap_toc_description.
ENDCLASS.


CLASS ltcl_toc_description IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD increment_counter.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-custom.
    cut->descriptions = VALUE #( ( transport = space ask_user = abap_false counter = 2  ) ).
    cut->get_toc_description( space ).

    cl_abap_unit_assert=>assert_equals( act = cut->descriptions[ transport = space ]-counter exp = 3 ).
  ENDMETHOD.

  METHOD counter_description.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-custom.
    cut->descriptions = VALUE #( ( transport = space ask_user = abap_false use_counter = abap_true counter = 2 description = 'TEST' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = cut->get_toc_description( space ) exp = '*3*' ).
  ENDMETHOD.

  METHOD custom_description.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-custom.
    cut->descriptions = VALUE #( ( transport = space ask_user = abap_false use_counter = abap_false description = 'Custom' ) ).

    cl_abap_unit_assert=>assert_equals( act = cut->get_toc_description( space ) exp = 'Custom' ).
  ENDMETHOD.

  METHOD original_description.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-original.
    cut->descriptions = VALUE #( ( transport = space ask_user = abap_false description = 'ABC' ) ).

    cl_abap_unit_assert=>assert_equals( act = cut->get_toc_description( original_transport = space original_desciption = 'TEST' ) exp = 'TEST' ).
  ENDMETHOD.

  METHOD toc_description.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-toc.
    cut->descriptions = VALUE #( ( transport = 'TR1234' ask_user = abap_false ) ).

    cl_abap_unit_assert=>assert_char_cp( act = cut->get_toc_description( 'TR1234' ) exp = replace( val = TEXT-001 sub = '&1' with = 'TR1234' ) ).
  ENDMETHOD.

ENDCLASS.
