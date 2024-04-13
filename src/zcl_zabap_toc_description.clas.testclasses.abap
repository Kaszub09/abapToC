*"* use this source file for your ABAP unit test classes
CLASS ltcl_toc_description DEFINITION DEFERRED.
CLASS zcl_zabap_toc_description DEFINITION LOCAL FRIENDS ltcl_toc_description.
CLASS ltcl_toc_description DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      increment_counter FOR TESTING,
      counter_description FOR TESTING,
      custom_description FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_zabap_toc_description.
ENDCLASS.


CLASS ltcl_toc_description IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD increment_counter.
    cut->selected_description = zcl_zabap_toc_description=>c_toc_description-custom.
    cut->descriptions = VALUE #( ( transport = space ask_user = abap_false counter = 2 description = 'TEST' ) ).

    cl_abap_unit_assert=>assert_char_cp( act = cut->get_toc_description( space ) exp = '*3' ).
  ENDMETHOD.

  METHOD counter_description.

  ENDMETHOD.

  METHOD custom_description.

  ENDMETHOD.

ENDCLASS.
