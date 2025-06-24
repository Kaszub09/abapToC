CLASS zcl_zabap_toc_description DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_toc_description,
        toc      TYPE i VALUE 0,
        original TYPE i VALUE 1,
        custom   TYPE i VALUE 2,
      END OF c_toc_description.

    METHODS:
      constructor IMPORTING selected_description TYPE i DEFAULT c_toc_description-toc,
      get_toc_description IMPORTING original_transport TYPE trkorr original_desciption TYPE string DEFAULT space
                          RETURNING VALUE(toc_description) TYPE string
                          RAISING zcx_zabap_user_cancel.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_description,
        transport   TYPE trkorr,
        description TYPE string,
        use_counter TYPE abap_bool,
        counter     TYPE i,
        ask_user    TYPE abap_bool,
      END OF t_description,
      tt_description TYPE HASHED TABLE OF t_description WITH UNIQUE KEY transport.

    DATA:
      selected_description TYPE i,
      descriptions         TYPE tt_description.

    METHODS:
      ask_user_for_description CHANGING description TYPE REF TO t_description RAISING zcx_zabap_user_cancel.
ENDCLASS.



CLASS zcl_zabap_toc_description IMPLEMENTATION.
  METHOD constructor.
    me->selected_description = selected_description.
  ENDMETHOD.

  METHOD get_toc_description.
    CASE selected_description.
      WHEN c_toc_description-toc. toc_description = replace( val = TEXT-001 sub = '&1' with = original_transport ).

      WHEN c_toc_description-original. toc_description = original_desciption.

      WHEN c_toc_description-custom.
        DATA(description) = REF #( descriptions[ transport = original_transport ] OPTIONAL ).
        IF NOT description IS BOUND.
          INSERT VALUE #( transport = original_transport description = original_desciption use_counter = abap_false counter = 0
                          ask_user = abap_true ) INTO TABLE descriptions REFERENCE INTO description.
        ENDIF.

        description->counter = description->counter + 1.

        IF description->ask_user = abap_true.
          ask_user_for_description( CHANGING description = description ).
        ENDIF.

        IF description->use_counter = abap_true.
          toc_description = condense( replace( val = replace( val = TEXT-002 sub = '&1' with = description->description )
                                     sub = '&2' with = CONV string( description->counter ) ) ).
        ELSE.
          toc_description = description->description.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD ask_user_for_description.
    DATA user_confirmed TYPE abap_bool.

    CALL FUNCTION 'ZABAP_TOC_ASK_USER_DESCRIPTION'
      IMPORTING
        user_confirmed = user_confirmed
      CHANGING
        description    = description->description
        ask_user       = description->ask_user
        use_counter    = description->use_counter
        counter        = description->counter.

    IF user_confirmed = abap_false.
      RAISE EXCEPTION TYPE zcx_zabap_user_cancel.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
