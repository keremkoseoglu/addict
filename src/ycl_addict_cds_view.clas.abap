CLASS ycl_addict_cds_view DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES ddlname_list TYPE STANDARD TABLE OF ddlname WITH KEY table_line.

    DATA def TYPE ddddlsrc READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !ddlname      TYPE ddddlsrc-ddlname
      RETURNING VALUE(result) TYPE REF TO ycl_addict_cds_view
      RAISING   ycx_addict_table_content.

    METHODS get_child_views RETURNING VALUE(result) TYPE ddlname_list.
    METHODS get_parent_views RETURNING VALUE(result) TYPE ddlname_list.
    METHODS get_view_family RETURNING VALUE(result) TYPE ddlname_list.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             ddlname TYPE ddddlsrc-ddlname,
             obj     TYPE REF TO ycl_addict_cds_view,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS ddlname.

    TYPES objectname_list TYPE STANDARD TABLE OF ddldependency-objectname WITH KEY table_line.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'DDDDLSRC',
               END OF table.

    CONSTANTS: BEGIN OF objecttype,
                 view TYPE ddldependency-objecttype VALUE 'VIEW',
               END OF objecttype.

    DATA: child_views        TYPE ddlname_list,
          child_views_built  TYPE abap_bool,
          parent_views       TYPE ddlname_list,
          parent_views_built TYPE abap_bool,
          view_family        TYPE ddlname_list,
          view_family_built  TYPE abap_bool,
          child_tables       TYPE tr_tabnames,
          child_tables_built TYPE abap_bool.

    CLASS-DATA multitons TYPE multiton_set.

    METHODS constructor
      IMPORTING !ddlname TYPE ddddlsrc-ddlname
      RAISING   ycx_addict_table_content.
ENDCLASS.



CLASS YCL_ADDICT_CDS_VIEW IMPLEMENTATION.


  METHOD constructor.
    SELECT SINGLE * FROM ddddlsrc
           WHERE ddlname = @ddlname
           INTO CORRESPONDING FIELDS OF @me->def.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_addict_table_content
        EXPORTING
          textid   = ycx_addict_table_content=>no_entry_for_objectid
          objectid = CONV #( ddlname )
          tabname  = me->table-def.
    ENDIF.
  ENDMETHOD.


  METHOD get_child_views.
    DATA objectnames TYPE objectname_list.

    IF me->child_views_built = abap_false.
      SELECT DISTINCT objectname FROM ddldependency
             WHERE ddlname    = @me->def-ddlname AND
                   objecttype = @me->objecttype-view
             INTO TABLE @DATA(views).

      LOOP AT views ASSIGNING FIELD-SYMBOL(<view>).
        TRY.
            DATA(view) = ycl_addict_view=>get_instance( CONV #( <view> ) ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

        DATA(view_children) = view->get_child_views( ).
        objectnames         = view_children.
        CHECK objectnames IS NOT INITIAL.

        SELECT DISTINCT ddlname FROM ddldependency
               FOR ALL ENTRIES IN @objectnames
               WHERE objectname = @objectnames-table_line AND
                     objecttype = @me->objecttype-view
               INTO TABLE @DATA(ddlnames).

        DELETE ddlnames WHERE table_line = me->def-ddlname.
        SORT ddlnames BY table_line.
        DELETE ADJACENT DUPLICATES FROM ddlnames COMPARING table_line.

        APPEND LINES OF ddlnames TO me->child_views.
      ENDLOOP.

      me->child_views_built = abap_true.
    ENDIF.

    result = me->child_views.
  ENDMETHOD.


  METHOD get_instance.
    DATA(mts) = REF #( ycl_addict_cds_view=>multitons ).

    ASSIGN mts->*[ KEY primary_key COMPONENTS
                   ddlname = ddlname
                 ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( ddlname  = ddlname
                      obj       = NEW #( ddlname ) )
             INTO TABLE mts->* ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.


  METHOD get_parent_views.
    DATA objectnames TYPE objectname_list.

    IF me->parent_views_built = abap_false.
      SELECT DISTINCT objectname FROM ddldependency
             WHERE ddlname    = @me->def-ddlname AND
                   objecttype = @me->objecttype-view
             INTO TABLE @DATA(views).

      LOOP AT views ASSIGNING FIELD-SYMBOL(<view>).
        TRY.
            DATA(view) = ycl_addict_view=>get_instance( CONV #( <view> ) ).
          CATCH cx_root.
            CONTINUE.
        ENDTRY.

        DATA(view_parents) = view->get_parent_views( ).
        objectnames         = view_parents.
        CHECK objectnames IS NOT INITIAL.

        SELECT DISTINCT ddlname FROM ddldependency
               FOR ALL ENTRIES IN @objectnames
               WHERE objectname = @objectnames-table_line AND
                     objecttype = @me->objecttype-view
               INTO TABLE @DATA(ddlnames).

        DELETE ddlnames WHERE table_line = me->def-ddlname.
        SORT ddlnames BY table_line.
        DELETE ADJACENT DUPLICATES FROM ddlnames COMPARING table_line.

        APPEND LINES OF ddlnames TO me->parent_views.
      ENDLOOP.

      me->parent_views_built = abap_true.
    ENDIF.

    result = me->parent_views.
  ENDMETHOD.


  METHOD get_view_family.
    IF me->view_family_built = abap_false.
      DATA(children) = get_child_views( ).
      DATA(parents)  = get_parent_views( ).

      APPEND LINES OF:  children TO me->view_family,
                        parents  TO me->view_family.

      SORT view_family BY table_line.
      DELETE ADJACENT DUPLICATES FROM view_family COMPARING table_line.

      me->view_family_built = abap_true.
    ENDIF.

    result = me->view_family.
  ENDMETHOD.
ENDCLASS.
