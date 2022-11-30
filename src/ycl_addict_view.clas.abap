CLASS ycl_addict_view DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA def TYPE dd25l READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING !viewname     TYPE dd25l-viewname
      RETURNING VALUE(result) TYPE REF TO ycl_addict_view
      RAISING   ycx_addict_table_content.

    METHODS get_child_views RETURNING VALUE(result) TYPE viewnames.
    METHODS get_parent_views RETURNING VALUE(result) TYPE viewnames.
    METHODS get_view_family RETURNING VALUE(result) TYPE viewnames.

    METHODS get_child_tables RETURNING VALUE(result) TYPE tr_tabnames.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             viewname TYPE dd25l-viewname,
             obj      TYPE REF TO ycl_addict_view,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict WITH UNIQUE KEY primary_key COMPONENTS viewname.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'DD25L',
               END OF table.

    DATA: child_views        TYPE viewnames,
          child_views_built  TYPE abap_bool,
          parent_views       TYPE viewnames,
          parent_views_built TYPE abap_bool,
          view_family        TYPE viewnames,
          view_family_built  TYPE abap_bool,
          child_tables       TYPE tr_tabnames,
          child_tables_built TYPE abap_bool.

    CLASS-DATA multitons TYPE multiton_set.

    METHODS constructor
      IMPORTING !viewname TYPE dd25l-viewname
      RAISING   ycx_addict_table_content.
ENDCLASS.



CLASS YCL_ADDICT_VIEW IMPLEMENTATION.


  METHOD constructor.
    SELECT SINGLE * FROM dd25l
           WHERE viewname = @viewname
           INTO CORRESPONDING FIELDS OF @me->def.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_addict_table_content
        EXPORTING
          textid   = ycx_addict_table_content=>no_entry_for_objectid
          objectid = CONV #( viewname )
          tabname  = me->table-def.
    ENDIF.
  ENDMETHOD.


  METHOD get_child_tables.
    IF me->child_tables_built = abap_false.
      DATA(tabclass_rng) = ycl_addict_table=>get_data_storage_tabclass_rng( ).

      SELECT DISTINCT dd26s~tabname
             FROM dd26s
                  INNER JOIN dd02l ON dd02l~tabname = dd26s~tabname
             WHERE dd26s~viewname =   @me->def-viewname AND
                   dd02l~tabclass IN  @tabclass_rng
             INTO TABLE @me->child_tables.

      SORT me->child_tables BY table_line.
      DELETE ADJACENT DUPLICATES FROM me->child_tables COMPARING table_line.

      me->child_tables_built = abap_true.
    ENDIF.

    result = me->child_tables.
  ENDMETHOD.


  METHOD get_child_views.
    IF me->child_views_built = abap_false.
      SELECT DISTINCT tabname
             FROM dd26s
                  INNER JOIN dd25l ON dd25l~viewname = dd26s~tabname
             WHERE dd26s~viewname = @me->def-viewname
             INTO TABLE @me->child_views.

      SORT me->child_views BY table_line.
      DELETE ADJACENT DUPLICATES FROM me->child_views COMPARING table_line.

      me->child_views_built = abap_true.
    ENDIF.

    result = me->child_views.
  ENDMETHOD.


  METHOD get_instance.
    DATA(mts) = REF #( ycl_addict_view=>multitons ).

    ASSIGN mts->*[ KEY primary_key COMPONENTS
                   viewname = viewname
                 ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      INSERT VALUE #( viewname  = viewname
                      obj       = NEW #( viewname ) )
             INTO TABLE mts->* ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.


  METHOD get_parent_views.
    IF me->parent_views_built = abap_false.
      SELECT DISTINCT viewname
             FROM dd26s
             WHERE tabname = @me->def-viewname
             INTO TABLE @me->parent_views.

      SORT me->parent_views BY table_line.
      DELETE ADJACENT DUPLICATES FROM me->parent_views COMPARING table_line.

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
