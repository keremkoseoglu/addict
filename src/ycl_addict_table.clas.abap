CLASS ycl_addict_table DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES dd03l_list TYPE STANDARD TABLE OF dd03l WITH EMPTY KEY.
    TYPES tabname_list TYPE STANDARD TABLE OF tabname WITH EMPTY KEY.
    TYPES tabname_range TYPE RANGE OF tabname.
    TYPES tabclass_range TYPE RANGE OF dd02l-tabclass.

    TYPES: BEGIN OF tabfld_dict,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             rollname  TYPE dd03l-rollname,
           END OF tabfld_dict,

           tabfld_list TYPE STANDARD TABLE OF tabfld_dict WITH EMPTY KEY,
           tabfld_sort TYPE SORTED TABLE OF tabfld_dict WITH UNIQUE KEY primary_key COMPONENTS fieldname.

    TYPES: BEGIN OF fldroll_dict,
             fieldname TYPE dd03l-fieldname,
             rollname  TYPE dd03l-rollname,
           END OF fldroll_dict,

           fldroll_list TYPE STANDARD TABLE OF fldroll_dict WITH EMPTY KEY.

    CONSTANTS: BEGIN OF tabclass,
                 transparent TYPE dd02l-tabclass VALUE 'TRANSP',
                 cluster     TYPE dd02l-tabclass VALUE 'CLUSTER',
                 pooled      TYPE dd02l-tabclass VALUE 'POOL',
               END OF tabclass.

    DATA def TYPE dd02l READ-ONLY.

    CLASS-METHODS get_dbfield_text
      IMPORTING !dbfield      TYPE clike
      RETURNING VALUE(output) TYPE ddtext.

    CLASS-METHODS get_instance
      IMPORTING !tabname      TYPE tabname
      RETURNING VALUE(output) TYPE REF TO ycl_addict_table
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_rollname_pairs
      IMPORTING !tabname1     TYPE tabname
                !tabname2     TYPE tabname
      RETURNING VALUE(output) TYPE ytt_addict_rollname_pair
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_tables_containing_dtel
      IMPORTING !rollname     TYPE rollname
      RETURNING VALUE(output) TYPE tabname_list.

    CLASS-METHODS get_tables_containing_fldroll
      IMPORTING !tabname_rng  TYPE tabname_range
                !fldroll      TYPE fldroll_list
      RETURNING VALUE(output) TYPE tabname_list.

    CLASS-METHODS get_data_storage_tabclass_rng
      RETURNING VALUE(result) TYPE tabclass_range.

    METHODS check_table_has_flds_of_tab
      IMPORTING !tabname TYPE tabname
      RAISING   ycx_addict_table_content.

    METHODS check_table_has_field
      IMPORTING !fieldname TYPE fieldname
      RAISING   ycx_addict_table_content.

    METHODS enqueue
      IMPORTING !key TYPE clike OPTIONAL
      RAISING   cx_rs_foreign_lock.

    METHODS get_field
      IMPORTING !fnam         TYPE fieldname
      RETURNING VALUE(output) TYPE dd03l
      RAISING   ycx_addict_table_content.

    METHODS get_field_count RETURNING VALUE(output) TYPE i.

    METHODS get_fields RETURNING VALUE(output) TYPE dd03l_list.

    METHODS get_included_tables
      IMPORTING !recursive    TYPE abap_bool
      RETURNING VALUE(output) TYPE tabname_list.

    METHODS get_key_fields
      IMPORTING !with_mandt   TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(output) TYPE dd03l_list.

    METHODS get_rollname_of_field
      IMPORTING !fnam         TYPE fieldname
      RETURNING VALUE(output) TYPE rollname
      RAISING   ycx_addict_table_content.

    METHODS is_field_key
      IMPORTING !fnam         TYPE fieldname
      RETURNING VALUE(output) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES rollname_list TYPE STANDARD TABLE OF rollname WITH EMPTY KEY.

    TYPES: BEGIN OF dtel_tab_dict,
             rollname TYPE rollname,
             tabname  TYPE tabname_list,
           END OF dtel_tab_dict,

           dtel_tab_set TYPE HASHED TABLE OF dtel_tab_dict
                        WITH UNIQUE KEY primary_key COMPONENTS rollname.

    TYPES: BEGIN OF lazy_flag_dict,
             field   TYPE abap_bool,
             include TYPE abap_bool,
             key     TYPE abap_bool,
           END OF lazy_flag_dict.

    TYPES: BEGIN OF lazy_val_dict,
             field   TYPE dd03l_list,
             include TYPE tabname_list,
             key     TYPE dd03l_list,
           END OF lazy_val_dict.

    TYPES: BEGIN OF lazy_dict,
             flag TYPE lazy_flag_dict,
             val  TYPE lazy_val_dict,
           END OF lazy_dict.

    TYPES: BEGIN OF mt_dict, " Multiton
             tabname TYPE dd02l-tabname,
             obj     TYPE REF TO ycl_addict_table,
           END OF mt_dict,

           mt_set TYPE HASHED TABLE OF mt_dict
                  WITH UNIQUE KEY primary_key COMPONENTS tabname.


    CONSTANTS: BEGIN OF table,
                 def TYPE tabname VALUE 'DD02L',
                 fld TYPE tabname VALUE 'DD03L',
               END OF table.

    CONSTANTS: BEGIN OF fnam,
                 mandt TYPE fieldname VALUE 'MANDT',
               END OF fnam.


    CLASS-DATA dtel_tabs TYPE dtel_tab_set.
    CLASS-DATA mts TYPE mt_set.

    DATA lazy TYPE lazy_dict.

    METHODS read_fields_lazy.
    METHODS read_includes_lazy.
    METHODS read_keys_lazy.
ENDCLASS.



CLASS YCL_ADDICT_TABLE IMPLEMENTATION.


  METHOD check_table_has_field.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ensures that the table has the given field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_fields_lazy( ).

    IF NOT line_exists( me->lazy-val-field[ fieldname = fieldname ] ).
      RAISE EXCEPTION TYPE ycx_addict_table_content
        EXPORTING
          objectid = |{ me->def-tabname } - { fieldname }|
          tabname  = me->def-tabname
          textid   = ycx_addict_table_content=>no_entry_for_objectid.
    ENDIF.
  ENDMETHOD.


  METHOD check_table_has_flds_of_tab.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ensures that the table has the given fields
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT get_instance( tabname )->get_fields( ) ASSIGNING FIELD-SYMBOL(<fld>).
      check_table_has_field( <fld>-fieldname ).
    ENDLOOP.
  ENDMETHOD.


  METHOD enqueue.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Enqueue table against editing
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        view_name            = me->def-tabname
      EXCEPTIONS
        client_reference     = 1
        foreign_lock         = 2
        invalid_action       = 3
        invalid_enqueue_mode = 4
        system_failure       = 5
        table_not_found      = 6
        OTHERS               = 7.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_rs_foreign_lock
        EXPORTING
          key    = CONV #( key )
          object = CONV #( me->def-tabname )
          textid = cx_rs_foreign_lock=>cx_rs_foreign_lock
          user   = CONV #( sy-msgv1 ).
    ENDIF.
  ENDMETHOD.


  METHOD get_data_storage_tabclass_rng.
    result = VALUE #( sign    = ycl_addict_toolkit=>sign-include
                      option  = ycl_addict_toolkit=>option-eq
                      ( low   = tabclass-transparent )
                      ( low   = tabclass-cluster )
                      ( low   = tabclass-pooled ) ).
  ENDMETHOD.


  METHOD get_dbfield_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns text of the given database field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = ycl_addict_dbfield_text_abs=>get_text_via_chain( dbfield ).
  ENDMETHOD.


  METHOD get_field.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the requested field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_fields_lazy( ).

    TRY.
        output = me->lazy-val-field[ fieldname = fnam ].
      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = |{ me->def-tabname } { fnam }|
            previous = diaper
            tabname  = me->table-fld.
    ENDTRY.
  ENDMETHOD.


  METHOD get_fields.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns all fields of table
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_fields_lazy( ).
    output = me->lazy-val-field.
  ENDMETHOD.


  METHOD get_field_count.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns number of fields
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_fields_lazy( ).
    output = lines( me->lazy-val-field ).
  ENDMETHOD.


  METHOD get_included_tables.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns included tables
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_includes_lazy( ).
    DATA(returnables) = me->lazy-val-include.

    IF recursive = abap_true.
      LOOP AT me->lazy-val-include ASSIGNING FIELD-SYMBOL(<include>).
        TRY.
            DATA(tab) = ycl_addict_table=>get_instance( <include> ).
          CATCH cx_root. " Paranoya
            DELETE returnables WHERE table_line = <include>.
            CONTINUE.
        ENDTRY.

        APPEND LINES OF tab->get_included_tables( abap_true ) TO returnables.
      ENDLOOP.

      SORT returnables BY table_line.
      DELETE ADJACENT DUPLICATES FROM returnables COMPARING table_line.
    ENDIF.

    output = returnables.
  ENDMETHOD.


  METHOD get_instance.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_table=>mts[ KEY primary_key COMPONENTS tabname = tabname ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      DATA(mt) = VALUE mt_dict( tabname = tabname ).
      mt-obj = NEW #( ).

      SELECT SINGLE * FROM dd02l
             WHERE tabname = @mt-tabname
             INTO @mt-obj->def.                         "#EC CI_NOORDER

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = CONV #( mt-tabname )
            tabname  = ycl_addict_table=>table-def.
      ENDIF.

      INSERT mt INTO TABLE ycl_addict_table=>mts ASSIGNING <mt>.
    ENDIF.

    output = <mt>-obj.
  ENDMETHOD.


  METHOD get_key_fields.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns key fields of table
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_keys_lazy( ).
    output = me->lazy-val-key.

    IF with_mandt = abap_false.
      DELETE output WHERE fieldname = me->fnam-mandt.
    ENDIF.
  ENDMETHOD.


  METHOD get_rollname_of_field.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns roll name of field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = get_field( fnam )-rollname.
  ENDMETHOD.


  METHOD get_rollname_pairs.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns rollname pairs
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA dd04t TYPE STANDARD TABLE OF dd04t.
    DATA roll  TYPE rollname_list.
    DATA tab1  TYPE tabfld_sort.
    DATA tab2  TYPE tabfld_sort.
    DATA ret   LIKE LINE OF output.

    tab1 = CORRESPONDING #( get_instance( tabname1 )->get_fields( ) ).
    tab2 = CORRESPONDING #( get_instance( tabname2 )->get_fields( ) ).

    LOOP AT tab1 ASSIGNING FIELD-SYMBOL(<tab1>) WHERE rollname IS NOT INITIAL. "#EC CI_SORTSEQ
      APPEND <tab1>-rollname TO roll.
    ENDLOOP.

    LOOP AT tab2 ASSIGNING FIELD-SYMBOL(<tab2>) WHERE rollname IS NOT INITIAL. "#EC CI_SORTSEQ
      APPEND <tab2>-rollname TO roll.
    ENDLOOP.

    SORT roll BY table_line.
    DELETE ADJACENT DUPLICATES FROM roll COMPARING table_line.

    IF roll IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM dd04t
           FOR ALL ENTRIES IN @roll
           WHERE rollname   = @roll-table_line AND
                 ddlanguage = @sy-langu
           INTO TABLE @dd04t.

    SORT dd04t BY rollname.

    LOOP AT roll ASSIGNING FIELD-SYMBOL(<roll>).
      CLEAR ret.
      ret-rollname = <roll>.

      READ TABLE dd04t ASSIGNING FIELD-SYMBOL(<dd04t>)
           WITH KEY rollname = <roll>
           BINARY SEARCH.

      IF sy-subrc = 0.
        ret-ddtext = <dd04t>-ddtext.
      ENDIF.

      LOOP AT tab1 ASSIGNING <tab1>
           USING KEY primary_key
           WHERE rollname = <roll>.                     "#EC CI_SORTSEQ

        ret-tabname1   = <tab1>-tabname.
        ret-fieldname1 = <tab1>-fieldname.

        LOOP AT tab2 ASSIGNING <tab2>
             USING KEY primary_key
             WHERE rollname = <roll>.                   "#EC CI_SORTSEQ

          ret-tabname2   = <tab2>-tabname.
          ret-fieldname2 = <tab2>-fieldname.
          APPEND ret TO output.

        ENDLOOP.

        IF sy-subrc <> 0.
          APPEND ret TO output.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_tables_containing_dtel.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of tables containing the given data element
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_table=>dtel_tabs[
             rollname = rollname
          ] TO FIELD-SYMBOL(<dtel_tab>).

    IF sy-subrc <> 0.
      DATA(new) = VALUE dtel_tab_dict( rollname = rollname ).

      SELECT DISTINCT dd03l~tabname
             FROM dd03l
                  INNER JOIN dd02l ON dd02l~tabname = dd03l~tabname
             WHERE rollname = @new-rollname AND
                   dd02l~tabclass <> 'INTTAB' AND
                   dd02l~tabclass <> 'VIEW' AND
                   dd02l~tabclass <> 'APPEND'
             INTO TABLE @new-tabname.

      SORT new-tabname BY table_line.
      DELETE ADJACENT DUPLICATES FROM new-tabname COMPARING table_line.
      INSERT new INTO TABLE ycl_addict_table=>dtel_tabs ASSIGNING <dtel_tab>.
    ENDIF.

    output = <dtel_tab>-tabname.
  ENDMETHOD.


  METHOD get_tables_containing_fldroll.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns tables containing the fieldname and rollname
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK fldroll IS NOT INITIAL.

    SELECT tabname, fieldname, rollname
           FROM dd03l
           FOR ALL ENTRIES IN @fldroll
           WHERE tabname   IN @tabname_rng      AND
                 fieldname = @fldroll-fieldname AND
                 rollname  = @fldroll-rollname
          INTO TABLE @DATA(dd03l).

    output = VALUE #(
        FOR GROUPS _tabname OF _dd03l IN dd03l
        GROUP BY _dd03l-tabname
        ( _tabname ) ).

    SORT dd03l BY tabname fieldname rollname. " Binary Search var

    LOOP AT output ASSIGNING FIELD-SYMBOL(<tabname>).
      DATA(table_ok) = abap_true.

      LOOP AT fldroll ASSIGNING FIELD-SYMBOL(<fldroll>).
        READ TABLE dd03l TRANSPORTING NO FIELDS
             WITH KEY tabname   = <tabname>
                      fieldname = <fldroll>-fieldname
                      rollname  = <fldroll>-rollname
             BINARY SEARCH.

        CHECK sy-subrc <> 0.
        table_ok = abap_false.
        EXIT.
      ENDLOOP.

      CHECK table_ok = abap_false.
      DELETE output.
      CONTINUE.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_field_key.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns TRUE if the given field is a key field
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_keys_lazy( ).

    output = xsdbool( line_exists(
        me->lazy-val-key[ fieldname = fnam ] ) ).
  ENDMETHOD.


  METHOD read_fields_lazy.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy read fields
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->lazy-flag-field IS INITIAL.

    SELECT * FROM dd03l
           WHERE tabname = @me->def-tabname AND
                 fieldname NOT LIKE '.%'
           ORDER BY position
           INTO TABLE @me->lazy-val-field.

    me->lazy-flag-field = abap_true.
  ENDMETHOD.


  METHOD read_includes_lazy.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy read includes
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->lazy-flag-include IS INITIAL.

    SELECT precfield AS tabname
           FROM dd03l
           WHERE tabname   = @me->def-tabname AND
                 fieldname = '.INCLUDE'
           INTO TABLE @me->lazy-val-include.

    me->lazy-flag-include = abap_true.
  ENDMETHOD.


  METHOD read_keys_lazy.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Lazy read table keys
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->lazy-flag-key IS INITIAL.

    read_fields_lazy( ).

    me->lazy-val-key = VALUE #(
        FOR field IN me->lazy-val-field
        WHERE ( keyflag = abap_true )
        ( field ) ).

    me->lazy-flag-key = abap_true.
  ENDMETHOD.
ENDCLASS.
