CLASS ycl_addict_class DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES clsname_list TYPE STANDARD TABLE OF seoclsname WITH EMPTY KEY.
    TYPES dok_text_list TYPE STANDARD TABLE OF dok_text WITH EMPTY KEY.

    TYPES clsname_range TYPE RANGE OF seoclsname.
    TYPES cmpname_range TYPE RANGE OF seocompo-cmpname.
    TYPES cmptype_range TYPE RANGE OF seocompo-cmptype.
    TYPES mtdtype_range TYPE RANGE OF seocompo-mtdtype.

    TYPES: BEGIN OF component_param_dict,
             cmpname_rng TYPE cmpname_range,
             cmptype_rng TYPE cmptype_range,
             mtdtype_rng TYPE mtdtype_range,
           END OF component_param_dict.

    TYPES: BEGIN OF component_dict,
             cmpname TYPE seocmpname,
             cmptype TYPE seocmptype,
             mtdtype TYPE seomtdtype,
           END OF component_dict,

           component_set  TYPE HASHED TABLE OF component_dict
                         WITH UNIQUE KEY primary_key COMPONENTS cmpname,

           component_sort TYPE SORTED TABLE OF component_dict
                          WITH UNIQUE KEY primary_key COMPONENTS cmpname,

           component_list TYPE STANDARD TABLE OF component_dict WITH EMPTY KEY.

    CONSTANTS: BEGIN OF cmptype,
                 method TYPE seocmptype VALUE '1',
               END OF cmptype.

    CONSTANTS: BEGIN OF method,
                 constructor TYPE seocpdname VALUE 'CONSTRUCTOR',
               END OF method.

    DATA def TYPE seoclass READ-ONLY.

    CLASS-METHODS check_class_existence
      IMPORTING !clsname TYPE seoclsname
      RAISING   ycx_addict_table_content.

    CLASS-METHODS check_class_has_interface
      IMPORTING !class     TYPE seoclsname
                !interface TYPE seoclsname
      RAISING   ycx_addict_table_content.

    CLASS-METHODS convert_prgname_to_clsname
      IMPORTING !prgname       TYPE clike
      RETURNING VALUE(clsname) TYPE seoclsname.

    CLASS-METHODS get_class_name
      IMPORTING !obj          TYPE REF TO object
      RETURNING VALUE(result) TYPE seoclsname.

    CLASS-METHODS get_deepest_exception
      IMPORTING !cx           TYPE REF TO cx_root
      RETURNING VALUE(result) TYPE REF TO cx_root.

    CLASS-METHODS get_instance
      IMPORTING !clsname   TYPE seoclsname
      RETURNING VALUE(obj) TYPE REF TO ycl_addict_class
      RAISING   ycx_addict_table_content.

    METHODS accept
      IMPORTING !visitor TYPE REF TO yif_addict_class_visitor
      RAISING   ycx_addict_class_method.

    METHODS dequeue_exec.
    METHODS enqueue_exec RAISING ycx_addict_lock.

    METHODS get_components
      IMPORTING !param    TYPE component_param_dict OPTIONAL
      EXPORTING !cmp_hash TYPE component_set
                !cmp_sort TYPE component_sort
                !cmp_std  TYPE component_list.

    METHODS get_immediate_subclass_names RETURNING VALUE(output) TYPE clsname_list.
    METHODS get_instanceable_subclasses RETURNING VALUE(output) TYPE clsname_list.
    METHODS get_recursive_subclass_names RETURNING VALUE(output) TYPE clsname_list.
    METHODS get_text RETURNING VALUE(output) TYPE seoclasstx.
    METHODS is_in_call_stack RETURNING VALUE(stack) TYPE abap_bool.

    METHODS search_class_doc
      IMPORTING !words     TYPE dok_text_list
      RETURNING VALUE(hit) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             clsname TYPE seoclsname,
             obj     TYPE REF TO ycl_addict_class,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS clsname.

    CONSTANTS: BEGIN OF doku_id,
                 class TYPE dokil-id  VALUE 'CL',
               END OF doku_id.

    CONSTANTS: BEGIN OF doku_typ,
                 class TYPE dokil-typ VALUE 'E',
               END OF doku_typ.

    CONSTANTS: BEGIN OF table,
                 def TYPE tabname   VALUE 'SEOCLASS',
                 rel TYPE tabname   VALUE 'SEOMETAREL',
               END OF table.

    DATA dokil                 TYPE dokil.
    DATA txt                   TYPE seoclasstx.
    DATA component_std         TYPE component_list.
    DATA dokil_read            TYPE abap_bool.
    DATA txt_read              TYPE abap_bool.
    DATA component_read        TYPE abap_bool.

    CLASS-DATA multiton TYPE multiton_set.
    METHODS read_dokil.
ENDCLASS.



CLASS YCL_ADDICT_CLASS IMPLEMENTATION.


  METHOD accept.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Accepts a visitor
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    visitor->visit( me ).
  ENDMETHOD.


  METHOD check_class_existence.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ensures that the provided class exists
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    get_instance( clsname ).
  ENDMETHOD.


  METHOD check_class_has_interface.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ensures that the class has the given interface
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cls) = get_instance( interface )->get_instanceable_subclasses( ).
    CHECK NOT line_exists( cls[ table_line = class ] ).

    RAISE EXCEPTION TYPE ycx_addict_table_content
      EXPORTING
        objectid = CONV #( |{ class } { interface }| )
        tabname  = table-rel
        textid   = ycx_addict_table_content=>no_entry_for_objectid.
  ENDMETHOD.


  METHOD convert_prgname_to_clsname.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Converts program name to class name
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    clsname = prgname+0(30).
    REPLACE ALL OCCURRENCES OF '=' IN clsname WITH space.
  ENDMETHOD.


  METHOD dequeue_exec.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Dequeues class for execution
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'DEQUEUE_EYADDICT_CLSNAME'
      EXPORTING
        clsname = me->def-clsname.
  ENDMETHOD.


  METHOD enqueue_exec.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Enqueues class for execution
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'ENQUEUE_EYADDICT_CLSNAME'
      EXPORTING
        clsname        = me->def-clsname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3
        ##FM_SUBRC_OK.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_addict_lock
        EXPORTING
          textid = ycx_addict_lock=>locked_by_user
          bname  = CONV #( sy-msgv1 ).
    ENDIF.
  ENDMETHOD.


  METHOD get_class_name.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns class name of the given object
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK obj IS NOT INITIAL.
    DATA(long_class_name) = cl_abap_classdescr=>get_class_name( obj ).
    REPLACE ALL OCCURRENCES OF '\CLASS=' IN long_class_name WITH space.
    result = long_class_name.
  ENDMETHOD.


  METHOD get_components.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns components of class
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->component_read = abap_false. " Lazy initialization
      SELECT * FROM seocompo
             WHERE clsname = @me->def-clsname
             INTO CORRESPONDING FIELDS OF TABLE @me->component_std.

      me->component_read = abap_true.
    ENDIF.

    cmp_std = me->component_std.

    DELETE cmp_std WHERE NOT (
        cmpname IN param-cmpname_rng AND
        cmptype IN param-cmptype_rng AND
        mtdtype IN param-mtdtype_rng ).

    IF cmp_hash IS REQUESTED.
      cmp_hash = cmp_std.
    ENDIF.

    IF cmp_sort IS REQUESTED.
      cmp_sort = cmp_std.
    ENDIF.
  ENDMETHOD.


  METHOD get_deepest_exception.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the deepest exception object
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = COND #( WHEN cx->previous IS INITIAL
                     THEN cx
                     ELSE get_deepest_exception( cx->previous ) ).
  ENDMETHOD.


  METHOD get_immediate_subclass_names.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns immediate children classes
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = ycl_addict_class_inheritance=>get_instance( )->get_immediate_subclasses( def-clsname ).
  ENDMETHOD.


  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_class=>multiton[ KEY primary_key COMPONENTS
                                       clsname = clsname ]
           TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_dict( clsname = clsname ).
      multiton-obj = NEW #( ).

      SELECT SINGLE * FROM seoclass
             WHERE clsname = @multiton-clsname
             INTO @multiton-obj->def.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = CONV #( multiton-clsname )
            tabname  = ycl_addict_class=>table-def.
      ENDIF.

      INSERT multiton INTO TABLE ycl_addict_class=>multiton ASSIGNING <multiton>.
    ENDIF.

    obj = <multiton>-obj.
  ENDMETHOD.


  METHOD get_instanceable_subclasses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns subclasses which are instanceable
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = ycl_addict_class_inheritance=>get_instance( )->get_instanceable_subclasses( me->def-clsname ).
  ENDMETHOD.


  METHOD get_recursive_subclass_names.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns subclass names in a recursive manner
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = ycl_addict_class_inheritance=>get_instance( )->get_recursive_subclasses( me->def-clsname ).
  ENDMETHOD.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns text of class
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->txt_read = abap_false. " Lazy initialization

      SELECT SINGLE * FROM seoclasstx
             WHERE clsname = @me->def-clsname AND
                   langu   = @sy-langu
             INTO @me->txt.

      IF sy-subrc <> 0.
        SELECT SINGLE * FROM seoclasstx
               WHERE clsname = @me->def-clsname
               INTO @me->txt ##WARN_OK .                "#EC CI_NOORDER
      ENDIF.

      me->txt_read = abap_true.
    ENDIF.

    output = me->txt.
  ENDMETHOD.


  METHOD is_in_call_stack.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tells if the class is in the call stack
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA cs TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = cs.

    ASSERT cs IS NOT INITIAL. " Can't be
    DATA(progname_pattern) = |{ me->def-clsname }*|.

    LOOP AT cs TRANSPORTING NO FIELDS
         WHERE progname CP progname_pattern.

      stack = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_dokil.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Reads dokil table (lazy)
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->dokil_read = abap_false.
    me->dokil_read = abap_true.

    SELECT SINGLE * FROM dokil
           WHERE id     = @me->doku_id-class AND
                 object = @me->def-clsname AND
                 langu  = @sy-langu AND
                 typ    = @me->doku_typ-class
           INTO @me->dokil .

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM dokil                          "#EC CI_GENBUFF
           WHERE id     = @me->doku_id-class AND        "#EC CI_NOORDER
                 object = @me->def-clsname   AND
                 typ    = @me->doku_typ-class
           INTO @me->dokil ##WARN_OK.
  ENDMETHOD.


  METHOD search_class_doc.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Search class documentation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA hitlist TYPE STANDARD TABLE OF tline.

    CHECK words IS NOT INITIAL.
    read_dokil( ).

    IF me->dokil IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT words ASSIGNING FIELD-SYMBOL(<word>).
      CALL FUNCTION 'DOCU_SEARCH_TEXT'
        EXPORTING
          id           = me->dokil-id
          langu        = me->dokil-langu
          object       = me->dokil-object
          typ          = me->dokil-typ
          searchstring = <word>
        TABLES
          hitlist      = hitlist.

      CHECK hitlist IS NOT INITIAL.
      hit = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
