CLASS ycl_addict_tadir_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS does_object_exist_facade
      IMPORTING !key         TYPE ycl_addict_package=>tadir_key_dict
      RETURNING VALUE(exist) TYPE abap_bool
      RAISING   ycx_addict_function_subrc.

    METHODS does_object_exist
      IMPORTING !key         TYPE ycl_addict_package=>tadir_key_dict
      RETURNING VALUE(exist) TYPE abap_bool.

    METHODS read_tadir
      IMPORTING !sysnam          TYPE tmscsys-sysnam OPTIONAL
                !tadir_key       TYPE ycl_addict_package=>tadir_key_list
                !exclude_deleted TYPE abap_bool DEFAULT abap_false
      RAISING   ycx_addict_function_subrc.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF doef_cache_dict,
             tadir_key TYPE ycl_addict_package=>tadir_key_dict,
             exists    TYPE abap_bool,
           END OF doef_cache_dict,

           doef_cache_set TYPE HASHED TABLE OF doef_cache_dict
           WITH UNIQUE KEY primary_key COMPONENTS tadir_key.

    TYPES tadir_list TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.

    TYPES tadir_key_set TYPE HASHED TABLE OF ycl_addict_package=>tadir_key_dict
          WITH UNIQUE KEY primary_key COMPONENTS pgmid object obj_name.

    TYPES: BEGIN OF state_dict,
             tadir_keys TYPE tadir_key_set,
           END OF state_dict.

    CLASS-DATA doef_cache TYPE doef_cache_set.

    DATA state TYPE state_dict.
ENDCLASS.



CLASS YCL_ADDICT_TADIR_READER IMPLEMENTATION.


  METHOD does_object_exist.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Checks object existence in TADIR
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    exist = xsdbool( line_exists( me->state-tadir_keys[ KEY primary_key COMPONENTS
        pgmid    = key-pgmid
        object   = key-object
        obj_name = key-obj_name ] ) ).
  ENDMETHOD.


  METHOD does_object_exist_facade.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " FAÇADE entry point
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_tadir_reader=>doef_cache[
             KEY primary_key COMPONENTS
             tadir_key = key
           ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      DATA(cache) = VALUE doef_cache_dict( tadir_key = key ).
      DATA(reader) = NEW ycl_addict_tadir_reader( ).
      reader->read_tadir( VALUE #( ( key ) ) ).
      cache-exists = reader->does_object_exist( key ).
      INSERT cache INTO TABLE ycl_addict_tadir_reader=>doef_cache ASSIGNING <cache>.
    ENDIF.

    exist = <cache>-exists.
  ENDMETHOD.


  METHOD read_tadir.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Primary method, reads TADIR
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA dat     TYPE STANDARD TABLE OF tab512.
    DATA del_rng TYPE RANGE OF tadir-delflag.
    DATA fld     TYPE STANDARD TABLE OF rfc_db_fld.
    DATA opt     TYPE STANDARD TABLE OF rfc_db_opt.
    DATA tadir   TYPE tadir_list.

    " Prepare """""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->state = VALUE #( ).

    IF tadir_key IS INITIAL.
      RETURN.
    ENDIF.

    " Read """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Same system -> go to table
    " Different system -> call RFC
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF sysnam IS INITIAL.
      IF exclude_deleted = abap_true.
        del_rng = VALUE #( ( option = ycl_addict_toolkit=>option-eq
                             sign   = ycl_addict_toolkit=>sign-exclude
                             low    = abap_true ) ).
      ENDIF.

      SELECT pgmid, object, obj_name FROM tadir
             FOR ALL ENTRIES IN @tadir_key
             WHERE pgmid    EQ @tadir_key-pgmid    AND
                   object   EQ @tadir_key-object   AND
                   obj_name EQ @tadir_key-obj_name AND
                   delflag  IN @del_rng
              INTO CORRESPONDING FIELDS OF TABLE @me->state-tadir_keys.

    ELSE.
      LOOP AT tadir_key ASSIGNING FIELD-SYMBOL(<tadir_key>).

        IF opt IS NOT INITIAL.
          APPEND VALUE #( text = 'OR' ) TO opt.
        ENDIF.

        APPEND VALUE #( text = '(' ) TO opt.

        APPEND VALUE #( text = |PGMID = '{ <tadir_key>-pgmid }' AND| ) TO opt.
        APPEND VALUE #( text = |OBJECT = '{ <tadir_key>-object }' AND| ) TO opt.
        APPEND VALUE #( text = |OBJ_NAME = '{ <tadir_key>-obj_name }'| ) TO opt.

        IF exclude_deleted = abap_true.
          APPEND VALUE #( text = | AND ( DELFLAG = '' OR DELFLAG IS NULL )| ) TO opt.
        ENDIF.

        APPEND VALUE #( text = ')' ) TO opt.
      ENDLOOP.

      IF opt IS INITIAL.
        RETURN.
      ENDIF.

      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION sysnam
        EXPORTING
          query_table          = 'TADIR'
        TABLES
          options              = opt
          fields               = fld
          data                 = dat
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7 ##FM_SUBRC_OK.

      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'RFC_READ_TABLE' ).

      tadir = dat.
      state-tadir_keys = CORRESPONDING #( tadir ).
    ENDIF.

    IF 1 = 0. " Where Used List
      SELECT SINGLE pgmid FROM tadir INTO @data(dummy).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
