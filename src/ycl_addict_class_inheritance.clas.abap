CLASS ycl_addict_class_inheritance DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance RETURNING VALUE(result) TYPE REF TO ycl_addict_class_inheritance.

    METHODS get_immediate_subclasses
      IMPORTING !parent       TYPE seoclsname
      RETURNING VALUE(result) TYPE ycl_addict_class=>clsname_list.

    METHODS get_recursive_subclasses
      IMPORTING !parent       TYPE seoclsname
      RETURNING VALUE(result) TYPE ycl_addict_class=>clsname_list.

    METHODS get_instanceable_subclasses
      IMPORTING !parent       TYPE seoclsname
      RETURNING VALUE(result) TYPE ycl_addict_class=>clsname_list.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF cache_dict,
             clsname                   TYPE seoclsname,
             immediate_subclasses_read TYPE abap_bool,
             immediate_subclasses      TYPE ycl_addict_class=>clsname_list,
             recur_subcnam_read        TYPE abap_bool,
             recur_subcnam             TYPE ycl_addict_class=>clsname_list,
             insta_subcnam_read        TYPE abap_bool,
             insta_subcnam             TYPE ycl_addict_class=>clsname_list,
           END OF cache_dict,

           cache_set TYPE HASHED TABLE OF cache_dict
                     WITH UNIQUE KEY primary_key COMPONENTS clsname.

    CLASS-DATA singleton TYPE REF TO ycl_addict_class_inheritance.
    DATA cache TYPE cache_set.

    METHODS get_cache
      IMPORTING !clsname      TYPE seoclsname
      RETURNING VALUE(result) TYPE REF TO cache_dict.

    METHODS get_recursive_subclass_names_p
      IMPORTING !refclsname TYPE seoclsname
                !rec        TYPE abap_bool
      CHANGING  !result     TYPE ycl_addict_class=>clsname_list.
ENDCLASS.



CLASS YCL_ADDICT_CLASS_INHERITANCE IMPLEMENTATION.


  METHOD get_cache.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Creates / reads cache entry
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        result = REF #( me->cache[ KEY primary_key COMPONENTS
                                   clsname = clsname ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( clsname = clsname )
               INTO TABLE me->cache
               REFERENCE INTO result.
    ENDTRY.
  ENDMETHOD.


  METHOD get_immediate_subclasses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns immediate subclasses
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cache) = get_cache( parent ).

    IF cache->immediate_subclasses_read = abap_false.
      SELECT clsname FROM seometarel                    "#EC CI_GENBUFF
             WHERE refclsname = @cache->clsname AND    "#EC CI_BUFFSUBQ
                   ( NOT EXISTS ( SELECT clsname FROM ytaddict_class
                                  WHERE clsname = seometarel~clsname AND
                                        disable_inherit = @abap_true ) )
             INTO TABLE @cache->immediate_subclasses.

      cache->immediate_subclasses_read = abap_true.
    ENDIF.

    result = cache->immediate_subclasses.
  ENDMETHOD.


  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Singleton design pattern
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    result = ycl_addict_class_inheritance=>singleton.

    IF result IS INITIAL.
      result = NEW #( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_instanceable_subclasses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns subclasses which are instanceable
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cache) = get_cache( parent ).

    IF cache->insta_subcnam_read = abap_false.
      cache->insta_subcnam_read = abap_true.
      cache->insta_subcnam = get_recursive_subclasses( parent ).

      IF cache->insta_subcnam IS INITIAL.
        RETURN.
      ENDIF.

      DATA(clsname_rng) = VALUE ycl_addict_class=>clsname_range(
                                  FOR cn IN cache->insta_subcnam
                                  ( option = ycl_addict_toolkit=>option-eq
                                    sign   = ycl_addict_toolkit=>sign-include
                                    low    = cn ) ).

      SELECT clsname FROM seoclassdf AS sd1
             WHERE clsname IN @clsname_rng AND
                   version > 0 AND
                   version = ( SELECT MAX( version )
                               FROM seoclassdf AS sd2
                               WHERE clsname = sd1~clsname ) AND
                   clsabstrct = @abap_true
             ORDER BY clsname
             INTO TABLE @data(abstract).               "#EC CI_BUFFSUBQ

      LOOP AT cache->insta_subcnam ASSIGNING FIELD-SYMBOL(<clsname>).
        READ TABLE abstract TRANSPORTING NO FIELDS
             WITH KEY clsname = <clsname>
             BINARY SEARCH.

        CHECK sy-subrc = 0.
        DELETE cache->insta_subcnam.
        CONTINUE.
      ENDLOOP.
    ENDIF.

    result = cache->insta_subcnam.
  ENDMETHOD.


  METHOD get_recursive_subclasses.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns subclass names in a recursive manner
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(cache) = get_cache( parent ).

    IF cache->recur_subcnam_read = abap_false.
      get_recursive_subclass_names_p(
        EXPORTING refclsname = parent
                  rec        = abap_false
        CHANGING  result     = cache->recur_subcnam ).

      cache->recur_subcnam_read = abap_true.
    ENDIF.

    result = cache->recur_subcnam.
  ENDMETHOD.


  METHOD get_recursive_subclass_names_p.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Recursion helper method
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(clsname_local) = get_immediate_subclasses( refclsname ).
      CATCH cx_root ##no_handler .
        RETURN.
    ENDTRY.

    LOOP AT clsname_local ASSIGNING FIELD-SYMBOL(<cl>).
      APPEND <cl> TO result.

      get_recursive_subclass_names_p(
        EXPORTING refclsname = <cl>
                  rec        = abap_true
        CHANGING  result     = result ).
    ENDLOOP.

    IF rec = abap_true.
      RETURN.
    ENDIF.

    SORT result BY table_line.
    DELETE ADJACENT DUPLICATES FROM result COMPARING table_line.
  ENDMETHOD.
ENDCLASS.
