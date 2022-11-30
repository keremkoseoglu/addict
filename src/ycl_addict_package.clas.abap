CLASS ycl_addict_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES objname_range TYPE RANGE OF tadir-obj_name.
    TYPES package_range TYPE RANGE OF tdevc-devclass.

    TYPES: BEGIN OF tadir_key_dict,
             pgmid    TYPE tadir-pgmid,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
           END OF tadir_key_dict,

           tadir_key_list TYPE STANDARD TABLE OF tadir_key_dict WITH KEY pgmid object obj_name.

    TYPES: BEGIN OF pack_cache_dict,
             key      TYPE tadir_key_dict,
             devclass TYPE tadir-devclass,
           END OF pack_cache_dict,

           pack_cache_set TYPE HASHED TABLE OF pack_cache_dict
                          WITH UNIQUE KEY primary_key COMPONENTS key.

    CLASS-METHODS get_package_of_obj
      IMPORTING !key            TYPE tadir_key_dict
      RETURNING VALUE(devclass) TYPE tadir-devclass.

    CLASS-METHODS get_package_of_objects
      IMPORTING !key            TYPE tadir_key_list
      RETURNING VALUE(devclass) TYPE pack_cache_set.

    CLASS-METHODS get_nonsap_objname_rng RETURNING VALUE(rng) TYPE objname_range.
    CLASS-METHODS get_nonsap_package_rng RETURNING VALUE(rng) TYPE package_range.

    CLASS-METHODS is_obj_custom_development
      IMPORTING !key          TYPE tadir_key_dict
      RETURNING VALUE(nonsap) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES devclass_range TYPE RANGE OF tdevc-devclass.

    TYPES: BEGIN OF custom_dev_cache_dict,
             key    TYPE tadir_key_dict,
             custom TYPE abap_bool,
           END OF custom_dev_cache_dict,

           custom_dev_cache_set TYPE HASHED TABLE OF custom_dev_cache_dict
                           WITH UNIQUE KEY primary_key COMPONENTS key.

    CLASS-DATA custom_dev_cache   TYPE custom_dev_cache_set.
    CLASS-DATA nonsap_objname_rng TYPE objname_range.
    CLASS-DATA nonsap_pack_rng    TYPE package_range.
    CLASS-DATA pack_cache         TYPE pack_cache_set.
ENDCLASS.



CLASS YCL_ADDICT_PACKAGE IMPLEMENTATION.


  METHOD get_nonsap_objname_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns object name pattern for objects not provided by SAP
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(output) = REF #( ycl_addict_package=>nonsap_objname_rng ).

    IF output->* IS INITIAL.
      SELECT obj_sign   AS sign,                        "#EC CI_NOWHERE
             obj_option AS option,
             obj_low    AS low,
             obj_high   AS high
             FROM ytaddict_nsobj
             INTO CORRESPONDING FIELDS OF TABLE @output->*.

      IF output->* IS INITIAL.
        output->* = VALUE #(
            sign   = ycl_addict_toolkit=>sign-include
            option = ycl_addict_toolkit=>option-cp
             ( low = 'Y*' )
             ( low = 'Z*' ) ).
      ENDIF.
    ENDIF.

    rng = output->*.
  ENDMETHOD.


  METHOD get_nonsap_package_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns packages which are not provided by SAP
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ycl_addict_package=>nonsap_pack_rng IS INITIAL.
      DATA(devclass_rng) = CORRESPONDING devclass_range(
          get_nonsap_objname_rng( ) ).

      SELECT @ycl_addict_toolkit=>option-eq AS option,  "#EC CI_GENBUFF
             @ycl_addict_toolkit=>sign-include AS sign,
             devclass AS low
             FROM tdevc
             WHERE devclass IN @devclass_rng
             INTO CORRESPONDING FIELDS OF TABLE
             @ycl_addict_package=>nonsap_pack_rng ##too_many_itab_fields.
    ENDIF.

    rng = ycl_addict_package=>nonsap_pack_rng.
  ENDMETHOD.


  METHOD get_package_of_obj.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the package of the given object
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(ret) = get_package_of_objects( VALUE #( ( key ) ) ).

    TRY.
        devclass = ret[ KEY primary_key COMPONENTS
                        key = key
                      ]-devclass.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_package_of_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the package of the given objects
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA new_keys TYPE tadir_key_list.

    " Prepare """""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK key IS NOT INITIAL.
    DATA(local_keys) = key.
    SORT local_keys BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM local_keys COMPARING pgmid object obj_name.

    " Eliminate those alread read """""""""""""""""""""""""""""""""""
    LOOP AT local_keys ASSIGNING FIELD-SYMBOL(<key>).

      TRY.
          INSERT ycl_addict_package=>pack_cache[
                   KEY primary_key COMPONENTS
                   key = <key>
                 ] INTO TABLE devclass.

        CATCH cx_sy_itab_line_not_found.
          APPEND <key> TO new_keys.
      ENDTRY.
    ENDLOOP.

    " Read new entries """"""""""""""""""""""""""""""""""""""""""""""
    IF new_keys IS INITIAL.
      RETURN.
    ENDIF.

    SELECT pgmid, object, obj_name, devclass
           FROM tadir
           FOR ALL ENTRIES IN @new_keys
           WHERE pgmid    = @new_keys-pgmid  AND
                 object   = @new_keys-object AND
                 obj_name = @new_keys-obj_name
            INTO TABLE @DATA(news).

    LOOP AT news ASSIGNING FIELD-SYMBOL(<new>).
      INSERT VALUE #( key = VALUE #(
                        pgmid = <new>-pgmid
                        object = <new>-object
                        obj_name = <new>-obj_name )
                      devclass = <new>-devclass
             ) INTO TABLE: ycl_addict_package=>pack_cache,
                           devclass.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_obj_custom_development.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tells if the object is a custom development or not
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_package=>custom_dev_cache[
             KEY primary_key
             COMPONENTS key = key
           ] TO FIELD-SYMBOL(<cache>).

    IF sy-subrc <> 0.
      INSERT VALUE #(
               key = key
               custom = xsdbool( get_package_of_obj( key ) IN get_nonsap_package_rng( ) )
             ) INTO TABLE ycl_addict_package=>custom_dev_cache ASSIGNING <cache>.
    ENDIF.

    nonsap = <cache>-custom.
  ENDMETHOD.
ENDCLASS.
