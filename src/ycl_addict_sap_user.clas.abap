CLASS ycl_addict_sap_user DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA bname TYPE xubname READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING bname             TYPE xubname   DEFAULT sy-uname
                tolerate_inactive TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result)     TYPE REF TO ycl_addict_sap_user
      RAISING   ycx_addict_sap_user.

    METHODS can_develop RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             bname TYPE xubname,
             obj   TYPE REF TO ycl_addict_sap_user,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map
                       WITH UNIQUE KEY primary_key COMPONENTS bname.

    CLASS-DATA multitons TYPE multiton_set.

    DATA usr02 TYPE usr02.
ENDCLASS.


CLASS ycl_addict_sap_user IMPLEMENTATION.
  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key
                      bname = bname ]
           TO FIELD-SYMBOL(<multiton>).

    IF sy-subrc <> 0.
      DATA(multiton) = VALUE multiton_map( bname = bname ).

      SELECT SINGLE * FROM usr02
             WHERE bname = @multiton-bname
             INTO @DATA(usr02).

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW ycx_addict_sap_user( textid = ycx_addict_sap_user=>user_unknown
                                                 uname  = multiton-bname ).
      ENDIF.

      IF     tolerate_inactive = abap_false
         AND (    usr02-uflag IS NOT INITIAL
               OR ( usr02-gltgv IS NOT INITIAL AND usr02-gltgv > sy-datum )
               OR ( usr02-gltgb IS NOT INITIAL AND usr02-gltgb < sy-datum ) ).

        RAISE EXCEPTION NEW ycx_addict_sap_user( textid = ycx_addict_sap_user=>user_inactive
                                                 uname  = multiton-bname ).
      ENDIF.

      multiton-obj = NEW #( ).
      multiton-obj->bname = multiton-bname.
      multiton-obj->usr02 = usr02.

      INSERT multiton INTO TABLE multitons ASSIGNING <multiton>.
    ENDIF.

    result = <multiton>-obj.
  ENDMETHOD.

  METHOD can_develop.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                    FOR USER me->bname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  DUMMY
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.
ENDCLASS.
