CLASS ycl_addict_dol_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES dol_list       TYPE STANDARD TABLE OF ysaddict_dol_list WITH EMPTY KEY.
    TYPES dol_list_wr    TYPE STANDARD TABLE OF ysaddict_dol_list_with_req WITH EMPTY KEY.

    TYPES: BEGIN OF param_dict,
             ticket_keys TYPE yif_addict_system_rules=>ticket_key_list,
             trkorr_rng  TYPE ytt_addict_trkorr_rng,
           END OF param_dict.

    CONSTANTS: BEGIN OF field,
                 trkorr TYPE fieldname VALUE 'TRKORR',
               END OF field.

    CONSTANTS: BEGIN OF table,
                 seoclass TYPE tabname VALUE 'SEOCLASS',
                 dlwr     TYPE tabname VALUE 'YSADDICT_DOL_LIST_WITH_REQ',
               END OF table.

    CLASS-METHODS get_dol_obj
      IMPORTING !object TYPE trobjtype
      EXPORTING !dol    TYPE REF TO yif_addict_dol_obj
      RAISING   ycx_addict_table_content.

    METHODS get_list
      IMPORTING !param      TYPE param_dict
      RETURNING VALUE(list) TYPE dol_list
      RAISING   ycx_addict_class_method.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF dol_obj_dict,
             object TYPE trobjtype,
             obj    TYPE REF TO yif_addict_dol_obj,
             cx     TYPE REF TO ycx_addict_table_content,
           END OF dol_obj_dict,

           dol_obj_set TYPE HASHED TABLE OF dol_obj_dict WITH UNIQUE KEY primary_key COMPONENTS object.

    CONSTANTS clsname_obj_pfx  TYPE seoclsname VALUE 'YCL_ADDICT_DOL_OBJ_'.

    CLASS-DATA dol_objects TYPE dol_obj_set.

    DATA list    TYPE dol_list.
    DATA trkorrs TYPE yif_addict_system_rules=>trkorr_list.
    DATA param   TYPE param_dict.

    METHODS build_request_list RAISING ycx_addict_class_method.
    METHODS read_request_contents.
ENDCLASS.



CLASS YCL_ADDICT_DOL_MODEL IMPLEMENTATION.


  METHOD build_request_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Builds transport request list of provided tickets
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(rules) = ycl_addict_toolkit=>get_system_rules( ).
    me->trkorrs = rules->get_requests_of_tickets( me->param-ticket_keys ).
  ENDMETHOD.


  METHOD get_dol_obj.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a data object list object
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA obj     TYPE REF TO object.
    DATA clsname TYPE seoclsname.

    READ TABLE ycl_addict_dol_model=>dol_objects
         ASSIGNING FIELD-SYMBOL(<dol_obj>)
         WITH TABLE KEY primary_key
         COMPONENTS object = object.

    IF sy-subrc <> 0.
      DATA(dol_obj) = VALUE dol_obj_dict( object = object ).

      clsname = |{ clsname_obj_pfx }{ object }|.

      SELECT SINGLE clsname FROM seoclass
             WHERE clsname = @clsname
             INTO @DATA(clsname_dummy).

      IF sy-subrc = 0.
        CREATE OBJECT obj TYPE (clsname).
        dol_obj-obj ?= obj.
      ELSE.
        dol_obj-cx = NEW #( objectid = CONV #( clsname )
                            tabname  = table-seoclass
                            textid   = ycx_addict_table_content=>no_entry_for_objectid ).
      ENDIF.

      INSERT dol_obj INTO TABLE ycl_addict_dol_model=>dol_objects ASSIGNING <dol_obj>.
    ENDIF.

    IF <dol_obj>-cx IS NOT INITIAL.
      RAISE EXCEPTION <dol_obj>-cx.
    ENDIF.

    dol = <dol_obj>-obj.
  ENDMETHOD.


  METHOD get_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Main method
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->param = param.
    CLEAR me->list.

    build_request_list( ).
    IF me->trkorrs IS INITIAL.
      RETURN.
    ENDIF.

    read_request_contents( ).
    IF me->list IS INITIAL.
      RETURN.
    ENDIF.

    list = me->list.
  ENDMETHOD.


  METHOD read_request_contents.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Reads the contents of the given transport request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->trkorrs IS NOT INITIAL.

    ycl_addict_transport_request=>get_request_objects(
      EXPORTING trkorr_rng = VALUE #( FOR _trkorr IN me->trkorrs (
                               option = ycl_addict_toolkit=>option-eq
                               sign   = ycl_addict_toolkit=>sign-include
                               low    = _trkorr ) )
      IMPORTING list       = me->list ).
  ENDMETHOD.
ENDCLASS.
