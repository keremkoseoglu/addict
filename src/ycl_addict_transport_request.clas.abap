CLASS ycl_addict_transport_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES as4text_range TYPE RANGE OF e07t-as4text.
    TYPES as4user_range TYPE RANGE OF e070-as4user.
    TYPES date_range TYPE RANGE OF dats.
    TYPES obj_name_range TYPE RANGE OF e071-obj_name .
    TYPES object_range TYPE RANGE OF e071-object.
    TYPES pgmid_range TYPE RANGE OF e071-pgmid.
    TYPES trfunction_range TYPE RANGE OF e070-trfunction.
    TYPES trstatus_range TYPE RANGE OF e070-trstatus.

    TYPES request_attribute_list TYPE STANDARD TABLE OF e070a WITH EMPTY KEY.
    TYPES request_key_list TYPE STANDARD TABLE OF e071k WITH EMPTY KEY.
    TYPES request_nametab_list TYPE STANDARD TABLE OF e071kf WITH EMPTY KEY.
    TYPES request_object_list TYPE STANDARD TABLE OF e071 WITH EMPTY KEY.
    TYPES trkorr_list TYPE STANDARD TABLE OF trkorr WITH KEY table_line.
    TYPES user_list TYPE STANDARD TABLE OF xubname WITH EMPTY KEY.

    TYPES: BEGIN OF content_dict,
             objects    TYPE request_object_list,
             keys       TYPE request_key_list,
             nametabs   TYPE request_nametab_list,
             attributes TYPE request_attribute_list,
           END OF content_dict.

    TYPES: BEGIN OF request_type_dict,
             pgmid  TYPE e071-pgmid,
             object TYPE e071-object,
           END OF request_type_dict.

    TYPES: BEGIN OF request_param_dict,
             trkorr_rng        TYPE ytt_addict_trkorr_rng,
             trfunction_rng    TYPE ytt_addict_trfunction_rng,
             trstatus_rng      TYPE trstatus_range,
             as4user_rng       TYPE as4user_range,
             as4text_rng       TYPE as4text_range,
             as4date_rng       TYPE date_range,
             must_have_subtask TYPE abap_bool,
             srch_strkorr      TYPE abap_bool,
             ignore_trkorr     TYPE abap_bool,
           END OF request_param_dict.

    TYPES: BEGIN OF request_and_object_dict,
             trkorr   TYPE e070-trkorr,
             as4text  TYPE e07t-as4text,
             strkorr  TYPE e070-strkorr,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF request_and_object_dict,

           request_and_object_list TYPE STANDARD TABLE OF request_and_object_dict WITH EMPTY KEY.

    TYPES: BEGIN OF request_obj_dict,
             trkorr TYPE trkorr,
             obj    TYPE REF TO ycl_addict_transport_request,
           END OF request_obj_dict,

           request_obj_list TYPE STANDARD TABLE OF request_obj_dict WITH EMPTY KEY.

    TYPES tmsbuffer_list TYPE STANDARD TABLE OF tmsbuffer WITH EMPTY KEY.

    CONSTANTS: BEGIN OF domain,
                 trfunction TYPE domname VALUE 'TRFUNCTION',
               END OF domain.

    CONSTANTS: BEGIN OF object,
                 cdat TYPE e071-object VALUE 'CDAT',
                 cinc TYPE e071-object VALUE 'CINC',
                 clas TYPE e071-object VALUE 'CLAS',
                 clsd TYPE e071-object VALUE 'CLSD',
                 cpub TYPE e071-object VALUE 'CPUB',
                 cpri TYPE e071-object VALUE 'CPRI',
                 cpro TYPE e071-object VALUE 'CPRO',
                 ddls TYPE e071-object VALUE 'DDLS',
                 doma TYPE e071-object VALUE 'DOMA',
                 domd TYPE e071-object VALUE 'DOMD',
                 dted TYPE e071-object VALUE 'DTED',
                 dtel TYPE e071-object VALUE 'DTEL',
                 tabd TYPE e071-object VALUE 'TABD',
                 tabl TYPE e071-object VALUE 'TABL',
                 tdat TYPE e071-object VALUE 'TDAT',
                 intf TYPE e071-object VALUE 'INTF',
                 meth TYPE e071-object VALUE 'METH',
                 prog TYPE e071-object VALUE 'PROG',
                 reps type e071-object value 'REPS',
                 vdat TYPE e071-object VALUE 'VDAT',
                 view TYPE e071-object VALUE 'VIEW',
               END OF object.

    CONSTANTS: BEGIN OF pgmid,
                 corr TYPE e071-pgmid VALUE 'CORR',
                 limu TYPE e071-pgmid VALUE 'LIMU',
                 r3tr TYPE e071-pgmid VALUE 'R3TR',
               END OF pgmid.

    CONSTANTS: BEGIN OF trfunction,
                 cust      TYPE trfunction VALUE 'W',
                 cust_task TYPE trfunction VALUE 'Q',
                 toc       TYPE trfunction VALUE 'T',
                 unclass   TYPE trfunction VALUE 'X',
                 wb        TYPE trfunction VALUE 'K',
               END OF trfunction.

    CONSTANTS: BEGIN OF trstatus,
                 modif           TYPE trstatus VALUE 'D',
                 modif_prot      TYPE trstatus VALUE 'L',
                 release_started TYPE trstatus VALUE 'O',
                 released        TYPE trstatus VALUE 'R',
                 release_prot    TYPE trstatus VALUE 'N',
               END OF trstatus.

    CONSTANTS: BEGIN OF method,
                 get_request_subtask_tree TYPE seocpdname VALUE 'GET_REQUEST_SUBTASK_TREE',
               END OF method.

    CONSTANTS: BEGIN OF class,
                 me TYPE seoclsname VALUE 'YCL_ADDICT_TRANSPORT_REQUEST',
               END OF class.

    DATA trkorr TYPE trkorr READ-ONLY .

    CLASS-METHODS create_new_request
      IMPORTING !trfunction   TYPE trfunction
                !as4text      TYPE as4text OPTIONAL
                !users        TYPE user_list OPTIONAL
                !target       TYPE tr_target OPTIONAL
      RETURNING VALUE(output) TYPE REF TO ycl_addict_transport_request
      RAISING   ycx_addict_table_content
                ycx_addict_function_subrc.

    CLASS-METHODS get_as4text_safe
      IMPORTING !trkorr       TYPE trkorr
      RETURNING VALUE(output) TYPE as4text .

    CLASS-METHODS get_empty_open_requests
      IMPORTING !trkorr_rng   TYPE ytt_addict_trkorr_rng
      RETURNING VALUE(output) TYPE ytt_addict_trkorr_det
      RAISING   ycx_addict_table_content
                ycx_addict_function_subrc.

    CLASS-METHODS get_instance
      IMPORTING !trkorr       TYPE trkorr
                !top          TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(output) TYPE REF TO ycl_addict_transport_request
      RAISING   ycx_addict_table_content.

    CLASS-METHODS get_modified_objects
      IMPORTING
        !obj          TYPE ytt_addict_e071_obj_key
      RETURNING
        VALUE(output) TYPE ytt_addict_e071_obj_key.

    CLASS-METHODS get_open_status_rng RETURNING VALUE(output) TYPE trstatus_range.
    CLASS-METHODS get_released_status_rng RETURNING VALUE(result) TYPE trstatus_range.

    CLASS-METHODS get_open_requests
      IMPORTING !as4text_rng       TYPE as4text_range OPTIONAL
                !trfunction_rng    TYPE ytt_addict_trfunction_rng OPTIONAL
                !must_have_subtask TYPE abap_bool OPTIONAL
      RETURNING VALUE(output)      TYPE ytt_addict_trkorr_det.

    CLASS-METHODS get_request_list
      IMPORTING !param        TYPE request_param_dict
      RETURNING VALUE(output) TYPE ytt_addict_trkorr_det.

    CLASS-METHODS get_request_and_objects
      IMPORTING !tags         TYPE ytt_addict_e071_obj_key
      RETURNING VALUE(output) TYPE request_and_object_list.

    CLASS-METHODS get_request_objects
      IMPORTING !trkorr_rng    TYPE ytt_addict_trkorr_rng
                !pgmid_rng     TYPE pgmid_range OPTIONAL
                !read_creation TYPE abap_bool DEFAULT abap_false
      EXPORTING !list          TYPE ycl_addict_dol_model=>dol_list
                !list_wr       TYPE ycl_addict_dol_model=>dol_list_wr.

    CLASS-METHODS get_requests_containing_obj
      IMPORTING !obj            TYPE ytt_addict_e071_obj_key
                !top            TYPE abap_bool DEFAULT abap_true
                !holistic_cls   TYPE abap_bool DEFAULT abap_false
                !trfunction_rng TYPE ytt_addict_trfunction_rng OPTIONAL
      RETURNING VALUE(req)      TYPE request_and_object_list.

    CLASS-METHODS get_source_client_safe
      IMPORTING !trkorr       TYPE trkorr
      RETURNING VALUE(client) TYPE e070c-client .

    CLASS-METHODS get_toc_safety_safe
      IMPORTING !trkorr     TYPE trkorr
      RETURNING VALUE(safe) TYPE abap_bool .

    CLASS-METHODS get_user_creatable_trf_rng
      RETURNING VALUE(func) TYPE ytt_addict_trfunction_rng .

    CLASS-METHODS is_obj_type_class_related
      IMPORTING !obj_type      TYPE request_type_dict
      RETURNING VALUE(related) TYPE abap_bool .

    CLASS-METHODS is_request_external
      IMPORTING !trkorr       TYPE e070-trkorr
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS add_objects
      IMPORTING !obj               TYPE ytt_addict_e071_obj_key
                !sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING   ycx_addict_function_subrc
                RESUMABLE(ycx_addict_sort_and_compress).

    METHODS add_objects_from_request
      IMPORTING !from              TYPE trkorr_list
                !wait              TYPE abap_bool DEFAULT abap_false
                !sort_and_compress TYPE abap_bool DEFAULT abap_false
      RAISING   ycx_addict_function_subrc
                RESUMABLE(ycx_addict_sort_and_compress)
                ycx_addict_table_content .

    METHODS complete_shi_piece_list RAISING ycx_addict_sh_piece_list_comp.
    METHODS create_subtask IMPORTING !user TYPE user_list.
    METHODS delete RAISING ycx_addict_function_subrc.

    METHODS delete_empty_subtasks
      RAISING ycx_addict_function_subrc
              ycx_addict_table_content.

    METHODS delete_object
      IMPORTING !obj TYPE ysaddict_e071_obj_key
      RAISING   ycx_addict_function_subrc.

    METHODS get_content
      RETURNING VALUE(content) TYPE content_dict
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content .

    METHODS get_objects
      IMPORTING !pgmid_rng     TYPE pgmid_range OPTIONAL
                !object_rng    TYPE object_range OPTIONAL
                !obj_name_rng  TYPE obj_name_range OPTIONAL
                !devclass_rng  TYPE ycl_addict_package=>package_range OPTIONAL
      RETURNING VALUE(objects) TYPE request_object_list
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content.

    METHODS get_header RETURNING VALUE(header) TYPE e070.

    METHODS get_obj_related_requests
      IMPORTING !include_self   TYPE abap_bool DEFAULT abap_false
                !top            TYPE abap_bool DEFAULT abap_true
                !object_rng     TYPE object_range OPTIONAL
                !obj_name_rng   TYPE obj_name_range OPTIONAL
                !holistic_cls   TYPE abap_bool DEFAULT abap_false
                !trfunction_rng TYPE ytt_addict_trfunction_rng OPTIONAL
                !devclass_rng   TYPE ycl_addict_package=>package_range OPTIONAL
      RETURNING VALUE(list)     TYPE request_and_object_list
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content .

    METHODS get_source_client RETURNING VALUE(client) TYPE e070c-client .

    METHODS get_subtasks
      IMPORTING !only_empty    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(subtask) TYPE request_obj_list
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content.

    METHODS get_text RETURNING VALUE(as4text) TYPE as4text.

    METHODS get_request_subtask_tree
      RETURNING VALUE(result) TYPE trkorr_list
      RAISING   ycx_addict_class_method.

    METHODS has_locked_object RETURNING VALUE(has) TYPE abap_bool.

    METHODS has_merge
      RETURNING VALUE(has) TYPE abap_bool
      RAISING   ycx_addict_data_read.

    METHODS is_empty
      RETURNING VALUE(empty) TYPE abap_bool
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content.

    METHODS is_toc_safe
      RETURNING VALUE(safe) TYPE abap_bool
      RAISING   ycx_addict_class_method.

    METHODS is_released RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_external RETURNING VALUE(result) TYPE abap_bool.

    METHODS release
      IMPORTING !rel_subtasks_too    TYPE abap_bool
                !del_empty_subtasks  TYPE abap_bool DEFAULT abap_true
                !wait_until_released TYPE abap_bool DEFAULT abap_false
                !max_rel_wait        TYPE i OPTIONAL
                !compl_sh_piece_list TYPE abap_bool DEFAULT abap_true
      EXPORTING rel_wait_success     TYPE abap_bool
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content.

    METHODS sort_and_compress RAISING ycx_addict_sort_and_compress.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES request_object_type_set TYPE HASHED TABLE OF request_type_dict
          WITH UNIQUE KEY primary_key COMPONENTS pgmid object.

    TYPES: BEGIN OF clazy_flag_dict,
             class_related_obj_tags TYPE abap_bool,
             dev_req_rng            TYPE abap_bool,
             released_trstatus_rng  TYPE abap_bool,
           END OF clazy_flag_dict.

    TYPES: BEGIN OF clazy_val_dict,
             class_related_obj_tag TYPE request_object_type_set,
             dev_req_rng           TYPE ytt_addict_trkorr_rng,
             released_trstatus_rng TYPE trstatus_range,
           END OF clazy_val_dict.

    TYPES: BEGIN OF lazy_flag_dict,
             as4text              TYPE abap_bool,
             content              TYPE abap_bool,
             e070                 TYPE abap_bool,
             empty                TYPE abap_bool,
             source_client        TYPE abap_bool,
             toc_safe             TYPE abap_bool,
             request_subtask_tree TYPE abap_bool,
           END OF lazy_flag_dict.

    TYPES: BEGIN OF lazy_val_dict,
             as4text              TYPE as4text,
             content              TYPE content_dict,
             e070                 TYPE e070,
             empty                TYPE abap_bool,
             source_client        TYPE e070c-client,
             toc_safe             TYPE abap_bool,
             request_subtask_tree TYPE trkorr_list,
           END OF lazy_val_dict.

    TYPES abap_bool_list TYPE STANDARD TABLE OF abap_bool WITH EMPTY KEY.
    TYPES obj_name_list TYPE STANDARD TABLE OF e071-obj_name WITH EMPTY KEY.

    TYPES: BEGIN OF multiton_dict,
             trkorr TYPE trkorr,
             top    TYPE abap_bool,
             obj    TYPE REF TO ycl_addict_transport_request,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS trkorr top.

    CONSTANTS: BEGIN OF table,
                 e070 TYPE tabname VALUE 'E070',
               END OF table.

    CONSTANTS: BEGIN OF tcode,
                 shi_piece_list TYPE sytcode VALUE 'YADDICT001',
               END OF tcode.

    CLASS-DATA clazy_flag TYPE clazy_flag_dict.
    CLASS-DATA clazy_val  TYPE clazy_val_dict.
    CLASS-DATA multitons  TYPE multiton_set.

    DATA lazy_flag TYPE lazy_flag_dict.
    DATA lazy_val  TYPE lazy_val_dict.

    CLASS-METHODS build_dev_req_rng.

    CLASS-METHODS release_single
      IMPORTING !trkorr              TYPE trkorr
                !req                 TYPE REF TO ycl_addict_transport_request OPTIONAL
                !wait_until_released TYPE abap_bool DEFAULT abap_false
                !max_rel_wait        TYPE i
                !compl_sh_piece_list TYPE abap_bool DEFAULT abap_true
      EXPORTING !rel_wait_success    TYPE abap_bool
      RAISING   ycx_addict_function_subrc
                ycx_addict_table_content.
ENDCLASS.



CLASS YCL_ADDICT_TRANSPORT_REQUEST IMPLEMENTATION.


  METHOD add_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Appends the given objects to the transport request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA t071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    " Add objects """""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK obj IS NOT INITIAL.

    t071 = CORRESPONDING #( obj ).

    CALL FUNCTION 'TRINT_APPEND_TO_COMM_ARRAYS'
      EXPORTING
        wi_error_table            = abap_true
        wi_trkorr                 = me->trkorr
        iv_append_at_order        = abap_true
      TABLES
        wt_e071                   = t071[]
      EXCEPTIONS
        key_check_keysyntax_error = 1
        ob_check_obj_error        = 2
        tr_lockmod_failed         = 3
        tr_lock_enqueue_failed    = 4
        tr_wrong_order_type       = 5
        tr_order_update_error     = 6
        file_access_error         = 7
        ob_no_systemname          = 8
        OTHERS                    = 9
        ##FM_SUBRC_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_APPEND_TO_COMM_ARRAYS' ).

    " Sort & Compress """""""""""""""""""""""""""""""""""""""""""""""
    IF sort_and_compress = abap_true.
      TRY.
          sort_and_compress( ).
        CATCH ycx_addict_sort_and_compress INTO DATA(sac).
          RAISE RESUMABLE EXCEPTION sac.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD add_objects_from_request.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Appends objects of a request to this request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Build a list of requests & tasks """"""""""""""""""""""""""""""
    DATA(trkorr_from) = from.

    LOOP AT from ASSIGNING FIELD-SYMBOL(<from>).
      DATA(subtasks) = get_instance( trkorr = <from>
                                     top = abap_true
                                   )->get_subtasks( ).

      APPEND LINES OF VALUE trkorr_list( FOR _subtask IN subtasks
                                         ( _subtask-trkorr ) )
             TO trkorr_from.
    ENDLOOP.

    SORT trkorr_from BY table_line.
    DELETE ADJACENT DUPLICATES FROM trkorr_from COMPARING table_line.

    " Append """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT trkorr_from ASSIGNING <from>.
      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = abap_false
          wi_trkorr_from           = <from>
          wi_trkorr_to             = me->trkorr
          wi_without_documentation = abap_true
        EXCEPTIONS
          db_access_error          = 1
          trkorr_from_not_exist    = 2
          trkorr_to_is_repair      = 3
          trkorr_to_locked         = 4
          trkorr_to_not_exist      = 5
          trkorr_to_released       = 6
          user_not_owner           = 7
          no_authorization         = 8
          wrong_client             = 9
          wrong_category           = 10
          object_not_patchable     = 11
          OTHERS                   = 12
          ##FM_SUBRC_OK ##NUMBER_OK.

      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TR_COPY_COMM' ).
      CHECK wait = abap_true.
      WAIT UP TO 1 SECONDS.
    ENDLOOP.

    " Sort & Compress """""""""""""""""""""""""""""""""""""""""""""""
    IF sort_and_compress = abap_true.
      TRY.
          sort_and_compress( ).
        CATCH ycx_addict_sort_and_compress INTO DATA(sac).
          RAISE RESUMABLE EXCEPTION sac.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD build_dev_req_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Builds a range of requests in the development system
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK ycl_addict_transport_request=>clazy_flag-dev_req_rng IS INITIAL.

    ycl_addict_transport_request=>clazy_val-dev_req_rng = VALUE #( (
        sign   = ycl_addict_toolkit=>sign-include
        option = ycl_addict_toolkit=>option-cp
        low    = |{ sy-sysid }K*| ) ).

    ycl_addict_transport_request=>clazy_flag-dev_req_rng = abap_true.
  ENDMETHOD.


  METHOD complete_shi_piece_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Completes the piece list within the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(bdc) = NEW ycl_addict_bdc( ).

        bdc->add_scr( prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
                      dyn = '1000' ).

        bdc->add_fld(: nam = 'BDC_OKCODE' val = '=ONLI' ),
                       nam = 'P_TRKORR'   val = CONV #( me->trkorr ) ).

        bdc->add_scr( prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
                      dyn = '0100' ).

        bdc->add_fld( nam = 'BDC_OKCODE'
                      val = '=ENTER' ).

        bdc->add_scr( prg = 'RS_STREE_OBJECTS_TO_REQ_GET'
                      dyn = '1000' ).

        bdc->add_fld( nam = 'BDC_OKCODE'
                      val = '/EE' ).

        bdc->submit( tcode        = me->tcode-shi_piece_list
                     option       = VALUE #( dismode = ycl_addict_bdc=>dismode-none )
                     validate_msg = abap_true ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_sh_piece_list_comp
          EXPORTING
            previous = diaper
            trkorr   = me->trkorr.
    ENDTRY.
  ENDMETHOD.


  METHOD create_new_request.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Creates a new transport request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA head          TYPE trwbo_request_header.
    DATA final_as4text TYPE as4text.

    IF users IS SUPPLIED.
      DATA(user) = VALUE scts_users( FOR _user IN users
                                     ( user = _user
                                       type = ycl_addict_transport_request=>trfunction-unclass ) ).
    ENDIF.

    DATA(auto_prefix) = ycl_addict_toolkit=>get_system_definitions( )-auto_request_prefix.

    final_as4text = COND #( WHEN as4text IS SUPPLIED
                            THEN as4text
                            ELSE |{ auto_prefix } Request| ).

    CLEAR head.

    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = trfunction
        iv_text           = final_as4text
        it_users          = user
        iv_target         = target
      IMPORTING
        es_request_header = head
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3
        ##FM_SUBRC_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TR_INSERT_REQUEST_WITH_TASKS' ).
    output = get_instance( head-trkorr ).
  ENDMETHOD.


  METHOD create_subtask.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Creates a new subtask within the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA msg   TYPE STANDARD TABLE OF tr004.
    DATA tr005 TYPE STANDARD TABLE OF tr005.

    CLEAR: tr005, msg.

    tr005 = VALUE #( FOR _user IN user ( as4user = _user ) ).

    CALL FUNCTION 'TR40_TASK_ADD'
      EXPORTING
        iv_trkorr   = me->trkorr
      TABLES
        tt_userlist = tr005
        tt_msg      = msg.
  ENDMETHOD.


  METHOD delete.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Deletes the transport request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'TRINT_DELETE_COMM'
      EXPORTING
        wi_dialog                     = abap_false
        wi_trkorr                     = me->trkorr
        iv_without_any_checks         = abap_true
        iv_without_user_check         = abap_true
        iv_without_ctsproject_check   = abap_true
      EXCEPTIONS
        file_access_error             = 1
        order_already_released        = 2
        order_contains_c_member       = 3
        order_contains_locked_entries = 4
        order_is_refered              = 5
        repair_order                  = 6
        user_not_owner                = 7
        delete_was_cancelled          = 8
        objects_free_but_still_locks  = 9
        order_lock_failed             = 10
        wrong_client                  = 11
        project_still_referenced      = 12
        successors_already_released   = 13
        OTHERS                        = 14
        ##FM_SUBRC_OK ##NUMBER_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_DELETE_COMM' ).
  ENDMETHOD.


  METHOD delete_empty_subtasks.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Deletes the empty subtasks within the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT get_subtasks( only_empty = abap_true ) ASSIGNING FIELD-SYMBOL(<sub>).
      <sub>-obj->delete( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_object.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Deletes an object from within the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(e071) = CORRESPONDING e071( obj ).
    e071-trkorr = me->trkorr.

    DATA(req) = VALUE trwbo_request( ).

    CALL FUNCTION 'TRINT_DELETE_COMM_OBJECT_KEYS'
      EXPORTING
        is_e071_delete              = e071
        iv_dialog_flag              = abap_false
      CHANGING
        cs_request                  = req
      EXCEPTIONS
        e_bad_target_request        = 1
        e_database_access_error     = 2
        e_empty_lockkey             = 3
        e_wrong_source_client       = 4
        n_no_deletion_of_c_objects  = 5
        n_no_deletion_of_corr_entry = 6
        n_object_entry_doesnt_exist = 7
        n_request_already_released  = 8
        n_request_from_other_system = 9
        r_user_cancelled            = 10
        r_user_didnt_confirm        = 11
        r_foreign_lock              = 12
        w_bigger_lock_in_same_order = 13
        w_duplicate_entry           = 14
        w_no_authorization          = 15
        w_user_not_owner            = 16
        OTHERS                      = 17
        ##FM_SUBRC_OK ##NUMBER_OK.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_DELETE_COMM_OBJECT_KEYS' ).
  ENDMETHOD.


  METHOD get_as4text_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the request description without generating exception
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        output = get_instance( trkorr )->get_text( ).
      CATCH cx_root ##no_handler .
    ENDTRY.
  ENDMETHOD.


  METHOD get_content.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the content of the request
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-content IS INITIAL.
      CALL FUNCTION 'TR_READ_COMM'
        EXPORTING
          wi_trkorr        = me->trkorr
          wi_dialog        = abap_false
          wi_langu         = sy-langu
          wi_sel_e071      = abap_true
          wi_sel_e071k     = abap_true
          iv_sel_e071kf    = abap_true
          iv_sel_e070a     = abap_true
        TABLES
          wt_e071          = me->lazy_val-content-objects
          wt_e071k         = me->lazy_val-content-keys
          et_e071kf        = me->lazy_val-content-nametabs
          et_e070a         = me->lazy_val-content-attributes
        EXCEPTIONS
          not_exist_e070   = 1
          no_authorization = 2
          OTHERS           = 3
          ##FM_SUBRC_OK.

      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TR_READ_COMM' ).

      LOOP AT get_subtasks( ) ASSIGNING FIELD-SYMBOL(<sub>).
        DATA(sub_content) = <sub>-obj->get_content( ).

        APPEND LINES OF: sub_content-objects    TO me->lazy_val-content-objects,
                         sub_content-keys       TO me->lazy_val-content-keys,
                         sub_content-nametabs   TO me->lazy_val-content-nametabs,
                         sub_content-attributes TO me->lazy_val-content-attributes.
      ENDLOOP.

      me->lazy_flag-content = abap_true.
    ENDIF.

    content = me->lazy_val-content.
  ENDMETHOD.


  METHOD get_empty_open_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns empty open requests
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT trkorr FROM e070 AS request
           WHERE trkorr IN @trkorr_rng AND
                 ( strkorr = @space OR strkorr IS NULL ) AND
                 ( NOT EXISTS ( SELECT trkorr FROM e071 WHERE trkorr = request~trkorr ) ) AND
                 ( NOT EXISTS ( SELECT trkorr FROM e070 AS task
                                WHERE strkorr = request~trkorr AND
                                EXISTS ( SELECT trkorr FROM e071 WHERE trkorr = task~trkorr ) ) )
           INTO TABLE @DATA(trkorr).

    LOOP AT trkorr ASSIGNING FIELD-SYMBOL(<trkorr>).
      CHECK NOT get_instance( <trkorr>-trkorr )->is_empty( ).
      DELETE trkorr.
      CONTINUE.
    ENDLOOP.

    IF trkorr IS INITIAL.
      RETURN.
    ENDIF.

    output = get_request_list( VALUE #( trkorr_rng    = VALUE #( FOR _trkorr IN trkorr
                                                                 ( option = ycl_addict_toolkit=>option-eq
                                                                   sign   = ycl_addict_toolkit=>sign-include
                                                                   low    = _trkorr ) )
                                       trstatus_rng   = get_open_status_rng( )
                                       srch_strkorr   = abap_false
                                       ignore_trkorr  = abap_false ) ).
  ENDMETHOD.


  METHOD get_header.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns request header
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-e070 IS INITIAL.
      SELECT SINGLE * FROM e070
             WHERE trkorr = @me->trkorr
             INTO CORRESPONDING FIELDS OF @me->lazy_val-e070.

      me->lazy_flag-e070 = abap_true.
    ENDIF.

    header = me->lazy_val-e070.
  ENDMETHOD.


  METHOD get_instance.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Factory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN ycl_addict_transport_request=>multitons[
             KEY primary_key COMPONENTS
             trkorr = trkorr
             top    = top
           ] TO FIELD-SYMBOL(<mt>).

    IF sy-subrc <> 0.
      DATA(mt) = VALUE multiton_dict( trkorr = trkorr
                                      top    = top ).

      SELECT SINGLE trkorr, strkorr
             FROM e070
             WHERE trkorr = @trkorr
             INTO @DATA(e070).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ycx_addict_table_content
          EXPORTING
            textid   = ycx_addict_table_content=>no_entry_for_objectid
            objectid = CONV #( trkorr )
            tabname  = ycl_addict_transport_request=>table-e070.
      ENDIF.

      mt-obj = NEW #( ).

      mt-obj->trkorr = SWITCH #( top
                                 WHEN abap_false THEN e070-trkorr
                                 WHEN abap_true  THEN COND #( WHEN e070-strkorr IS NOT INITIAL
                                                              THEN e070-strkorr
                                                              ELSE e070-trkorr ) ).

      INSERT mt INTO TABLE ycl_addict_transport_request=>multitons ASSIGNING <mt>.
    ENDIF.

    output = <mt>-obj.
  ENDMETHOD.


  METHOD get_modified_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns modified objects within the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(req) = get_requests_containing_obj( obj          = obj
                                             top          = abap_true
                                             holistic_cls = abap_true ).

    DELETE req WHERE trkorr+0(3) <> sy-sysid.
    SORT req BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM req COMPARING pgmid object obj_name. " Binary Search

    LOOP AT obj ASSIGNING FIELD-SYMBOL(<obj>).
      READ TABLE req ASSIGNING FIELD-SYMBOL(<req>)
                     WITH KEY pgmid    = <obj>-pgmid
                              object   = <obj>-object
                              obj_name = <obj>-obj_name
                     BINARY SEARCH.

      IF sy-subrc = 0.
        APPEND <obj> TO output.
        CONTINUE.
      ENDIF.

      CHECK is_obj_type_class_related( CORRESPONDING #( <obj> ) ).

      LOOP AT req ASSIGNING <req> WHERE obj_name+0(30) = <obj>-obj_name+0(30).
        APPEND CORRESPONDING #( <req> ) TO output.
      ENDLOOP.

      CHECK sy-subrc = 0.
      APPEND <obj> TO output.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns objects within the request
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    objects = get_content( )-objects.

    DELETE objects WHERE NOT ( pgmid    IN pgmid_rng  AND
                               object   IN object_rng AND
                               obj_name IN obj_name_rng ).

    IF NOT ( devclass_rng IS NOT INITIAL AND
             object IS NOT INITIAL ).
      RETURN.
    ENDIF.

    DATA(dc) = ycl_addict_package=>get_package_of_objects( CORRESPONDING #( objects ) ).

    LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
      ASSIGN dc[ KEY primary_key COMPONENTS
                 key = VALUE #( pgmid    = <object>-pgmid
                                object   = <object>-object
                                obj_name = <object>-obj_name )
               ] TO FIELD-SYMBOL(<dc>).

      CHECK sy-subrc = 0 AND <dc>-devclass NOT IN devclass_rng.
      DELETE objects.
      CONTINUE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_obj_related_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns requests related to the given objects
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(objects) = get_objects( object_rng   = object_rng
                                 obj_name_rng = obj_name_rng
                                 devclass_rng = devclass_rng ).

    list = get_requests_containing_obj( obj            = CORRESPONDING #( objects )
                                        top            = top
                                        holistic_cls   = holistic_cls
                                        trfunction_rng = trfunction_rng ).

    IF include_self = abap_true.
      RETURN.
    ENDIF.

    TRY.
        DATA(head) = get_header( ).

        DELETE list WHERE trkorr  = head-trkorr  OR
                          trkorr  = head-strkorr OR
                          ( strkorr IS NOT INITIAL AND
                            ( strkorr = head-trkorr  OR
                              strkorr = head-strkorr ) ).

        IF head-strkorr IS NOT INITIAL.
          DATA(shead) = get_instance( head-strkorr )->get_header( ).

          DELETE list WHERE trkorr  = shead-trkorr OR
                            strkorr = shead-trkorr.
        ENDIF.

      CATCH cx_root ##no_handler .
    ENDTRY.
  ENDMETHOD.


  METHOD get_open_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of corresponding open requests
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = get_request_list( VALUE #( trfunction_rng    = COND #( WHEN trfunction_rng IS NOT INITIAL
                                                                    THEN trfunction_rng
                                                                    ELSE get_user_creatable_trf_rng( ) )
                                        trstatus_rng      = get_open_status_rng( )
                                        as4text_rng       = as4text_rng
                                        must_have_subtask = must_have_subtask ) ).
  ENDMETHOD.


  METHOD get_open_status_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a range of open request statuses
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = VALUE #( option = ycl_addict_toolkit=>option-eq
                      sign   = ycl_addict_toolkit=>sign-include
                      ( low  = ycl_addict_transport_request=>trstatus-modif )
                      ( low  = ycl_addict_transport_request=>trstatus-modif_prot ) ).
  ENDMETHOD.


  METHOD get_released_status_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a range of released request statuses
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(flg) = REF #( ycl_addict_transport_request=>clazy_flag-released_trstatus_rng ).
    DATA(val) = REF #( ycl_addict_transport_request=>clazy_val-released_trstatus_rng ).

    IF flg->* = abap_false.
      val->* = VALUE #( sign   = ycl_addict_toolkit=>sign-include
                        option = ycl_addict_toolkit=>option-eq
                        ( low  = ycl_addict_transport_request=>trstatus-released )
                        ( low  = ycl_addict_transport_request=>trstatus-release_prot )
                        ( low  = ycl_addict_transport_request=>trstatus-release_started ) ).

      flg->* = abap_true.
    ENDIF.

    result = val->*.
  ENDMETHOD.


  METHOD get_requests_containing_obj.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of requests which contain the provided objects
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA obj_name_rng TYPE RANGE OF e071-obj_name.
    DATA obj_name_tmp TYPE obj_name_list.

    CHECK obj IS NOT INITIAL.

    " Find requests containing those objects """"""""""""""""""""""""
    build_dev_req_rng( ).

    SELECT trkorr, pgmid, object, obj_name
           FROM e071
           FOR ALL ENTRIES IN @obj
           WHERE trkorr   IN @ycl_addict_transport_request=>clazy_val-dev_req_rng AND
                 pgmid     = @obj-pgmid AND
                 object    = @obj-object AND
                 obj_name  = @obj-obj_name
           INTO TABLE @DATA(e071).

    " Do holistic search if requested """""""""""""""""""""""""""""""
    " Holistic means:
    "
    " If the request contains X=>Y, any other request containing
    " any other X artifact is also related.
    "
    " (X: Class, Y: Method, Type, etc.)
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF holistic_cls = abap_true.
      LOOP AT obj ASSIGNING FIELD-SYMBOL(<obj>).
        CHECK is_obj_type_class_related( CORRESPONDING #( <obj> ) ) = abap_true.

        CLEAR obj_name_tmp.
        SPLIT <obj>-obj_name AT space INTO TABLE obj_name_tmp.

        APPEND VALUE #( option = ycl_addict_toolkit=>option-cp
                        sign   = ycl_addict_toolkit=>sign-include
                        low    = |{ COND #( WHEN obj_name_tmp IS INITIAL THEN <obj>-obj_name ELSE obj_name_tmp[ 1 ] ) }*|
                      ) TO obj_name_rng.
      ENDLOOP.

      SORT obj_name_rng BY option sign low high.
      DELETE ADJACENT DUPLICATES FROM obj_name_rng COMPARING option sign low high.

      IF obj_name_rng IS NOT INITIAL.
        SELECT trkorr, pgmid, object, obj_name
               FROM e071
               WHERE trkorr IN @ycl_addict_transport_request=>clazy_val-dev_req_rng AND
                     ( pgmid = @yif_addict_dol_obj=>pgmid-r3tr OR
                       pgmid = @yif_addict_dol_obj=>pgmid-limu ) AND
                     obj_name IN @obj_name_rng
               APPENDING CORRESPONDING FIELDS OF TABLE @e071. "#EC CI_NOFIRST
      ENDIF.
    ENDIF.

    " Build a list of found requests """"""""""""""""""""""""""""""""
    SORT e071 BY trkorr pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM e071 COMPARING trkorr pgmid object obj_name.

    LOOP AT e071 ASSIGNING FIELD-SYMBOL(<e071>).

      TRY.
          DATA(found_request) = get_instance( trkorr = <e071>-trkorr
                                              top    = top ).

          DATA(head) = found_request->get_header( ).

          APPEND VALUE #( trkorr   = head-trkorr
                          as4text  = found_request->get_text( )
                          strkorr  = head-strkorr
                          pgmid    = <e071>-pgmid
                          object   = <e071>-object
                          obj_name = <e071>-obj_name
                        ) TO req.

        CATCH cx_root ##no_Handler .
      ENDTRY.
    ENDLOOP.

    " Filter by request type if wanted """"""""""""""""""""""""""""""
    IF trfunction_rng IS NOT INITIAL.
      LOOP AT req ASSIGNING FIELD-SYMBOL(<req>).
        TRY.
            CHECK get_instance( trkorr = <req>-trkorr
                                top    = abap_true
                              )->get_header( )-trfunction NOT IN trfunction_rng.
            DELETE req.
            CONTINUE.

          CATCH cx_root ##no_handler.
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_request_and_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns corresponding requests with their contents
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK tags IS NOT INITIAL.

    SELECT e070~trkorr, e07t~as4text, e070~strkorr, e071~obj_name
           FROM e071
                INNER JOIN e070 ON e070~trkorr = e071~trkorr
                LEFT  JOIN e07t ON e07t~trkorr = e071~trkorr AND
                                   e07t~langu  = @sy-langu
           FOR ALL ENTRIES IN @tags
           WHERE pgmid    = @tags-pgmid AND
                 object   = @tags-object AND
                 obj_name = @tags-obj_name
           INTO CORRESPONDING FIELDS OF TABLE @output ##TOO_MANY_ITAB_FIELDS .
  ENDMETHOD.


  METHOD get_request_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of corresponding requests
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Raw list """"""""""""""""""""""""""""""""""""""""""""""""""""""
    IF param-ignore_trkorr = abap_false.
      SELECT e070~trkorr, e070~trfunction, e070~trstatus,
             e07t~as4text, e070~as4user, e070~strkorr,
             e070~as4date, e070~as4time,
             e070~tarsystem
             FROM e070
                  LEFT JOIN e07t ON e07t~trkorr = e070~trkorr
             WHERE e070~trkorr     IN @param-trkorr_rng     AND
                   e070~trfunction IN @param-trfunction_rng AND
                   e070~trstatus   IN @param-trstatus_rng   AND
                   e070~as4date    IN @param-as4date_rng    AND
                   e070~as4user    IN @param-as4user_rng
             APPENDING CORRESPONDING FIELDS OF TABLE @output ##TOO_MANY_ITAB_FIELDS.
    ENDIF.

    IF param-srch_strkorr = abap_true.
      SELECT e070~trkorr, e070~trfunction, e070~trstatus, e07t~as4text,
             e070~as4user, e070~strkorr, e070~as4date, e070~as4time,
             e070~tarsystem
             FROM e070
                  LEFT JOIN e07t ON e07t~trkorr = e070~strkorr
             WHERE e070~strkorr    IN @param-trkorr_rng     AND
                   e070~trfunction IN @param-trfunction_rng AND
                   e070~trstatus   IN @param-trstatus_rng   AND
                   e070~as4date    IN @param-as4date_rng  AND
                   e070~as4user    IN @param-as4user_rng
        APPENDING CORRESPONDING FIELDS OF TABLE @output ##TOO_MANY_ITAB_FIELDS.
    ENDIF.

    DELETE output WHERE as4text NOT IN param-as4text_rng.
    SORT output BY trkorr.
    DELETE ADJACENT DUPLICATES FROM output COMPARING trkorr.

    " Subtask check """""""""""""""""""""""""""""""""""""""""""""""""
    IF param-must_have_subtask = abap_true AND
       output IS NOT INITIAL.

      SELECT trkorr, strkorr FROM e070
             FOR ALL ENTRIES IN @output
             WHERE strkorr  =  @output-trkorr AND
                   trstatus IN (@ycl_addict_transport_request=>trstatus-modif,
                                @ycl_addict_transport_request=>trstatus-modif_prot)
             INTO TABLE @data(task).

      SORT task BY strkorr.

      LOOP AT output ASSIGNING FIELD-SYMBOL(<list>).
        READ TABLE task TRANSPORTING NO FIELDS
                        WITH KEY strkorr = <list>-trkorr
                        BINARY SEARCH.

        CHECK sy-subrc <> 0.
        DELETE output.
        CONTINUE.
      ENDLOOP.
    ENDIF.

    " Domain texts """"""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(domain_trf) = ycl_addict_domain_line=>get_instance( ycl_addict_transport_request=>domain-trfunction ).

        LOOP AT output ASSIGNING <list>.
          TRY.
              <list>-trftxt = domain_trf->domain->get_value_text( CONV #( <list>-trfunction ) ).
            CATCH cx_root ##no_Handler .
          ENDTRY.
        ENDLOOP.

      CATCH cx_root ##no_Handler.
    ENDTRY.
  ENDMETHOD.


  METHOD get_request_objects.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns objects within the request
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA trkorr TYPE RANGE OF e070-trkorr.

    CLEAR: list, list_wr.

    " Detect requests """""""""""""""""""""""""""""""""""""""""""""""
    SELECT @ycl_addict_toolkit=>option-eq AS option,
           @ycl_addict_toolkit=>sign-include AS sign,
           trkorr AS low
           FROM e070
           WHERE trkorr  IN @trkorr_rng OR
                 strkorr IN @trkorr_rng
           APPENDING CORRESPONDING FIELDS OF TABLE @trkorr ##TOO_MANY_ITAB_FIELDS.

    SELECT @ycl_addict_toolkit=>option-eq AS option,
           @ycl_addict_toolkit=>sign-include AS sign,
           strkorr AS low
           FROM e070
           WHERE trkorr  IN @trkorr_rng OR
                 strkorr IN @trkorr_rng
           APPENDING CORRESPONDING FIELDS OF TABLE @trkorr ##TOO_MANY_ITAB_FIELDS .

    SORT trkorr BY low.
    DELETE ADJACENT DUPLICATES FROM trkorr COMPARING low.
    IF trkorr IS INITIAL.
      RETURN.
    ENDIF.

    " Detect objects """"""""""""""""""""""""""""""""""""""""""""""""
    DATA(active_pgmid_rng) = COND #( WHEN pgmid_rng IS SUPPLIED
                                     THEN pgmid_rng
                                     ELSE VALUE #( ( option = ycl_addict_toolkit=>option-eq
                                                     sign   = ycl_addict_toolkit=>sign-include
                                                     low    = yif_addict_dol_obj=>pgmid-r3tr ) ) ).

    SELECT DISTINCT
           e071~trkorr, e071~pgmid, e071~object, e071~obj_name,
           e07t~as4text
           FROM e071
                LEFT JOIN e07t ON e07t~trkorr = e071~trkorr AND
                                  e07t~langu = @sy-langu
           WHERE e071~trkorr IN @trkorr AND
                 e071~pgmid  IN @active_pgmid_rng
           ORDER BY e071~pgmid, e071~object, e071~obj_name
           INTO CORRESPONDING FIELDS OF TABLE @list_wr
           ##too_many_itab_fields.

    DELETE ADJACENT DUPLICATES FROM list_wr COMPARING trkorr pgmid object obj_name.

    " Correct object names """"""""""""""""""""""""""""""""""""""""""
    LOOP AT list_wr ASSIGNING FIELD-SYMBOL(<list>).
      IF is_obj_type_class_related( CORRESPONDING #( <list> ) ).
        DATA(main_pgmid)    = ycl_addict_transport_request=>pgmid-r3tr.
        DATA(main_object)   = ycl_addict_transport_request=>object-clas.
        DATA(main_obj_name) = CONV yd_addict_dol_obj_name( <list>-obj_name+0(30) ).
      ELSE.
        main_pgmid    = <list>-pgmid.
        main_object   = <list>-object.
        main_obj_name = <list>-obj_name.
      ENDIF.

      TRY.
          ycl_addict_dol_model=>get_dol_obj( EXPORTING object = main_object
                                             IMPORTING dol    = DATA(dol) ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      IF dol->is_deleted( pgmid    = main_pgmid
                          object   = main_object
                          obj_name = main_obj_name ).

        DELETE list_wr.
        CONTINUE.
      ENDIF.

      <list>-object_txt = dol->get_object_txt( pgmid  = main_pgmid
                                               object = main_object ).

      <list>-ddtext = dol->get_ddtext( pgmid    = main_pgmid
                                       object   = main_object
                                       obj_name = main_obj_name ).
    ENDLOOP.

    list = CORRESPONDING #( list_wr ).
    SORT list BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM list COMPARING pgmid object obj_name.

    " Created new? """"""""""""""""""""""""""""""""""""""""""""""""""
    IF read_creation = abap_false.
      RETURN.
    ENDIF.

    DATA(e071_tmp) = CORRESPONDING e071tab( list_wr ).

    IF e071_tmp IS INITIAL.
      RETURN.
    ENDIF.

    SELECT e071~trkorr, e071~object, e071~obj_name, e070~strkorr, e070~as4date, e070~as4time
           FROM e071
                INNER JOIN e070 ON e070~trkorr = e071~trkorr
           FOR ALL ENTRIES IN @e071_tmp
           WHERE e071~pgmid    = @ycl_addict_transport_request=>pgmid-r3tr AND
                 e071~object   = @e071_tmp-object AND
                 e071~obj_name = @e071_tmp-obj_name
           INTO TABLE @DATA(req_history).

    SORT req_history BY object obj_name as4date as4time.
    DELETE ADJACENT DUPLICATES FROM req_history COMPARING object obj_name.

    LOOP AT list_wr ASSIGNING FIELD-SYMBOL(<wr>).
      ASSIGN req_history[ object = <wr>-object
                          obj_name = <wr>-obj_name
                        ] TO FIELD-SYMBOL(<history>).

      <wr>-creation = xsdbool( sy-subrc = 0 AND
                               ( <history>-trkorr  = <wr>-trkorr OR
                                 <history>-strkorr = <wr>-trkorr ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_request_subtask_tree.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the entire request structure as a list
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(flag) = REF #( me->lazy_flag-request_subtask_tree ).
        DATA(val)  = REF #( me->lazy_val-request_subtask_tree ).

        IF flag->* = abap_false.
          DATA(header) = get_header( ).

          DATA(parent_request) = COND trkorr( WHEN header-strkorr IS NOT INITIAL
                                              THEN header-strkorr
                                              ELSE header-trkorr ).

          DATA(subtasks) = get_instance( parent_request )->get_subtasks( ).

          val->* = VALUE #( ( parent_request ) ).
          APPEND LINES OF VALUE trkorr_list( FOR _subtask IN subtasks ( _subtask-trkorr ) ) TO val->*.
          flag->* = abap_true.
        ENDIF.

        result = val->*.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_class_method
          EXPORTING
            textid   = ycx_addict_class_method=>unexpected_error
            previous = diaper
            class    = CONV #( ycl_addict_class=>get_class_name( me ) )
            method   = me->method-get_request_subtask_tree.
    ENDTRY.
  ENDMETHOD.


  METHOD get_source_client.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the source client of the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-source_client = abap_false.
      SELECT SINGLE client INTO me->lazy_val-source_client FROM e070c WHERE trkorr = me->trkorr.
      me->lazy_flag-source_client = abap_true.
    ENDIF.

    client = me->lazy_val-source_client.
  ENDMETHOD.


  METHOD get_source_client_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the source client of the request suppressing errors
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        client = ycl_addict_transport_request=>get_instance( trkorr )->get_source_client( ).
      CATCH cx_root ##no_handler .
    ENDTRY.
  ENDMETHOD.


  METHOD get_subtasks.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns subtasks of the request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT trkorr FROM e070
           WHERE strkorr = @me->trkorr
           INTO CORRESPONDING FIELDS OF TABLE @subtask ##TOO_MANY_ITAB_FIELDS .

    LOOP AT subtask ASSIGNING FIELD-SYMBOL(<subtask>).
      <subtask>-obj = ycl_addict_transport_request=>get_instance( <subtask>-trkorr ).

      IF only_empty = abap_true AND <subtask>-obj->is_empty( ) = abap_false.
        DELETE subtask.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns text of request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-as4text = abap_false.
      SELECT SINGLE as4text FROM e07t
             WHERE trkorr = @me->trkorr AND
                   langu  = @sy-langu
             INTO @me->lazy_val-as4text.

      IF sy-subrc <> 0.
        SELECT SINGLE as4text FROM e07t
               WHERE trkorr = @me->trkorr
               INTO @me->lazy_val-as4text ##WARN_OK.    "#EC CI_NOORDER
      ENDIF.

      me->lazy_flag-as4text = abap_true.
    ENDIF.

    as4text = me->lazy_val-as4text.
  ENDMETHOD.


  METHOD get_toc_safety_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns TOC safety, suppressing errors
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        safe = get_instance( trkorr )->is_toc_safe( ).
      CATCH cx_root ##no_Handler .
    ENDTRY.
  ENDMETHOD.


  METHOD get_user_creatable_trf_rng.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a range of TR functions, which can be created by a user
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    func = VALUE #( option = ycl_addict_toolkit=>option-eq
                    sign   = ycl_addict_toolkit=>sign-include
                    ( low = ycl_addict_transport_request=>trfunction-cust )
                    ( low = ycl_addict_transport_request=>trfunction-toc )
                    ( low = ycl_addict_transport_request=>trfunction-wb ) ).
  ENDMETHOD.


  METHOD has_locked_object.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Does the Request contain a locked object?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE lockflag FROM e071
           WHERE trkorr = @me->trkorr AND
                 lockflag = @abap_true
           INTO @data(dummy) ##WARN_OK.

    has = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD has_merge.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Does the Request contain a merged Request?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(content) = get_content( ).

        has = xsdbool( line_exists( content-objects[
                                    pgmid  = 'CORR'
                                    object = 'MERG' ] ) ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_data_read
          EXPORTING
            textid    = ycx_addict_data_read=>cant_read_data_type
            previous  = diaper
            objectid  = CONV #( me->trkorr )
            data_type = CONV #( TEXT-407 ).
    ENDTRY.
  ENDMETHOD.


  METHOD is_empty.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Is Request completely empty?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-empty = abap_false.
      me->lazy_val-empty = boolc( get_content( ) IS INITIAL ).
      me->lazy_flag-empty = abap_true.
    ENDIF.

    empty = me->lazy_val-empty.
  ENDMETHOD.


  METHOD is_external.
    " Is Request external? """"""""""""""""""""""""""""""""""""""""""
    result = is_request_external( me->trkorr ).
  ENDMETHOD.


  METHOD is_obj_type_class_related.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Is the provided object type related to a class?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ycl_addict_transport_request=>clazy_flag-class_related_obj_tags = abap_false.

      ycl_addict_transport_request=>clazy_val-class_related_obj_tag =
        VALUE #( ( pgmid  = ycl_addict_transport_request=>pgmid-limu
                   object = ycl_addict_transport_request=>object-cinc )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-limu
                   object = ycl_addict_transport_request=>object-clsd )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-limu
                   object = ycl_addict_transport_request=>object-cpri )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-limu
                   object = ycl_addict_transport_request=>object-cpub )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-limu
                   object = ycl_addict_transport_request=>object-meth )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-r3tr
                   object = ycl_addict_transport_request=>object-clas )
                 ( pgmid  = ycl_addict_transport_request=>pgmid-r3tr
                   object = ycl_addict_transport_request=>object-intf ) ).

      ycl_addict_transport_request=>clazy_flag-class_related_obj_tags = abap_true.
    ENDIF.

    related = xsdbool( line_exists( ycl_addict_transport_request=>clazy_val-class_related_obj_tag[
                                    KEY primary_key COMPONENTS
                                    pgmid  = obj_type-pgmid
                                    object = obj_type-object ] ) ).
  ENDMETHOD.


  METHOD is_released.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Is this Request released
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(header) = me->get_header( ).
    result = xsdbool( header-trstatus IN get_released_status_rng( ) ).
  ENDMETHOD.


  METHOD is_request_external.
    " Is the given request external? """"""""""""""""""""""""""""""""
    result = xsdbool( trkorr+0(3) <> sy-sysid ).
  ENDMETHOD.


  METHOD is_toc_safe.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Is this Request safe enough to be included in a ToC?
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF me->lazy_flag-toc_safe = abap_false.
      DATA(rules) = ycl_addict_toolkit=>get_system_rules( ).
      me->lazy_val-toc_safe = rules->is_request_toc_safe( me->trkorr ).
      me->lazy_flag-toc_safe = abap_true.
    ENDIF.

    safe = me->lazy_val-toc_safe.
  ENDMETHOD.


  METHOD release.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Release the Request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR rel_wait_success.

    DATA(wait_success) = VALUE abap_bool_list( ).

    IF del_empty_subtasks = abap_true.
      delete_empty_subtasks( ).
    ENDIF.

    " Release subtasks if wanted """"""""""""""""""""""""""""""""""""
    DATA(max_wait) = COND i( WHEN max_rel_wait IS NOT INITIAL
                             THEN max_rel_wait
                             ELSE ycl_addict_toolkit=>get_system_definitions( )-max_wait ).

    IF rel_subtasks_too = abap_true.
      DATA(sub) = get_subtasks( ).

      LOOP AT sub ASSIGNING FIELD-SYMBOL(<sub>).
        release_single( EXPORTING trkorr              = <sub>-trkorr
                                  req                 = <sub>-obj
                                  wait_until_released = wait_until_released
                                  max_rel_wait        = max_wait
                                  compl_sh_piece_list = compl_sh_piece_list
                        IMPORTING rel_wait_success    = DATA(this_wait_success) ).

        APPEND this_wait_success TO wait_success.
        CLEAR this_wait_success.
      ENDLOOP.
    ENDIF.

    " Release the Request itself """"""""""""""""""""""""""""""""""""
    release_single( EXPORTING trkorr              = me->trkorr
                              req                 = me
                              wait_until_released = wait_until_released
                              max_rel_wait        = max_rel_wait
                              compl_sh_piece_list = compl_sh_piece_list
                    IMPORTING rel_wait_success    = this_wait_success ).

    APPEND this_wait_success TO wait_success.
    CLEAR this_wait_success.

    " Success? """"""""""""""""""""""""""""""""""""""""""""""""""""""
    SORT wait_success BY table_line.
    DELETE ADJACENT DUPLICATES FROM wait_success COMPARING table_line.

    rel_wait_success = xsdbool( wait_until_released = abap_true AND
                                wait_success IS NOT INITIAL AND
                                ( NOT line_exists( wait_success[ table_line = abap_false ] ) ) ).
  ENDMETHOD.


  METHOD release_single.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Releases a single request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA request TYPE REF TO ycl_addict_transport_request.

    CLEAR rel_wait_success.

    " Detect & check request """"""""""""""""""""""""""""""""""""""""
    request = COND #( WHEN req IS INITIAL
                      THEN get_instance( trkorr )
                      ELSE req ).

    IF NOT request->get_header( )-trstatus IN get_open_status_rng( ).
      RETURN.
    ENDIF.

    " Complete piece list """""""""""""""""""""""""""""""""""""""""""
    IF compl_sh_piece_list = abap_true.
      TRY.
          request->complete_shi_piece_list( ).
        CATCH cx_root ##no_handler.
      ENDTRY.
    ENDIF.

    " Release """""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = trkorr
        iv_dialog                  = abap_true
        iv_as_background_job       = abap_false
        iv_success_message         = abap_false
        iv_display_export_log      = abap_false
        iv_simulation              = abap_false
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        error_in_export_methods    = 7
        object_check_error         = 8
        docu_missing               = 9
        db_access_error            = 10
        action_aborted_by_user     = 11
        export_failed              = 12
        OTHERS                     = 13
        ##FM_SUBRC_OK ##NUMBER_OK.

    IF sy-subrc <> 5.
      ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TR_RELEASE_REQUEST' ).
    ENDIF.

    " Wait until released (if wanted) """""""""""""""""""""""""""""""
    IF wait_until_released = abap_true.
      DATA(max_wait) = COND i( WHEN max_rel_wait IS NOT INITIAL
                               THEN max_rel_wait
                               ELSE ycl_addict_toolkit=>get_system_definitions( )-max_wait ).
      DATA(total_wait) = 0.

      DO.
        IF request->has_locked_object( ) = abap_false.
          rel_wait_success = abap_true.
          EXIT.
        ENDIF.

        WAIT UP TO 1 SECONDS.

        ADD 1 TO total_wait.
        CHECK total_wait > max_wait.
        rel_wait_success = abap_false.
        EXIT.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD sort_and_compress.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sort and compress request
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        CALL FUNCTION 'TRINT_SORT_AND_COMPRESS_COMM'
          EXPORTING
            iv_trkorr            = me->trkorr
          EXCEPTIONS
            request_doesnt_exist = 1
            request_released     = 2
            no_authorization     = 3
            update_error         = 4
            OTHERS               = 5
            ##FM_SUBRC_OK.

        IF sy-subrc <> 2.
          ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'TRINT_SORT_AND_COMPRESS_COMM' ).
        ENDIF.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_addict_sort_and_compress
          EXPORTING
            textid   = ycx_addict_sort_and_compress=>soc_function_error
            previous = diaper
            trkorr   = me->trkorr.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
