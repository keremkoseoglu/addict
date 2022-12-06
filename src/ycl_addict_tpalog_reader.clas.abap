CLASS ycl_addict_tpalog_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF date_range_dict,
             begda TYPE begda,
             endda TYPE endda,
           END OF date_range_dict.

    TYPES: BEGIN OF output_dict,
             trkorr     TYPE trkorr,
             as4text    TYPE as4text,
             trfunction TYPE trfunction,
             tarsystem  TYPE tarsystem,
             status     TYPE yd_addict_request_status,
             trdate     TYPE dats,
             trtime     TYPE tims,
             as4user    TYPE tr_as4user,
             admin      TYPE yd_addict_tpalog_admin,
           END OF output_dict,

           output_list TYPE STANDARD TABLE OF output_dict WITH EMPTY KEY.

    TYPES: BEGIN OF sys_data_dict,
             sysid TYPE sysysid,
             list  TYPE output_list,
           END OF sys_data_dict,

           sys_data_set TYPE HASHED TABLE OF sys_data_dict
                        WITH UNIQUE KEY primary_key COMPONENTS sysid.

    CONSTANTS: BEGIN OF status,
                 error        TYPE icon_d   VALUE '@5C@',
                 error_txt    TYPE iconname VALUE 'ICON_LED_RED',
                 imported     TYPE icon_d   VALUE '@K5@',
                 imported_txt TYPE iconname VALUE 'ICON_IMPORT_TRANSPORT_REQUEST',
                 missing      TYPE icon_d   VALUE '@BZ@',
                 missing_txt  TYPE iconname VALUE 'ICON_LED_INACTIVE',
                 open         TYPE icon_d   VALUE '@E1@',
                 open_txt     TYPE iconname VALUE 'ICON_ENVELOPE_OPEN',
                 released     TYPE icon_d   VALUE '@4A@',
                 released_txt TYPE iconname VALUE 'ICON_TRANSPORT',
                 unknown      TYPE icon_d   VALUE '@1C@',
                 unknown_txt  TYPE iconname VALUE 'ICON_MESSAGE_QUESTION',
                 waiting      TYPE icon_d   VALUE '@1T@',
                 waiting_txt  TYPE iconname VALUE 'ICON_TIME',
               END OF status.

    CLASS-METHODS class_constructor.

    CLASS-METHODS format_ticket_id_input
      CHANGING !ticket_ids TYPE yif_addict_system_rules=>ticket_id_list.

    METHODS get_list
      IMPORTING !rfcdest          TYPE rfcdest
                !trkorr_rng       TYPE cts_organizer_tt_wd_request OPTIONAL
                !ticket_ids       TYPE yif_addict_system_rules=>ticket_id_list OPTIONAL
                !sys_data         TYPE sys_data_set OPTIONAL
                !date_range       TYPE date_range_dict OPTIONAL
                !read_master      TYPE abap_bool DEFAULT abap_false
                !only_major_steps TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(list)       TYPE output_list
      RAISING   ycx_addict_tpalog_read.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF master_dict,
             trkorr     TYPE e070-trkorr,
             trfunction TYPE e070-trfunction,
             tarsystem  TYPE e070-tarsystem,
             as4text    TYPE e07t-as4text,
           END OF master_dict,

           master_set TYPE HASHED TABLE OF master_dict WITH UNIQUE KEY primary_key COMPONENTS trkorr.

    TYPES tpalog_list   TYPE STANDARD TABLE OF tpalog WITH DEFAULT KEY.
    TYPES tpalog_s_list TYPE STANDARD TABLE OF tpalog WITH DEFAULT KEY WITH NON-UNIQUE SORTED KEY k1 COMPONENTS trkorr retcode.

    TYPES: BEGIN OF trkorr_dict,
             trkorr TYPE tpalog-trkorr,
             trtime TYPE tpalog-trtime,
             admin  TYPE tpalog-admin,
           END OF trkorr_dict,

           trkorr_list TYPE STANDARD TABLE OF trkorr_dict WITH DEFAULT KEY.

    CONSTANTS trkorr_size TYPE i VALUE 100.

    CONSTANTS: BEGIN OF trstep,
                 main_import TYPE tpalog-trstep VALUE 'I',
                 activation  TYPE tpalog-trstep VALUE 'A',
                 generation  TYPE tpalog-trstep VALUE 'G',
               END OF trstep.

    CLASS-DATA major_trstep_rng TYPE RANGE OF tpalog-trstep.

    DATA date_range          TYPE date_range_dict.
    DATA list                TYPE output_list.
    DATA sys_data            TYPE sys_data_set.
    DATA tpalog              TYPE tpalog_s_list.
    DATA trkorr_rng          TYPE cts_organizer_tt_wd_request.
    DATA rfcdest             TYPE rfcdest.
    DATA readable_trstep_rng TYPE RANGE OF tpalog-trstep.

    METHODS get_req_date
      IMPORTING !trtime     TYPE tpalog-trtime
      RETURNING VALUE(date) TYPE dats.

    METHODS get_req_status
      IMPORTING !trkorr       TYPE tpalog-trkorr
      RETURNING VALUE(status) TYPE yd_addict_request_status.

    METHODS get_req_time
      IMPORTING !trtime     TYPE tpalog-trtime
      RETURNING VALUE(time) TYPE tims.

    METHODS parse_tpalog.
    METHODS read_master.
    METHODS read_tpalog RAISING ycx_addict_tpalog_read.

    METHODS read_tpalog_for_requests
      IMPORTING !trkorr TYPE trkorr_list
      RAISING   ycx_addict_function_subrc.

    METHODS req_has_retcode
      IMPORTING !trkorr    TYPE tpalog-trkorr
                !retcode   TYPE tpalog-retcode
      RETURNING VALUE(has) TYPE abap_bool.
ENDCLASS.



CLASS ycl_addict_tpalog_reader IMPLEMENTATION.


  METHOD class_constructor.
    ycl_addict_tpalog_reader=>major_trstep_rng =
      VALUE #( sign   = ycl_addict_toolkit=>sign-include
               option = ycl_addict_toolkit=>option-eq
               ( low  = ycl_addict_tpalog_reader=>trstep-main_import )
               ( low  = ycl_addict_tpalog_reader=>trstep-activation )
               ( low  = ycl_addict_tpalog_reader=>trstep-generation ) ).
  ENDMETHOD.


  METHOD format_ticket_id_input.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " String-format ticket ID's
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT ticket_ids ASSIGNING FIELD-SYMBOL(<ticket_id>).
      CONDENSE <ticket_id>.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_list.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Main method
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR me->list.

    me->date_range          = date_range.
    me->sys_data            = sys_data.
    me->trkorr_rng          = trkorr_rng.
    me->rfcdest             = rfcdest.
    me->readable_trstep_rng = SWITCH #( only_major_steps
                                        WHEN abap_true
                                        THEN me->major_trstep_rng
                                        ELSE VALUE #( ) ).

    read_tpalog( ).
    parse_tpalog( ).

    IF read_master = abap_true.
      read_master( ).
    ENDIF.

    list = me->list.
  ENDMETHOD.


  METHOD get_req_date.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the request date
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    date = trtime+0(8).
  ENDMETHOD.


  METHOD get_req_status.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns request status
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    status = COND #(
        WHEN req_has_retcode( trkorr  = trkorr
                              retcode = '0012' )
        THEN me->status-error

        WHEN req_has_retcode( trkorr  = trkorr
                              retcode = '0008' )
        THEN me->status-error

        WHEN req_has_retcode( trkorr  = trkorr
                              retcode = '0004' )
        THEN me->status-imported

        WHEN req_has_retcode( trkorr  = trkorr
                              retcode = '0000' )
        THEN me->status-imported

        ELSE me->status-unknown ).
  ENDMETHOD.


  METHOD get_req_time.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the request time
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    time = trtime+8(6).
  ENDMETHOD.


  METHOD parse_tpalog.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Parse data gathered from TPALOG
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR me->list.

    DATA(unique_trkorrs) = CORRESPONDING trkorr_list( me->tpalog ).
    SORT unique_trkorrs BY trkorr ASCENDING
                           trtime DESCENDING.
    DELETE ADJACENT DUPLICATES FROM unique_trkorrs COMPARING trkorr.

    me->list = VALUE #( FOR _tr IN unique_trkorrs
                        ( trkorr = _tr-trkorr
                          status = get_req_status( _tr-trkorr )
                          trdate = get_req_date( _tr-trtime )
                          trtime = get_req_time( _tr-trtime )
                          admin  = _tr-admin ) ).
  ENDMETHOD.


  METHOD read_master.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Read request master data
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->list IS NOT INITIAL.
    DATA(master) = VALUE master_set( ).

    SELECT e070~trkorr, e070~trfunction, e070~tarsystem,
           e07t~as4text
           FROM e070
                LEFT JOIN e07t ON e07t~trkorr = e070~trkorr AND
                                  e07t~langu  = @sy-langu
        FOR ALL ENTRIES IN @me->list
        WHERE e070~trkorr = @me->list-trkorr
        INTO CORRESPONDING FIELDS OF TABLE @master.

    LOOP AT me->list ASSIGNING FIELD-SYMBOL(<list>).
      ASSIGN master[ KEY primary_key
                     COMPONENTS trkorr = <list>-trkorr
                   ] TO FIELD-SYMBOL(<master>).

      CHECK sy-subrc = 0.

      <list>-as4text    = <master>-as4text.
      <list>-trfunction = <master>-trfunction.
      <list>-tarsystem  = <master>-tarsystem.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_tpalog.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Reads TPALOG from the target system
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA trkorr TYPE trkorr_list.

    TRY.
        " ______________________________
        " Preparation
        " ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
        CLEAR me->tpalog.

        " ______________________________
        " Add requests to where condition
        " ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
        ASSIGN me->sys_data[ KEY primary_key COMPONENTS
                             sysid = sy-sysid
                           ] TO FIELD-SYMBOL(<sys_data>).
        IF sy-subrc = 0.
          APPEND LINES OF CORRESPONDING trkorr_list( <sys_data>-list ) TO trkorr.
        ENDIF.

        IF me->trkorr_rng IS NOT INITIAL.
          SELECT trkorr FROM e070
                 WHERE trkorr IN @me->trkorr_rng
                 APPENDING CORRESPONDING FIELDS OF TABLE @trkorr ##TOO_MANY_ITAB_FIELDS .
        ENDIF.

        IF trkorr IS INITIAL.
          read_tpalog_for_requests( trkorr ).
          RETURN.
        ENDIF.

        WHILE trkorr IS NOT INITIAL.
          DATA(trkorr_subset) = VALUE trkorr_list( ).

          LOOP AT trkorr ASSIGNING FIELD-SYMBOL(<trkorr>).
            APPEND <trkorr> TO trkorr_subset.
            DELETE trkorr.
            IF lines( trkorr_subset ) >= me->trkorr_size.
              EXIT.                                     "#EC CI_NOORDER
            ENDIF.
          ENDLOOP.

          read_tpalog_for_requests( trkorr_subset ).
          CLEAR trkorr_subset.
        ENDWHILE.

      CATCH ycx_addict_tpalog_read INTO DATA(lo_tr).
        RAISE EXCEPTION lo_tr.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE ycx_addict_tpalog_read
          EXPORTING
            textid   = ycx_addict_tpalog_read=>read_error
            previous = lo_diaper
            rfcdest  = me->rfcdest.
    ENDTRY.
  ENDMETHOD.


  METHOD read_tpalog_for_requests.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " TPALOG tablosunu iletilen Request'ler için okur
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA dat    TYPE STANDARD TABLE OF tab512.
    DATA fld    TYPE STANDARD TABLE OF rfc_db_fld.
    DATA opt    TYPE STANDARD TABLE OF rfc_db_opt.
    DATA tpalog TYPE tpalog_list.

    DATA(trkorr_appended) = abap_false.

    LOOP AT trkorr ASSIGNING FIELD-SYMBOL(<trkorr>).
      IF trkorr_appended = abap_false.
        APPEND VALUE #( text = '(' ) TO opt.
        trkorr_appended = abap_true.
      ELSE.
        APPEND VALUE #( text = 'OR' ) TO opt.
      ENDIF.

      APPEND VALUE #( text = |TRKORR = '{ <trkorr>-trkorr }'| ) TO opt.
    ENDLOOP.

    IF trkorr_appended = abap_true.
      APPEND VALUE #( text = ')' ) TO opt.
    ENDIF.

    " ______________________________
    " Add date range to where condition
    " ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
    IF me->date_range IS NOT INITIAL.
      ASSERT me->date_range-begda IS NOT INITIAL AND
             me->date_range-endda IS NOT INITIAL AND
             me->date_range-begda <= me->date_range-endda.

      IF opt IS INITIAL.
        APPEND VALUE #( text = '(' ) TO opt.
      ELSE.
        APPEND VALUE #( text = 'AND (' ) TO opt.
      ENDIF.

      APPEND VALUE #( text = |TRTIME >= { me->date_range-begda }000000| ) TO opt.
      APPEND VALUE #( text = |AND TRTIME <= { me->date_range-endda }235959| ) TO opt.
      APPEND VALUE #( text = ')' ) TO opt.
    ENDIF.

    " ______________________________
    " Read
    " ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION me->rfcdest
      EXPORTING
        query_table          = 'TPALOG'
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

    tpalog = dat.

    APPEND LINES OF VALUE tpalog_s_list( FOR _t IN tpalog
                                         WHERE ( trstep IN me->readable_trstep_rng )
                                         ( _t ) )
           TO me->tpalog.

    IF 1 = 0. " Where Used List
      SELECT SINGLE trkorr FROM tpalog INTO @DATA(dummy). "#EC CI_GENBUFF
    ENDIF.
  ENDMETHOD.


  METHOD req_has_retcode.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tells if the provided request has the return code
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(latest_tpstat_key) =
      REDUCE tpalog-tpstat_key( INIT _tt TYPE tpalog-tpstat_key
                                FOR _tpalog IN me->tpalog
                                USING KEY k1
                                WHERE ( trkorr =  trkorr AND
                                        trstep IN me->major_trstep_rng )
                                NEXT _tt = COND #( WHEN _tt IS INITIAL THEN _tpalog-tpstat_key
                                                   WHEN _tt < _tpalog-tpstat_key THEN _tpalog-tpstat_key
                                                   ELSE _tt ) ).


    LOOP AT me->tpalog TRANSPORTING NO FIELDS
         USING KEY k1
         WHERE tpstat_key =  latest_tpstat_key     AND
               trkorr     =  trkorr                AND
               trstep     IN me->major_trstep_rng  AND
               retcode    =  retcode.

      has = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
