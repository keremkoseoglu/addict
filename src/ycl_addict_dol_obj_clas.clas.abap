CLASS ycl_addict_dol_obj_clas DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_dol_obj .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_ADDICT_DOL_OBJ_CLAS IMPLEMENTATION.


  METHOD yif_addict_dol_obj~get_ddtext.
    SELECT SINGLE descript FROM seoclasstx
           WHERE clsname = @obj_name AND
                 langu   = @sy-langu
           INTO @output.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~get_object_txt.
    output = TEXT-452.
  ENDMETHOD.


  METHOD yif_addict_dol_obj~is_deleted.
    SELECT SINGLE clsname FROM seoclass
           WHERE clsname = @obj_name
           INTO @DATA(seoclass_dummy).

    output = boolc( sy-subrc <> 0 ).
  ENDMETHOD.
ENDCLASS.
