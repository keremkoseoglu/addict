INTERFACE yif_addict_dol_obj
  PUBLIC .

  CONSTANTS: BEGIN OF class,
               me TYPE seoclsname VALUE 'YIF_ADDICT_DOL_OBJ',
             END OF class.

  CONSTANTS: BEGIN OF pgmid,
               limu TYPE pgmid VALUE 'LIMU',
               r3tr TYPE pgmid VALUE 'R3TR',
             END OF pgmid.

  METHODS get_object_txt
    IMPORTING
      !pgmid        TYPE ysaddict_dol_list-pgmid DEFAULT pgmid-r3tr
      !object       TYPE ysaddict_dol_list-object
    RETURNING
      VALUE(output) TYPE ysaddict_dol_list-object_txt.

  METHODS get_ddtext
    IMPORTING
      !pgmid        TYPE ysaddict_dol_list-pgmid DEFAULT pgmid-r3tr
      !object       TYPE ysaddict_dol_list-object
      !obj_name     TYPE ysaddict_dol_list-obj_name
    RETURNING
      VALUE(output) TYPE ysaddict_dol_list-ddtext.

  METHODS is_deleted
    IMPORTING
      !pgmid        TYPE ysaddict_dol_list-pgmid DEFAULT pgmid-r3tr
      !object       TYPE ysaddict_dol_list-object
      !obj_name     TYPE ysaddict_dol_list-obj_name
    RETURNING
      VALUE(output) TYPE abap_bool.

ENDINTERFACE.
