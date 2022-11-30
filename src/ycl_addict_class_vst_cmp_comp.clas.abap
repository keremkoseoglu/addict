CLASS ycl_addict_class_vst_cmp_comp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES yif_addict_class_visitor.

    CLASS-METHODS do_classes_have_meth_diff
      IMPORTING !clsnames      TYPE ycl_addict_class=>clsname_list
                !meth_name_rng TYPE ycl_addict_class=>cmpname_range
      RETURNING VALUE(diff)    TYPE abap_bool
      RAISING   ycx_addict_class_method
                ycx_addict_table_content.

    METHODS constructor IMPORTING !param TYPE ycl_addict_class=>component_param_dict.
    METHODS has_delta RETURNING VALUE(has) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF comp_dict,
             clsname   TYPE seoclsname,
             component TYPE ycl_addict_class=>component_sort,
           END OF comp_dict,

           comp_list TYPE STANDARD TABLE OF comp_dict WITH EMPTY KEY.

    DATA param TYPE ycl_addict_class=>component_param_dict.
    DATA comps TYPE comp_list.
ENDCLASS.



CLASS YCL_ADDICT_CLASS_VST_CMP_COMP IMPLEMENTATION.


  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Object creation
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->param = param.
  ENDMETHOD.


  METHOD do_classes_have_meth_diff.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Compares classes against different methods
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA visitor TYPE REF TO yif_addict_class_visitor.

    DATA(comp) = NEW ycl_addict_class_vst_cmp_comp( VALUE #(
        cmpname_rng = meth_name_rng
        cmptype_rng = VALUE #( ( option = ycl_addict_toolkit=>option-eq
                                 sign   = ycl_addict_toolkit=>sign-include
                                 low    = ycl_addict_class=>cmptype-method ) ) ) ).

    visitor ?= comp.

    LOOP AT clsnames ASSIGNING FIELD-SYMBOL(<clsname>).
      ycl_addict_class=>get_instance( <clsname> )->accept( visitor ).
    ENDLOOP.

    diff = comp->has_delta( ).
  ENDMETHOD.


  METHOD has_delta.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Compares components
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT me->comps ASSIGNING FIELD-SYMBOL(<comp_curr>).
      DATA(tabix_curr) = sy-tabix.

      ASSIGN me->comps[ tabix_curr + 1 ] TO FIELD-SYMBOL(<comp_next>).

      CHECK sy-subrc = 0 AND
            <comp_curr>-component <> <comp_next>-component.

      has = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD yif_addict_class_visitor~visit.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Visits class
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ASSIGN me->comps[ clsname = class->def-clsname ] TO FIELD-SYMBOL(<comp>).

    IF sy-subrc <> 0.
      APPEND VALUE #( clsname = class->def-clsname )
             TO me->comps ASSIGNING <comp>.
    ENDIF.

    class->get_components(
      EXPORTING param    = me->param
      IMPORTING cmp_sort = <comp>-component ).
  ENDMETHOD.
ENDCLASS.
