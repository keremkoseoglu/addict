*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE mac_msg_set_from_symsg.
  &1-msgid = sy-msgid.
  &1-msgty = sy-msgty.
  &1-msgno = sy-msgno.
  &1-msgv1 = sy-msgv1.
  &1-msgv2 = sy-msgv2.
  &1-msgv3 = sy-msgv3.
  &1-msgv4 = sy-msgv4.
END-OF-DEFINITION.

DEFINE mac_symsg_set_from_msg.
  sy-msgid = &1-msgid.                                    "#EC WRITE_OK
  sy-msgty = &1-msgty.                                    "#EC WRITE_OK
  sy-msgno = &1-msgno.                                    "#EC WRITE_OK
  sy-msgv1 = &1-msgv1.                                    "#EC WRITE_OK
  sy-msgv2 = &1-msgv2.                                    "#EC WRITE_OK
  sy-msgv3 = &1-msgv3.                                    "#EC WRITE_OK
  sy-msgv4 = &1-msgv4.                                    "#EC WRITE_OK
END-OF-DEFINITION.

DEFINE mac_symsg_raise.
  if sy-msgty <> '-'.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising &1.
  endif.
END-OF-DEFINITION.
