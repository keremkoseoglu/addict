*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
    DEFINE html_hdr.
      CLEAR  ls_objtxt.
      CONCATENATE  &1
             '<td style="font-size:12px;" '
             ' "font-family:Tahoma,geneva,sans-serif;"'
             ' width: 100%;" align = "LEFT"  BGCOLOR = "#213B5A">' '<FONT COLOR = "WHITE"> <B>'
              &2
             '</B> </FONT> '
             '</td>'  INTO ls_objtxt-line SEPARATED BY space.

      APPEND ls_objtxt TO lt_objtxt.
    END-OF-DEFINITION.


    DEFINE html_itm.
      CLEAR  ls_objtxt.
      WRITE &2 TO ls_objtxt-line LEFT-JUSTIFIED.
      CONCATENATE  &1
             '<td style="font-size:12px;"'
             ' "font-family:Tahoma,geneva,sans-serif;"'
             ' width: 100%;" align = "LEFT"   BGCOLOR = "#E6EDF6" >' '<FONT COLOR = "BLACK">'
              ls_objtxt-line
             '</FONT>' '</td>' INTO ls_objtxt-line SEPARATED BY space  ##NO_TEXT.

      APPEND ls_objtxt TO lt_objtxt.
    END-OF-DEFINITION.

    DEFINE html_body_txt.
      CLEAR  ls_objtxt.
      CONCATENATE '<p style="font-size:12px;">'
*                  '<h6>'
                  &1
*                  '</h6>'
                  '</p>'
                  INTO ls_objtxt-line SEPARATED BY space.
      APPEND ls_objtxt TO lt_objtxt.
    END-OF-DEFINITION.
