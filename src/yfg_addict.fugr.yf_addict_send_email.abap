FUNCTION yf_addict_send_email.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM) TYPE  SYUNAME DEFAULT SY-UNAME
*"     VALUE(TO) TYPE  RSEC_T_USERS OPTIONAL
*"     VALUE(CC) TYPE  RSEC_T_USERS OPTIONAL
*"     VALUE(RLIST) TYPE  YTT_ADDICT_REC_LIST OPTIONAL
*"     VALUE(DLIST) TYPE  YTT_ADDICT_DLIST OPTIONAL
*"     VALUE(SUBJECT) TYPE  SO_OBJ_DES
*"     VALUE(TOLERATE_NO_ADDR) TYPE  XFELD DEFAULT ''
*"     VALUE(BODY) TYPE  BCSY_TEXT
*"     VALUE(BODY_HTML) TYPE  BCSY_TEXT OPTIONAL
*"     VALUE(ATT_BIN) TYPE  YTT_ADDICT_MAIL_ATTACHMENT_BIN OPTIONAL
*"     VALUE(ATT_TXT) TYPE  YTT_ADDICT_MAIL_ATTACHMENT_TXT OPTIONAL
*"     VALUE(ATT_SPOOL) TYPE  YTT_ADDICT_MAIL_ATTACHMENT_SPL OPTIONAL
*"     VALUE(REQUESTED_STATUS) TYPE  BCS_RQST DEFAULT 'E'
*"     VALUE(COMMIT) TYPE  XFELD DEFAULT 'X'
*"     VALUE(LONG_SUBJECT) TYPE  STRING OPTIONAL
*"     VALUE(SENSITIVITY) TYPE  SO_OBJ_SNS OPTIONAL
*"     VALUE(SENDER) TYPE  AD_SMTPADR OPTIONAL
*"  EXCEPTIONS
*"      SEND_EMAIL_ERROR
*"----------------------------------------------------------------------
  TRY.
      ycl_addict_email=>send_email( iv_from             = from
                                    it_to               = to
                                    it_cc               = cc
                                    it_rlist            = rlist
                                    it_dlist            = dlist
                                    iv_subject          = subject
                                    iv_tolerate_no_addr = tolerate_no_addr
                                    it_body             = body
                                    it_body_html        = body_html
                                    it_att_bin          = att_bin
                                    it_att_txt          = att_txt
                                    it_att_spool        = att_spool
                                    iv_requested_status = requested_status
                                    iv_commit           = commit
                                    iv_long_subject     = long_subject
                                    iv_sensitivity      = sensitivity
                                    iv_sender           = sender ).

    CATCH cx_root INTO DATA(diaper).
      DATA(deep_error) = ycl_addict_class=>get_deepest_exception( diaper ).
      MESSAGE deep_error TYPE ycl_simbal=>msgty-error RAISING send_email_error.
  ENDTRY.
ENDFUNCTION.
