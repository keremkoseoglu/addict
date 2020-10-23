@AbapCatalog.sqlViewName: 'YVADDICT004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Otomatik yaratılmış Request''ler için Extract'
define view YV_ADDICT_E07T_AUTEXTRACT
  with parameters
    sy_sysid :sysysid
  as select from    YV_ADDICT_SYSTEM_DEFINITIONS(sy_sysid: $parameters.sy_sysid) as _sydef
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex1 on
    _yex1.as4text_1 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex2 on
    _yex2.as4text_2 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex3 on
    _yex3.as4text_3 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex4 on
    _yex4.as4text_4 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex5 on
    _yex5.as4text_5 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex6 on
    _yex6.as4text_6 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex7 on
    _yex7.as4text_7 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex8 on
    _yex8.as4text_8 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex9 on
    _yex9.as4text_9 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex10 on
    _yex10.as4text_10 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex11 on
    _yex11.as4text_11 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex12 on
    _yex12.as4text_12 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex13 on
    _yex13.as4text_13 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex14 on
    _yex14.as4text_14 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex15 on
    _yex15.as4text_15 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex16 on
    _yex16.as4text_16 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex17 on
    _yex17.as4text_17 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex18 on
    _yex18.as4text_18 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex19 on
    _yex19.as4text_19 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex20 on
    _yex20.as4text_20 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex21 on
    _yex21.as4text_21 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex22 on
    _yex22.as4text_22 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex23 on
    _yex23.as4text_23 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex24 on
    _yex24.as4text_24 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex25 on
    _yex25.as4text_25 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex26 on
    _yex26.as4text_26 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex27 on
    _yex27.as4text_27 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex28 on
    _yex28.as4text_28 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex29 on
    _yex29.as4text_29 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex30 on
    _yex30.as4text_30 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex31 on
    _yex31.as4text_31 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex32 on
    _yex32.as4text_32 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex33 on
    _yex33.as4text_33 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex34 on
    _yex34.as4text_34 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex35 on
    _yex35.as4text_35 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex36 on
    _yex36.as4text_36 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex37 on
    _yex37.as4text_37 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex38 on
    _yex38.as4text_38 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex39 on
    _yex39.as4text_39 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex40 on
    _yex40.as4text_40 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex41 on
    _yex41.as4text_41 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex42 on
    _yex42.as4text_42 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex43 on
    _yex43.as4text_43 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex44 on
    _yex44.as4text_44 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex45 on
    _yex45.as4text_45 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex46 on
    _yex46.as4text_46 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex47 on
    _yex47.as4text_47 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex48 on
    _yex48.as4text_48 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex49 on
    _yex49.as4text_49 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex50 on
    _yex50.as4text_50 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex51 on
    _yex51.as4text_51 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex52 on
    _yex52.as4text_52 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex53 on
    _yex53.as4text_53 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex54 on
    _yex54.as4text_54 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex55 on
    _yex55.as4text_55 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex56 on
    _yex56.as4text_56 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex57 on
    _yex57.as4text_57 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex58 on
    _yex58.as4text_58 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex59 on
    _yex59.as4text_59 = _sydef.auto_request_prefix
    left outer join YV_ADDICT_E07T_TEXTRACT                                      as _yex60 on
    _yex60.as4text_60 = _sydef.auto_request_prefix {
  _yex1.trkorr as trkorr1,
  _yex2.trkorr as trkorr2,
  _yex3.trkorr as trkorr3,
  _yex4.trkorr as trkorr4,
  _yex5.trkorr as trkorr5,
  _yex6.trkorr as trkorr6,
  _yex7.trkorr as trkorr7,
  _yex8.trkorr as trkorr8,
  _yex9.trkorr as trkorr9,
  _yex10.trkorr as trkorr10,
  _yex11.trkorr as trkorr11,
  _yex12.trkorr as trkorr12,
  _yex13.trkorr as trkorr13,
  _yex14.trkorr as trkorr14,
  _yex15.trkorr as trkorr15,
  _yex16.trkorr as trkorr16,
  _yex17.trkorr as trkorr17,
  _yex18.trkorr as trkorr18,
  _yex19.trkorr as trkorr19,
  _yex20.trkorr as trkorr20,
  _yex21.trkorr as trkorr21,
  _yex22.trkorr as trkorr22,
  _yex23.trkorr as trkorr23,
  _yex24.trkorr as trkorr24,
  _yex25.trkorr as trkorr25,
  _yex26.trkorr as trkorr26,
  _yex27.trkorr as trkorr27,
  _yex28.trkorr as trkorr28,
  _yex29.trkorr as trkorr29,
  _yex30.trkorr as trkorr30,
  _yex31.trkorr as trkorr31,
  _yex32.trkorr as trkorr32,
  _yex33.trkorr as trkorr33,
  _yex34.trkorr as trkorr34,
  _yex35.trkorr as trkorr35,
  _yex36.trkorr as trkorr36,
  _yex37.trkorr as trkorr37,
  _yex38.trkorr as trkorr38,
  _yex39.trkorr as trkorr39,
  _yex40.trkorr as trkorr40,
  _yex41.trkorr as trkorr41,
  _yex42.trkorr as trkorr42,
  _yex43.trkorr as trkorr43,
  _yex44.trkorr as trkorr44,
  _yex45.trkorr as trkorr45,
  _yex46.trkorr as trkorr46,
  _yex47.trkorr as trkorr47,
  _yex48.trkorr as trkorr48,
  _yex49.trkorr as trkorr49,
  _yex50.trkorr as trkorr50,
  _yex51.trkorr as trkorr51,
  _yex52.trkorr as trkorr52,
  _yex53.trkorr as trkorr53,
  _yex54.trkorr as trkorr54,
  _yex55.trkorr as trkorr55,
  _yex56.trkorr as trkorr56,
  _yex57.trkorr as trkorr57,
  _yex58.trkorr as trkorr58,
  _yex59.trkorr as trkorr59,
  _yex60.trkorr as trkorr60
}