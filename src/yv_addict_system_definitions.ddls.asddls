@AbapCatalog.sqlViewName: 'YVADDICT001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sistem tanımları'
@ClientDependent:false
define view YV_ADDICT_SYSTEM_DEFINITIONS
  with parameters
    sy_sysid :sysysid
  as select from    t000
    left outer join ytaddict_sydef as _sydef on
    _sydef.mandt = t000.mandt                and
    _sydef.sysid = :sy_sysid {
  key t000.mandt,
  key :sy_sysid as sysid,
  case when _sydef.rule_class is not null then _sydef.rule_class else cast('YCL_ADDICT_DEF_SYSTEM_RULES' as YD_ADDICT_RULE_CLASS) end as rule_class,
  case when _sydef.max_wait is not null then _sydef.max_wait else 30 end as max_wait,
  case when _sydef.auto_request_prefix is not null then _sydef.auto_request_prefix else cast('(Auto)' as YD_ADDICT_AUTO_REQUEST_PREFIX) end as auto_request_prefix
}
where
  t000.mandt = $session.client
