@AbapCatalog.sqlViewName: 'YVADDICT003'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Otomatik yaratılmış Request''ler'
define view YV_ADDICT_AUTO_REQUESTS
  with parameters
    sy_sysid :sysysid
  as select distinct from YV_ADDICT_E07T_AUTEXTRACT(sy_sysid: $parameters.sy_sysid) as _ext {
      case
  when _ext.trkorr1 is not null then _ext.trkorr1
  when _ext.trkorr2 is not null then _ext.trkorr2
  when _ext.trkorr3 is not null then _ext.trkorr3
  when _ext.trkorr4 is not null then _ext.trkorr4
  when _ext.trkorr5 is not null then _ext.trkorr5
  when _ext.trkorr6 is not null then _ext.trkorr6
  when _ext.trkorr7 is not null then _ext.trkorr7
  when _ext.trkorr8 is not null then _ext.trkorr8
  when _ext.trkorr9 is not null then _ext.trkorr9
  when _ext.trkorr10 is not null then _ext.trkorr10
  when _ext.trkorr11 is not null then _ext.trkorr11
  when _ext.trkorr12 is not null then _ext.trkorr12
  when _ext.trkorr13 is not null then _ext.trkorr13
  when _ext.trkorr14 is not null then _ext.trkorr14
  when _ext.trkorr15 is not null then _ext.trkorr15
  when _ext.trkorr16 is not null then _ext.trkorr16
  when _ext.trkorr17 is not null then _ext.trkorr17
  when _ext.trkorr18 is not null then _ext.trkorr18
  when _ext.trkorr19 is not null then _ext.trkorr19
  when _ext.trkorr20 is not null then _ext.trkorr20
  when _ext.trkorr21 is not null then _ext.trkorr21
  when _ext.trkorr22 is not null then _ext.trkorr22
  when _ext.trkorr23 is not null then _ext.trkorr23
  when _ext.trkorr24 is not null then _ext.trkorr24
  when _ext.trkorr25 is not null then _ext.trkorr25
  when _ext.trkorr26 is not null then _ext.trkorr26
  when _ext.trkorr27 is not null then _ext.trkorr27
  when _ext.trkorr28 is not null then _ext.trkorr28
  when _ext.trkorr29 is not null then _ext.trkorr29
  when _ext.trkorr30 is not null then _ext.trkorr30
  when _ext.trkorr31 is not null then _ext.trkorr31
  when _ext.trkorr32 is not null then _ext.trkorr32
  when _ext.trkorr33 is not null then _ext.trkorr33
  when _ext.trkorr34 is not null then _ext.trkorr34
  when _ext.trkorr35 is not null then _ext.trkorr35
  when _ext.trkorr36 is not null then _ext.trkorr36
  when _ext.trkorr37 is not null then _ext.trkorr37
  when _ext.trkorr38 is not null then _ext.trkorr38
  when _ext.trkorr39 is not null then _ext.trkorr39
  when _ext.trkorr40 is not null then _ext.trkorr40
  when _ext.trkorr41 is not null then _ext.trkorr41
  when _ext.trkorr42 is not null then _ext.trkorr42
  when _ext.trkorr43 is not null then _ext.trkorr43
  when _ext.trkorr44 is not null then _ext.trkorr44
  when _ext.trkorr45 is not null then _ext.trkorr45
  when _ext.trkorr46 is not null then _ext.trkorr46
  when _ext.trkorr47 is not null then _ext.trkorr47
  when _ext.trkorr48 is not null then _ext.trkorr48
  when _ext.trkorr49 is not null then _ext.trkorr49
  when _ext.trkorr50 is not null then _ext.trkorr50
  when _ext.trkorr51 is not null then _ext.trkorr51
  when _ext.trkorr52 is not null then _ext.trkorr52
  when _ext.trkorr53 is not null then _ext.trkorr53
  when _ext.trkorr54 is not null then _ext.trkorr54
  when _ext.trkorr55 is not null then _ext.trkorr55
  when _ext.trkorr56 is not null then _ext.trkorr56
  when _ext.trkorr57 is not null then _ext.trkorr57
  when _ext.trkorr58 is not null then _ext.trkorr58
  when _ext.trkorr59 is not null then _ext.trkorr59
  when _ext.trkorr60 is not null then _ext.trkorr60
  end as trkorr
}