@AbapCatalog.sqlViewName: 'zviatoce070'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Read transports'
@Metadata.ignorePropagatedAnnotations: true
define view zds_abap_toc_transports 
as 
  select from e070
    left outer join e07t on e07t.trkorr = e070.trkorr
    left outer join e070 as sup on sup.trkorr = e070.strkorr
{
  sup.trkorr as main_transport,
  e070.trkorr as transport,
  e070.trfunction as type,
  e070.as4user as owner,
  e070.as4date as creation_date,
  e070.trstatus as status,
  e070.trfunction as function,
  sup.trfunction as main_function,
  e070.korrdev,
  sup.korrdev as main_korrdev,
  case when e070.tarsystem <> '          '
    then e070.tarsystem else sup.tarsystem end
    as target_system,
  e07t.as4text as description
}      

