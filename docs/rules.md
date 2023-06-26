# System rules

Each system has its own rules. If you need to change the behavior of **ADDICT** according to the rules of your own system, you have two options.

Rules, which can be represented with a single value, are stored in the table **YTADDICT_SYDEF** (SM30). You can add a new entry to the table **YTADDICT_SYDEF** (SM30) with the following values:

<table>
  <tr>
    <td><b>Field</b></td>
    <td><b>Description</b></td>
    <td><b>Default value</b></td>
  </tr>
  <tr>
    <td><b>SYSID</b></td>
    <td>ID of your system, found in SY-SYSID</td>
    <td></td>
  </tr>
  <tr>
    <td><b>MAX_WAIT</b></td>
    <td>Max wait time before timeout (in seconds)</td>
    <td>30</td>
  </tr>
  <tr>
    <td><b>AUTO_REQUEST_PREFIX</b></td>
    <td>The default text prefix of an automatically created request</td>
    <td>Auto</td>
  </tr>
  <tr>
    <td><b>RULE_CLASS</b></td>
    <td>Your rule class implementing complex behavior - see below for details</td>
    <td>YCL_ADDICT_DEF_SYSTEM_RULES</td>
  </tr>
</table>

Default values are coded into **YV_ADDICT_SYSTEM_DEFINITIONS**.

However, some rules correspond to complex behavior, which need to be coded as ABAP methods.

For such rules, **ADDICT** provides an interface: **YIF_ADDICT_SYSTEM_RULES** . You can create your own Z-Class, implement this interface and fill its methods.

If you implement your own **YIF_ADDICT_SYSTEM_RULES** class, you need to register it into the table **YTADDICT_SYDEF**. Otherwise; **ADDICT** will use the default rule class.

## `YIF_ADDICT_SYSTEM_RULES`

You can see the interface [here](https://github.com/keremkoseoglu/addict/blob/main/src/yif_addict_system_rules.intf.abap ). 

A sample implementation can be seen [here](https://github.com/keremkoseoglu/addict/blob/main/src/ycl_addict_def_system_rules.clas.abap).

```abap
METHODS get_requests_of_tickets
    IMPORTING !ticket_keys    TYPE ticket_key_list
    RETURNING VALUE(requests) TYPE trkorr_list
    RAISING   ycx_addict_class_method.
```

This method should return a list of transport requests which belong to the given ticket ID's. 

Hint: A suggested best practice is to put the ticket ID into the transport request description. Preferably as the first "n" characters. You can also put the request numbers into tickets, or use a Z-table - but those are less convenient.

- If you are entering ticket ID's into transport request descriptions, you can query E7* tables.
- If you are entering request numbers into tickets, you should call a ticketing system API and query tickets to build the request list.
- If you are mapping requests - tickets through a Z-table, you should query the Z-table.

```abap
METHODS is_request_toc_safe
    IMPORTING !trkorr     TYPE trkorr
    RETURNING VALUE(safe) TYPE abap_bool
    RAISING   ycx_addict_class_method.
```

Determines if the request is ToC-Safe or not (Transport of Copies). Generally, you should return ABAP_TRUE here. 

Case: You might have multiple clients on your development system. Client 100 is the main client used for development + customizing, while client 220 is used by the basis team for authorization. In that case, requests belonging to client 220 are not ToC-Safe. You shouldn't build a ToC on client 100 and include a request from client 220 - if you do that, you will transport an empty authorization schema and delete the authorizations on the target system.
