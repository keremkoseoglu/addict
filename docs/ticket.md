# Ticketing system integration

Addict provides a basic framework to integrate a ticketing system to SAP. This framework can (optionally) be used directly with [TickSys](https://github.com/keremkoseoglu/ticksys).

If you intend to integrate a ticketing system with SAP, the best practice would be:

- Follow the steps described below
- Install [TickSys](https://github.com/keremkoseoglu/ticksys) for further functionality

## Assumptions

This interface generally assumes that:

- There is a ticketing system
- It has APIs which can be called from ABAP
- Each ticket has its unique ID
- Tickets have status codes which can be stringified
- Tickets have assignees
- Tickets have open / closed status

Optional stuff:

- Tickets may have parent-child relations
- Tickets may be linked to each other
- Tickets may be related to SAP TCodes
- Tickets may be related to SAP modules (MM, SD, etc)
- Tickets may be viewed over an URL
- API may enable us to change ticket statuses
- Tickets may have different types (bug, feature, etc)
- Tickets may contain instructions regarding SAP transport requests

## Steps for integration

Basically, you need to create Z-implementations of two Y-interfaces. That's it. Obviously, the ticketing system should have corresponding APIs for the functionalities you need to use.

### `YIF_ADDICT_SYSTEM_RULES`

This interface is responsible to map SAP transport requests with ticket ID's. Implementation guide for this interface is available [here](rules.md).

### `YIF_ADDICT_TICKETING_SYSTEM`

This interface is responsible for the communication between SAP and the ticketing system. You can see the interface [here](https://github.com/keremkoseoglu/addict/blob/main/src/yif_addict_ticketing_system.intf.abap). 

For each ticketing system you have, you need to have distinct implementation of this interface.

A sample [TickSys](https://github.com/keremkoseoglu/ticksys) implementation for Jira can be seen [here](https://github.com/keremkoseoglu/ticksys/blob/main/src/ycl_ticksys_jira.clas.abap).

Here is a list of methods that need to be implemented. Most are optional; depending on your use case.

```
METHODS is_ticket_id_valid 
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(output) TYPE abap_bool
    RAISING   ycx_addict_ticketing_system.
```

Detects if the given ticket ID is valid and returns the result. If this method is called with an invalid ticket ID, it should return SPACE. 

```
METHODS can_set_ticket_to_status 
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
              !status_id    TYPE yd_addict_ticket_status_id
    RETURNING VALUE(result) TYPE abap_bool
    RAISING   ycx_addict_ticketing_system.
```

Determines if the ticket is suitable to be set to the given status code. 

```
METHODS get_ticket_header
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(output) TYPE ysaddict_ticket_header
    RAISING   ycx_addict_ticketing_system.
```

Returns ticket header information. Fields to return:

- Ticket ID
- Ticket description
- Status ID
- Status description
- Parent ticket ID (if exists)
- Ticket open / closed flag
- Ticket type ID
- Ticket type description
- SAP module ID
- SAP module description
- Current ticket assignee username

```abap
METHODS get_transport_instructions
    IMPORTING !ticket_id          TYPE yd_addict_ticket_id
    RETURNING VALUE(instructions) TYPE string_list
    RAISING   ycx_addict_ticketing_system.
```

If tickets have fields for SAP transport instructions, this method should return them per ticket ID. 

```abap
METHODS get_sub_tickets
    IMPORTING !parent         TYPE yd_addict_ticket_id
    RETURNING VALUE(children) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.
```

If the ticketing system supports parent-child relations, this method should return the sub-tickets of the given parent ticket.

```abap
METHODS get_linked_tickets
    IMPORTING !ticket_id     TYPE yd_addict_ticket_id
    RETURNING VALUE(tickets) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.
```

If the ticketing system supports inter-ticket links, this method should return a table of linked tickets of the given ticket ID. 

```abap
METHODS get_related_tcodes
    IMPORTING !ticket_id    TYPE yd_addict_ticket_id
    RETURNING VALUE(tcodes) TYPE tcode_list
    RAISING   ycx_addict_ticketing_system.
```

If the ticketing system contains a list of related SAP TCodes, this method should return them. 

```abap
METHODS get_tickets_related_to_tcodes
    IMPORTING !tcodes        TYPE tcode_list
    RETURNING VALUE(tickets) TYPE ticket_id_list
    RAISING   ycx_addict_ticketing_system.
```

If the ticketing system contains a list of related SAP TCodes, this method should return the tickets related to the given SAP TCode.

```abap
METHODS get_earliest_status
    IMPORTING !statuses       TYPE status_id_list
    RETURNING VALUE(earliest) TYPE status_dict
    RAISING   ycx_addict_ticketing_system.
```

This method should pick the earliest status in the given status list, and return it. 

Hint: If this metadata does not exist in the ticketing system, you can create a Z-table containing status codes & their sequence, and return the earliest status from there.

```abap
METHODS get_tickets_with_status
    IMPORTING !statuses      TYPE status_id_list
              !types         TYPE type_id_list OPTIONAL
    RETURNING VALUE(tickets) TYPE ticket_status_list
    RAISING   ycx_addict_ticketing_system.
```

This method should query the ticketing system and return the tickets corresponding to the given statuses & types.

```abap
METHODS set_ticket_status
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              !status_id TYPE yd_addict_ticket_status_id
    RAISING   ycx_addict_ticketing_system.
```

This method should change the ticket status to the given status ID.

```abap
METHODS set_ticket_assignee
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              assignee  TYPE clike
    RAISING   ycx_addict_ticketing_system.
```

This method should set the ticket assignee to the given username. 

Hint: It is a common practice to map SAP users with ticketing system users over a Z-table. [TickSys](https://github.com/keremkoseoglu/ticksys) has such a functionality already. 

```abap
METHODS set_ticket_assignee_for_status
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
              !status_id TYPE yd_addict_ticket_status_id
    RAISING   ycx_addict_ticketing_system.
```

This method should set the ticket assignee based on the ticket status. For instance; if the ticket is set to "Transported to QA" status, this method should assign the ticket to the user who should run the test.

Hint: It is a common practice to map SAP users with ticketing system users over a Z-table. [TickSys](https://github.com/keremkoseoglu/ticksys) has such a functionality already. 

```abap
METHODS display_ticket
    IMPORTING !ticket_id TYPE yd_addict_ticket_id
    RAISING   ycx_addict_ticketing_system.
```

This method should build the URL of the ticket, and open the ticket in a browser. If the ticketing system is not web based, it should call the executable to display the ticket (if possible).

