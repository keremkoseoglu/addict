# ABAP Development & Dictionary

This repository includes helpful objects regarding ABAP development & dictionary.

## Object index

Significant classes within the package:

<table>
  <tr>
    <td><b>Class</b></td>
    <td><b>Description</b></td>
    <td><b>Relevant T-Code</b></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_ALV</b></td>
    <td>ALV helper</td>
    <td></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_BDC</b></td>
    <td>Batch Input helper</td>
    <td>SM35</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_CLASS</b></td>
    <td>Represents an ABAP class</td>
    <td>SE24</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_DATA_ELEMENT</b></td>
    <td>Represents a data element</td>
    <td>SE11</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_DOMAIN</b></td>
    <td>Represents a domain</td>
    <td>SE11</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_DYNAMIC_ITAB</b></td>
    <td>Dynamic internal table helper</td>
    <td></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_PACKAGE</b></td>
    <td>Represents an ABAP package</td>
    <td>SE80</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_SE01_READER</b></td>
    <td>Reads request status from source system</td>
    <td>SE01</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TABLE</b></td>
    <td>Represents a database table, structure or view</td>
    <td>SE11</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TABLE_FIELD</b></td>
    <td>Represents a field of a table</td>
    <td>SE11</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TADIR_READER</b></td>
    <td>Reads TADIR from the local or remote system</td>
    <td>SE16</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TEXT_TOOLKIT</b></td>
    <td>Provides some useful text utilities</td>
    <td></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TOOLKIT</b></td>
    <td>Provides some useful data dictionary utilities</td>
    <td></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TPALOG_READER</b></td>
    <td>Reads request status from target systems</td>
    <td>STMS</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TRANSPORT_REQUEST</b></td>
    <td>Represents a transport request</td>
    <td>SE01</td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TRANSPORT_REQ_IMP</b></td>
    <td>Imports a transport request in the target system</td>
    <td>STMS</td>
  </tr>
</table>

Most of those objects provide [multition design pattern](https://www.sap-press.com/design-patterns-in-abap-objects_4277/) functionality.

Significant CDS views within the package:

<table>
  <tr>
    <td><b>View</b></td>
    <td><b>Description</b></td>
  </tr>
  <tr>
    <td><b>YV_ADDICT_SYSTEM_DEFINITIONS</b></td>
    <td>Custom ADDICT settings</td>
  </tr>
</table>

## Installation

You may install it to your system using [abapGit](https://github.com/abapGit/abapGit).

If you are using a ticketing system, it is recommended (but not required) to install and implement [TickSys](https://github.com/keremkoseoglu/ticksys) to help with your ticket system management with ABAP.

## Configuration

**ADDICT** needs zero configuration. It will run with its default configuration out of the box. So everything below is optional.

### System rules

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

For such rules, **ADDICT** provides an interface: **YIF_ADDICT_SYSTEM_RULES** . You can create your own Z-Class, implement this interface and fill its following methods:

- **GET_REQUESTS_OF_TICKETS** : This method gets a list of support ticket ID's, and should return a list of transport requests which were created for those tickets. If you don't use a ticketing system, you can ignore this method.
- **IS_REQUEST_TOC_SAFE** : Tells if the given request can safely be put into a ToC or not.

If you implement your own **YIF_ADDICT_SYSTEM_RULES** class, you need to register it into the table **YTADDICT_SYDEF**. Otherwise; **ADDICT** will use the default rule class.

### Customer development objects

**ADDICT** needs to know which ABAP objects are developed by you. By default, **ADDICT** will assume that Y- and Z- objects are developed by yourself. However, if you have custom namespaces or something, you should register those (along Y* Z*) into the table **YTADDICT_NSOBJ** .

### Class definitions

**YTADDICT_CLASS** will allow you to do some optional definitions per ABAP class.

<table>
  <tr>
    <td><b>Field</b></td>
    <td><b>Description</b></td>
    <td><b>Default value</b></td>
  </tr>
  <tr>
    <td><b>CLSNAME</b></td>
    <td>Name of the class</td>
    <td></td>
  </tr>
  <tr>
    <td><b>DISABLE_INHERIT</b></td>
    <td>X = Class will be invisible in inheritance queries of YCL_ADDICT_CLASS </td>
    <td></td>
  </tr>
</table>
