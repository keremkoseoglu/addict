# ABAP Development & Dictionary

This repository includes helpful objects regarding ABAP development & dictionary.

## Object index

Here is a list of significant classes within the package:

<table>
  <tr>
    <td><b>Class</b></td>
    <td><b>Description</b></td>
    <td><b>Relevant T-Code</b></td>
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
    <td>Reads request status from various systems</td>
    <td></td>
  </tr>
  <tr>
    <td><b>YCL_ADDICT_TRANSPORT_REQUEST</b></td>
    <td>Represents a transport request</td>
    <td>SE01</td>
  </tr>
</table>

Most of those objects provide [multition design pattern](https://www.sap-press.com/design-patterns-in-abap-objects_4277/) functionality.

## Installation

You may install it to your system using [abapGit](https://github.com/abapGit/abapGit).

## Configuration

**ADDICT** needs zero configuration. It will run with its default configuration out of the box.

However, each system has its own rules. If you need to change the behavior of **ADDICT**, you can add a new entry to the table **YTADDICT_SYDEF** (SM30) with the following values:

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

**SYSID** is mandantory. Other fields are not. If you leave a field empty, **ADDICT** will assume its default value.

### Rule Class

Rules, which can be represented with a single value, are stored in the table **YTADDICT_SYDEF** ; as explained above.

However, some rules correspond to complex behavior, which need to be coded as ABAP methods.

For such rules, **ADDICT** provides an interface: **YIF_ADDICT_SYSTEM_RULES** . You can create your own Z-Class, implement this interface and fill its following methods:

- **GET_REQUESTS_OF_TICKETS** : This method gets a list of support ticket ID's, and should return a list of transport requests which were created for those tickets. If you don't use a ticket system, you can ignore this method.

If you implement your own **YIF_ADDICT_SYSTEM_RULES** class, you need to register it into the table **YTADDICT_SYDEF**. Otherwise; **ADDICT** will use the default rule class.
