# Configuration

**ADDICT** needs zero configuration. It will run with its default configuration out of the box. So everything below is optional.

## System rules

Check the details of system rules [here](rules.md).

## Customer development objects

**ADDICT** needs to know which ABAP objects are developed by you. By default, **ADDICT** will assume that Y- and Z- objects are developed by yourself. However, if you have custom namespaces or something, you should register those (along Y* Z*) into the table **YTADDICT_NSOBJ** .

## Class definitions

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