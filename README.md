# ABAP Development & Dictionary

This repository includes helpful objects regarding ABAP dictionary.

## Installation

You may install it to your system using [abapGit](https://github.com/abapGit/abapGit).

## Configuration

**ADDICT** needs zero configuration. It will run with its default configuration out of the box.

However, each system has its own rules. If you need to change the behavior of **ADDICT**, you can add a new entry to the table **YTADDICT_SYDEF** (SM30) with the following values:

- **SYSID** : ID of your system, found in SY-SYSID.
- **RULE_CLASS** : Name of your Z-Class implementing the interface **YIF_ADDICT_SYSTEM_RULES** . You can check **YCL_ADDICT_DEF_SYSTEM_RULES** for the default implementation and its documentation. You can change many behaviors of **ADDICT** over such a Z-Class.
- **MAX_WAIT** : For how many seconds should **ADDICT** wait until a time-out is assumed? (default: 30)
- **AUTO_REQUEST_PREFIX** : The default text prefix of an automatically created request. (default: Auto)

You don't have to fill all of those values. If you leave a field empty, **ADDICT** will assume its default value.

## Object index

Here is a list of significant classes within the package:

- **YCL_ADDICT_BDC**: Batch Input helper
- **YCL_ADDICT_CLASS**: Represents an ABAP class (SE24)
- **YCL_ADDICT_DATA_ELEMENT**: Represents a data element (SE11)
- **YCL_ADDICT_DOMAIN**: Represents a domain (SE11)
- **YCL_ADDICT_PACKAGE**: Represents an ABAP package (SE80)
- **YCL_ADDICT_TABLE**: Represents a database table, structure or view (SE11)
- **YCL_ADDICT_TABLE_FIELD**: Represents a field of a table (SE11)
- **YCL_ADDICT_TEXT_TOOLKIT**: Provides some useful text utilities
- **YCL_ADDICT_TOOLKIT**: Provides some useful data dictionary utilities
- **YCL_ADDICT_TRANSPORT_REQUEST**: Represents a transport request (SE24)

Most of those objects provide [multition design pattern](https://www.sap-press.com/design-patterns-in-abap-objects_4277/) functionality.
