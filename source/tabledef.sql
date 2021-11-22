--*================================================================================
--* Copyright (C) Wright Service Corp. - 2021  All Rights Reserved
--*
--* Object....: tabledef.sql
--* Purpose...: Create table TableDef for FILELIB
--*
--* Author....: Virginia Klein
--* Date......: 11/04/2021 
--*
--*--------------------------------------------------------------------------------
--* Modifications:
--* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--* Proj#    Date      Init  Description
--* -----   ---------- ----  -----------
--*
--*
--*
--*--------------------------------------------------------------------------------
CREATE OR Replace TABLE FILELIB/TABLE_FIELD_DEFINITION FOR SYSTEM NAME TABLEDEF ( 
	TEXT VARCHAR(32) CCSID 37 NOT NULL , 
	FIELD_NAME for column FLD VARCHAR(10) CCSID 37 NOT NULL , 
	DATA_TYPE VARCHAR(7) CCSID 37 NOT NULL , 
	DATA_SIZE VARCHAR(4) CCSID 37 NOT NULL , 
	NUMERIC_SCALE FOR COLUMN NUMSCALE VARCHAR(2) CCSID 37 NOT NULL , 
	DEFAULT_VAL FOR COLUMN DEFVAL VARCHAR(1) CCSID 37 NOT NULL , 
	USER_ID VARCHAR(9) CCSID 37 NOT NULL )   
	  
	RCDFMT TABLEDEF ; 
  
  LABEL ON TABLE FILELIB/TABLE_FIELD_DEFINITION
	IS 'Puerto Rico payroll export' ; 
	
  LABEL ON COLUMN FILELIB/TABLE_FIELD_DEFINITION (
      TEXT IS 'Text for Name and Description',
      FIELD_NAME IS 'Field Name',
      DATA_TYPE IS 'Data Type',
      DATA_SIZE IS 'Data Size (length)',
      NUMERIC_SCALE IS 'Numeric Scale',
      DEFAULT_VAL IS 'Default Value', 
      USER_ID IS 'User ID');

    LABEL ON COLUMN FILELIB/TABLE_FIELD_DEFINITION (
      TEXT TEXT IS 'Text for Name and Description',
      FIELD_NAME TEXT IS 'Field Name',
      DATA_TYPE  TEXT IS 'Data Type',
      DATA_SIZE  TEXT IS 'Data Size (length)',
      NUMERIC_SCALE  TEXT IS 'Numeric Scale',
      DEFAULT_VAL  TEXT IS 'Default Value', 
      USER_ID  TEXT IS 'User ID');

   CALL LOG_SQL_BUILD('TABLE_FIELD_DEFINITION', 'TABLE', 'FILELIB',  'SOURCE_DOC', 'SQL_LOC');