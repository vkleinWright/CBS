--*================================================================================ 
--* Copyright (C) Wright Service Corp. - 2022  All Rights Reserved 
--* 
--* Object....: tabledef.sql 
--* Purpose...: Create table TABLE_FIELD_DEFINITION (tabledef) 
--* 
--* Author....: Virginia Klein 
--* Date......: 02/15/2022 
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
CREATE or replace TABLE FILELIB/TABLE_FIELD_DEFINITION FOR SYSTEM NAME TABLEDEF ( 
	TABLE_NAME        FOR COLUMN TABNAME    CHAR(10)    CCSID 37 NOT NULL DEFAULT '' , 
	LONG_FIELD_NAME   FOR COLUMN LONGFLD    VARCHAR(50) CCSID 37 NOT NULL DEFAULT '' , 
	FIELD_NAME        FOR COLUMN FLD        VARCHAR(10) CCSID 37 NOT NULL DEFAULT '' , 
	DATA_TYPE                               VARCHAR(9)  CCSID 37 NOT NULL DEFAULT '' , 
	DATA_SIZE                               VARCHAR(4)  CCSID 37 NOT NULL DEFAULT '' , 
	NUMERIC_SCALE     FOR COLUMN NUMSCALE   VARCHAR(2)  CCSID 37 NOT NULL DEFAULT '' , 
	DEFAULT_VAL       FOR COLUMN DEFVAL     VARCHAR(1)  CCSID 37 NOT NULL DEFAULT '' , 
	COLUMN_TEXT       FOR COLUMN CTEXT      CHAR(50)    CCSID 37 NOT NULL DEFAULT '' , 
	KEY_ORDER         FOR COLUMN KEYS       CHAR(2)     CCSID 37 NOT NULL DEFAULT '' , 
	FOREIGN_KEY_FLAG  FOR COLUMN FRNKEY     CHAR(1)     CCSID 37 NOT NULL DEFAULT '' , 
	FOREIGN_KEY_TABLE FOR COLUMN FKEYTBL    CHAR(10)    CCSID 37 NOT NULL DEFAULT '' , 
	USER_ID                                 VARCHAR(18) ALLOCATE(18) CCSID 37 NOT NULL DEFAULT USER , 
	PROCESSING_STATUS FOR COLUMN PSTATUS    CHAR(10) CCSID 37 NOT NULL DEFAULT 'NEW' )   
	  
	RCDFMT TABLEDEF   ; 
  
LABEL ON TABLE FILELIB/TABLE_FIELD_DEFINITION 
	IS 'Table definition work file' ; 
  
LABEL ON COLUMN FILELIB/TABLE_FIELD_DEFINITION 
( TABLE_NAME          IS 'Table                   Name' , 
	LONG_FIELD_NAME   IS 'Long                    Field                   Name' , 
	FIELD_NAME        IS 'Field                   Name' , 
	DATA_TYPE         IS 'Data                    Type' , 
	DATA_SIZE         IS 'Data                    Size                    (length)' , 
	NUMERIC_SCALE     IS 'Numeric                 Scale' , 
	DEFAULT_VAL       IS 'Default                 Value' , 
	KEY_ORDER         IS 'Key                     Order' , 
	FOREIGN_KEY_FLAG  IS 'Foreign                 Key                     Flag' , 
	FOREIGN_KEY_TABLE IS 'Foreign             	Key                 	Table' , 
	USER_ID           IS 'User ID' ) ; 
  
LABEL ON COLUMN FILELIB/TABLE_FIELD_DEFINITION 
( TABLE_NAME          TEXT IS 'Table Name' , 
	LONG_FIELD_NAME   TEXT IS 'Long Field Name' , 
	FIELD_NAME        TEXT IS 'Field Name' , 
	DATA_TYPE         TEXT IS 'Data Type' , 
	DATA_SIZE         TEXT IS 'Data Size (length)' , 
	NUMERIC_SCALE     TEXT IS 'Numeric Scale' , 
	DEFAULT_VAL       TEXT IS 'Default Value' , 
	KEY_ORDER         TEXT IS 'Key Order' , 
	FOREIGN_KEY_FLAG  TEXT IS 'Foreign Key Flag' , 
	FOREIGN_KEY_TABLE TEXT IS 'Foreign key table' , 
	USER_ID           TEXT IS 'User ID' ) ; 
  
GRANT DELETE , INSERT , SELECT , UPDATE   
ON FILELIB/TABLE_FIELD_DEFINITION TO PUBLIC ; 
  
GRANT ALTER , DELETE , INDEX , INSERT , REFERENCES , SELECT , UPDATE   
ON FILELIB/TABLE_FIELD_DEFINITION TO VIRGINIA WITH GRANT OPTION ;

CALL LOG_SQL_BUILD('TABLE_FIELD_DEFINITION', 'TABLE', 'FILELIB',  'SOURCE_DOC', 'SQL_LOC');  
  
