**Free
     //*================================================================================
     //*
     //*
     //* Object....: gentable
     //* Purpose...: Create a table from source in qtemp
     //*
     //*
     //* Author....: Virginia Klein
     //* Date......: 11-04-2021
     //*
     //*--------------------------------------------------------------------------------
     //* Modifications:
     //* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     //* Proj#    Date      Init  Description
     //* -----   ---------- ----  -----------
     //*
     //*
     //*--------------------------------------------------------------------------------

       Dcl-c MAX_ARRAY       250;

       // --------------------------------------------------
       // Prototype for procedure: buildTable
       // --------------------------------------------------
       Dcl-Pr buildTable ExtProc('buildTable');
           repoAddr          char(128) const; // where to write this
           tableName         like(longName) const;
           sysTableName      like(shortName) const;
           tableText         like(longName) const;  // table description
           type              char(1) const;  // R - regular  T - temporal
       End-Pr ;

        // --------------------------------------------------
        //  Prototype for procedure: getColumnDefinitions
        // --------------------------------------------------
        Dcl-Pr getColumnDefinitions ExtProc('getColumnDefinitions');
            sysTableName   like(shortName) const; // short name
            fldDefs        likeds(fld_ds) Dim(MAX_ARRAY);
        End-Pr ;

        // --------------------------------------------------
        //  Prototype for procedure: setColumnInfoCursor
        // --------------------------------------------------
        Dcl-Pr setColumnInfoCursor ExtProc('setColumnInfoCursor');
            sysTableName  like(shortName) const;
            action        ind const;
            cursorOpen    ind;
        End-Pr ;

        // --------------------------------------------------
        //  Prototype for procedure: fetchNextColumn
        // --------------------------------------------------
        Dcl-Pr fetchNextColumn ind ExtProc('fetchNextColumn');
            sysTableName  like(shortName) const;
            cursorOpen    ind;
            FldImpDs      likeds(fld_ds) ;
        End-Pr ;

       // --------------------------------------------------
       // Prototype for procedure: getFileHandle
       // --------------------------------------------------
       Dcl-Pr getFileHandle Ind ExtProc('getFileHandle');
           filePath      char(150) const; // where to write this
           sysTableName  char(10) const; // short name
       End-Pr ;

       // --------------------------------------------------
       // Prototype for procedure: MakeHeader
       // --------------------------------------------------
       Dcl-Pr makeHeader Ind ExtProc('makeHeader');
           upperTableName    char(50) const;
           upperSysTableName char(50) const;
           lowerSysTableName char(10) const;
           type              char(1) const;
       End-pr;

       // -----------------------------------------------------
       // Prototype for procedure: writeLine
       // Parameter: pValue -- line to write
       // --------------------------------------------------
       Dcl-Pr writeLine ExtProc('writeLine');
          pValue Char(250) const; // short name
       End-pr;

       // --------------------------------------------------
       // Prototype for procedure: AddIDColumn
       // --------------------------------------------------
       Dcl-Pr AddIDColumn EXTPROC('AddIDColumn');
         UpperSysTableName CHAR(10) CONST;
       END-PR ;


       // --------------------------------------------------
       // Prototype for procedure: AddTempTmsps
       // --------------------------------------------------
       Dcl-Pr AddTempTmsps EXTPROC('AddTempTmsps');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addUserID
       // --------------------------------------------------
       Dcl-Pr addUserID EXTPROC('addUserID');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addUpdatedBy
       // --------------------------------------------------
       Dcl-Pr addUpdatedBy EXTPROC('addUpdatedBy');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addRegularHousekeeping
       // --------------------------------------------------
       Dcl-Pr addRegularHousekeeping EXTPROC('addRegularHousekeeping');
       END-PR ;

     // --------------------------------------------------
       // Prototype for procedure: buildFieldDefinitions
       // --------------------------------------------------
       Dcl-Pr buildFieldDefinitions EXTPROC('buildFieldDefinitions');
         sysTableName    char(10) const;
         longNameLength  packed(3:0) const;
         shortNameLength packed(3:0) const;
         typeDefLength  packed(3:0) const;
         fldDefs likeds(fld_ds) Dim(MAX_ARRAY);
       END-PR ;


       // --------------------------------------------------
       // Prototype for procedure: loadFieldsFromCSV
       // --------------------------------------------------
       Dcl-Pr loadFieldsFromCSV  ind EXTPROC('loadFieldsFromCSV');
         sysTableName CHAR(10) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: runLabelCursor
       // --------------------------------------------------
       Dcl-Pr runLabelCursor EXTPROC('runLabelCursor');
         fldDefs        likeds(fld_ds) Dim(MAX_ARRAY) const;
         sysTableName   char(10) const;
         textFlag       char(1) const;
         longNameLength packed(3:0) const;
       END-PR ;

        // --------------------------------------------------
        // Prototype for procedure:  getNextLabel
        // --------------------------------------------------
        Dcl-Pr getNextLabel IND EXTPROC('getNextLabel');
            fldDefs      likeds(fld_ds) Dim(MAX_ARRAY) const;
            idx          packed(3:0);
            field_def    char(50);
            field_lbl    char(50);
            field_text   char(50);
            column_text  char(50);
        END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: writeLabels
       // --------------------------------------------------
       Dcl-Pr writeLabels EXTPROC('writeLabels');
           fldDefs           likeds(fld_ds) Dim(MAX_ARRAY) const;
           upperTableName    Char(50) const;
           //sysTableName      CHAR(10) CONST;
           upperSysTableName CHAR(10) CONST;
           longNameLength    packed(3:0) const;
           tableText         like(longName) const;  // table description
           type              char(1) const;  // R - regular  T - temporal
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: AddTempHistory
       // --------------------------------------------------
       Dcl-Pr AddTempHistory EXTPROC('AddTempHistory');
         upperTableName CHAR(50) CONST;
         upperSysTableName CHAR(10) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addLog
       // --------------------------------------------------
       Dcl-Pr addLog EXTPROC('addLog');
         upperTableName CHAR(50) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: fetchMaxFieldLengths
       // --------------------------------------------------
       Dcl-Pr fetchMaxFieldLengths EXTPROC('fetchMaxFieldLengths');
         lowerSysTableName char(10) const;
         longNameLength    packed(3:0);
         shortNameLength   packed(3:0);
         typeDefLength    packed(3:0);
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addPrimaryKey
       // --------------------------------------------------
       Dcl-Pr addPrimaryKey EXTPROC('addPrimaryKey');
         sysTableName LIKE(shortName) const;
       END-PR ;

        // --------------------------------------------------
        // Prototype for procedure:setKeyCursor
        // --------------------------------------------------
        Dcl-Pr setKeyCursor  extproc('setKeyCursor ');
            sysTableName  like(shortName) const;
            action     ind const;
            cursorOpen ind;
        End-Pr ;

        // --------------------------------------------------
        // Prototype for procedure:fetchNextKey
        // --------------------------------------------------
        Dcl-Pr fetchNextKey ind extproc('fetchNextKey ');
            sysTableName  like(shortName) const;
            cursorOpen    ind;
            key           char(50);
        End-Pr ;

       // --------------------------------------------------
       // Prototype for procedure: writeForeignKeys
       // --------------------------------------------------
       Dcl-Pr writeForeignKeys EXTPROC('writeForeignKeys');
           tableName like(longName);
           fldDefs   likeds(fld_ds) Dim(MAX_ARRAY) const;
       END-PR ;


        // --------------------------------------------------
        // Prototype for procedure:fetchNextForeignKey
        // --------------------------------------------------
        Dcl-Pr fetchNextForeignKey ind extproc('fetchNextForeignKey ');
            fldDefs      Likeds(fld_ds) Dim(MAX_ARRAY) const;
            idx          Packed(3:0);
            refTable     like(shortName);
            keyName      like(shortName);
            shortRefName like(shortName);
        End-Pr ;

       // --------------------------------------------------
       // Prototype for procedure: updatePStatus
       // --------------------------------------------------
       Dcl-Pr updatePStatus extproc('updatePStatus');
         sysTableName like(shortName) const;
         newStatus    char(10) const;
       End-Pr ;

       // --------------------------------------------------
       // Prototype for procedure: getIDName
       // --------------------------------------------------
       Dcl-Pr getIDName extproc('getIDName');
         tableName like(shortName) const;
         longID    like(longName);
         shortID   like(shortName);
       End-Pr ;

       // --------------------------------------------------
       // Prototype For Procedure: GetNextColumn
       // --------------------------------------------------
       Dcl-Pr GetNextColumn ind Extproc('GetNextColumn');
         flddefs Likeds(fld_ds) Dim(MAX_ARRAY) const;
         idx     Packed(3:0) const;
         fldDef  Like(fld_ds);
       End-Pr ;

       // --------------------------------------------------
       // Prototype For Procedure: getDefinitionForNextColumn
       // --------------------------------------------------
       Dcl-Pr getDefinitionForNextColumn ind Extproc('getDefinitionForNextColumn');
         flddefs     likeds(fld_ds) Dim(MAX_ARRAY) const;
         idx         Packed(3:0) const;
         longName    like(longName);
         shortName   like(shortName);
         dataTypeStr char(12);
         defaultStr  char(12);
       End-Pr ;

       // --------------------------------------------------
       // Prototype For Procedure: addUniqueKey
       // --------------------------------------------------
       Dcl-Pr addUniqueKey extproc('addUniqueKey');
          tableName    like(longName) const;
          shortFldName like(shortName) const;
       End-Pr;

