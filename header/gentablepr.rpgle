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


       // --------------------------------------------------
       // Prototype for procedure: buildTable
       // --------------------------------------------------
       Dcl-Pr buildTable ExtProc('buildTable');
           repoAddr          char(128) const; // where to write this
           upperTableName    char(50) const;
           upperSysTableName char(50) const;
           lowerSysTableName char(10) const;
           type              char(1) const;  // R - regular  T - temporal
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
           upperTableName char(50) const;
           upperSysTableName char(50) const;
           lowerSysTableName char(10) const;
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
       DCL-PR AddIDColumn EXTPROC('AddIDColumn');
         UpperSysTableName CHAR(10) CONST;
       END-PR ;


       // --------------------------------------------------
       // Prototype for procedure: AddTempTmsps
       // --------------------------------------------------
       DCL-PR AddTempTmsps EXTPROC('AddTempTmsps');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addUserID
       // --------------------------------------------------
       DCL-PR addUserID EXTPROC('addUserID');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addUpdatedBy
       // --------------------------------------------------
       DCL-PR addUpdatedBy EXTPROC('addUpdatedBy');
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addRegularHousekeeping
       // --------------------------------------------------
       DCL-PR addRegularHousekeeping EXTPROC('addRegularHousekeeping');
       END-PR ;

     // --------------------------------------------------
       // Prototype for procedure: runFieldCursor
       // --------------------------------------------------
       DCL-PR runFieldCursor EXTPROC('runFieldCursor');
         sysTableName    char(10) const;
         longNameLength  packed(3:0) const;
         shortNameLength packed(3:0) const;
         typeDefLength  packed(3:0) const;
       END-PR ;

        // --------------------------------------------------
        // Prototype for procedure:  fetchNextColumn
        // --------------------------------------------------
        DCL-PR fetchNextColumn IND EXTPROC('fetchNextColumn');
            cursorOpen  ind;
            field_def   char(50);
            column_def  char(22);
            type_def    char(12);
            default_def char(12);
        END-PR ;

        // --------------------------------------------------
        // Prototype for procedure: setFieldInfoCursor
        // --------------------------------------------------
        DCL-PR setFieldInfoCursor EXTPROC('setFieldInfoCursor');
            action     ind const;
            cursorOpen ind;
        END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: loadFieldsFromCSV
       // --------------------------------------------------
       DCL-PR loadFieldsFromCSV  ind EXTPROC('loadFieldsFromCSV');
         sysTableName CHAR(10) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: runLabelCursor
       // --------------------------------------------------
       DCL-PR runLabelCursor EXTPROC('runLabelCursor');
         sysTableName   char(10) const;
         textFlag       char(1) const;
         longNameLength packed(3:0) const;
       END-PR ;

        // --------------------------------------------------
        // Prototype for procedure:  fetchNextLabel
        // --------------------------------------------------
        DCL-PR fetchNextLabel IND EXTPROC('fetchNextLabel');
            cursorOpen    ind;
            field_def     char(50);
            field_lbl     char(50);
            field_text    char(50);
            column_text   char(50);
        END-PR ;

        // --------------------------------------------------
        // Prototype for procedure: setLabelCursor
        // --------------------------------------------------
        DCL-PR setLabelCursor EXTPROC('setLabelCursor');
            action         ind const;
            cursorOpen     ind;
        END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: writeLabels
       // --------------------------------------------------
       DCL-PR writeLabels EXTPROC('writeLabels');
           upperTableName    Char(50) const;
           sysTableName      CHAR(10) CONST;
           upperSysTableName CHAR(10) CONST;
           longNameLength    packed(3:0) const;
           type              char(1) const;  // R - regular  T - temporal
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: AddTempHistory
       // --------------------------------------------------
       DCL-PR AddTempHistory EXTPROC('AddTempHistory');
         upperTableName CHAR(50) CONST;
         upperSysTableName CHAR(10) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: addLog
       // --------------------------------------------------
       DCL-PR addLog EXTPROC('addLog');
         upperTableName CHAR(50) CONST;
       END-PR ;

       // --------------------------------------------------
       // Prototype for procedure: fetchMaxFieldLengths
       // --------------------------------------------------
       DCL-PR fetchMaxFieldLengths EXTPROC('fetchMaxFieldLengths');
         lowerSysTableName char(10) const;
         longNameLength    packed(3:0);
         shortNameLength   packed(3:0);
         typeDefLength    packed(3:0);
       END-PR ;
