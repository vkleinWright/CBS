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
           repoAddr      char(128) const; // where to write this
           tableName     char(50) const; // long name
           sysTableName  char(10) const; // short name
           type          char(1) const;  // R - regular  T - temporal
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
           tableName     char(50) const; // long name
           sysTableName  char(10) const; // short name
       End-pr;

       // -----------------------------------------------------
       // Prototype for procedure: writeLine
       // Parameter: pValue -- line to write
       // --------------------------------------------------
       Dcl-Pr writeLine ExtProc('writeLine');
          pValue Char(250) const; // short name
       End-pr;
