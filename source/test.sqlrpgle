**Free
      //*========================================================================
      //*
      //*
      //* Object....: Test
      //* Purpose...: For Class exercises
      //*
      //* Author....: Virginia Klein
      //* Date......: 8-02-2019
      //*
      //*------------------------------------------------------------------------
      //* Modifications:
      //* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      //* Proj#    Date    Init  Description
      //* ----- ---------- ----  -----------
      //*
      //*
      //*------------------------------------------------------------------------
       Ctl-Opt Option(*Srcstmt: *Nodebugio) ;
       /copy 'header/logpr'
       /copy 'header/sqlerrhndl'
       /copy 'header/pgmds'
       /copy 'header/ifsFiles'

       Dcl-PR Main EXTPGM( 'TEST' );

       End-PR ;

        //copy 'header/logpr'

        dcl-s repoAddr      char(128); // where to write this
        dcl-s tableName     char(50);  // long name
        dcl-s sysTableName  char(10);  // short name

        Dcl-Ds gStreamFile LikeDS(File_Temp);

         Dcl-Pi  main;
         End-Pi ;

        Dcl-s upperTableName char(50);
        Dcl-s lowerSysTableName char(10);

        sysTableName = 'test_this2';

        tableName = 'test_this_one_again';

        repoAddr = '/home/VIRGINIAK/repos/play/CBS';

        Exec SQL
           set :upperTableName = upper(:tableName);

        Exec SQL
           set :lowerSysTableName = lower(:sysTableName);


         gStreamFile.PathFile = '/home/VIRGINIAK/repos/play/CBS/source/' +
                    %trim(lowerSysTableName) + '.sql';
         gStreamFile.OpenMode = 'ab' + x'00';
         gStreamFile.FilePtr  = OpenFile(%addr(gStreamFile.PathFile)
                               :%addr(gStreamFile.OpenMode));


          makeHeader(upperTableName: lowerSysTableName);

        CloseFile(gStreamFile.FilePtr);

        *inlr = *on;

        // --------------------------------------------------
       // Procedure name: makeHeader
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC makeHeader;
         DCL-PI *N IND;
           tableName     char(50) const; // long name
           sysTableName  char(10) const; // short name
         END-PI ;

        Dcl-S pValue Char(250);
        Dcl-s USADate CHAR(10) ;
        Dcl-s isOK    ind inz(*on);

       Exec SQL
           set :USADate = VARCHAR_FORMAT(current date,'MM/DD/YYYY');


        // USADate =  %char(%date() :*USA);

          pvalue =  '--*================================================================================';
         writeLine(pvalue);

          pvalue =  '--* Copyright (C) Wright Service Corp. - '+ %char(%subdt(%date():*YEARS)) +'  All Rights Reserved';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--* Object....: ' + %trim(sysTableName) + '.sql';
         writeLine(pvalue);

          pvalue =  '--* Purpose...: Create table ' + %trim(tableName) + ' (' +  %trim(sysTableName) + ')';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--* Author....: ' + 'Virginia Klein';
         writeLine(pvalue);

          pvalue =  '--* Date......: ' +  USADate;
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--*--------------------------------------------------------------------------------';
         writeLine(pvalue);

          pvalue =  '--* Modifications:';
         writeLine(pvalue);

          pvalue =  '--* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -';
         writeLine(pvalue);

          pvalue =  '--* Proj#    Date      Init  Description';
         writeLine(pvalue);

          pvalue =  '--* -----   ---------- ----  -----------';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--*--------------------------------------------------------------------------------';

          writeLine(pValue);

        return isOK;
       END-PROC ;

       // -----------------------------------------------------
       // Procedure name: writeLine
       // Parameter: pValue -- line to write
       // Purpose:
       // Returns:
       // --------------------------------------------------
       Dcl-Proc writeLine ;
         DCL-PI *N ;
           inValue Char(250) const; // short name
         END-PI ;



         Dcl-S pValue Char(250);

            pValue = inValue;

          WriteFile(%Addr(pValue)
                   :%Len(%TrimR(pValue))
                   :1
                   :gStreamFile.FilePtr);

        return ;
       END-PROC ;


