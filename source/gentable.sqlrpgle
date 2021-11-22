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
    Ctl-Opt Option(*Srcstmt: *Nodebugio);

     /copy 'header/logpr'
     /copy 'header/sqlerrhndl'
     /copy 'header/pgmds'
     /copy 'header/ifsFiles'


     /copy 'header/gentablepr'



       Dcl-Pr unlink int(10) ExtProc('unlink');
          path pointer Value options(*string);
       End-Pr;
     // ----------------------------------------------------------------
     // Prototype for procedure: main
     // ----------------------------------------------------------------

     Dcl-Pr Main EXTPGM('gentable');
        repoAddr      char(128) const; // where to write this
        tableName     char(50) const;  // long name
        sysTableName  char(10) const;  // short name
        type          char(1) const;   // R - regular  T - temporal
     End-Pr ;

     Dcl-S  labels     Char(115)  Dim(200);
     Dcl-S  labelsText Char(120)  Dim(200);

     Dcl-Ds gBuildFile LikeDS(File_Temp);
    //  Dcl-Ds gStreamFile  ;
    //      PathFile char(128);
    //      RtvData  char(256);
    //      OpenMode char(5);
    //      FilePtr  pointer inz;
    //   End-ds;

       // ----------------------- Main procedure interface
     Dcl-Pi  main;
        repoAddr      char(128) const; // where to write this
        tableName     char(50) const; // long name
        sysTableName  char(10) const; // short name
        type          char(1) const;  // R - regular  T - temporal
     End-Pi ;

     dcl-s isOK      ind inz(*on);



        // call buildTable
        buildTable(repoAddr:tableName:sysTableName:type);


       CloseFile(%addr(gBuildFile.PathFile)) ;
     *inlr = *on;





       // --------------------------------------------------
       // Procedure name: buildTable
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC buildTable EXPORT;
         DCL-PI *N;
           repoAddr      char(128) const; // where to write this
           tableName     char(50) const; // long name
           sysTableName  char(10) const; // short name
           type          char(1) const;
         END-PI ;


         Dcl-s filePath char(150);

         filePath =  %trim(repoAddr) + '/source/' + %trim(sysTableName) + '.sql' ;
         // open the file
         getFileHandle(filePAth : sysTableName);

         // make header
         makeHeader( tableName : sysTableName);
         // make table declarations
         // add fields (for R and T)
         // add lables
         // add temporal history
         // add log

          return ;
        END-PROC ;

      //  // --------------------------------------------------
      //  // Procedure name: runCursor
      //  // Purpose:
      //  // Returns:
      //  // --------------------------------------------------
      //  DCL-PROC runCursor EXPORT;
      //    DCL-PI *N;
      //      tableName     char(50) const; // long name
      //      sysTableName  char(10) const; // short name
      //      type          char(1) const;
      //    END-PI ;

      //    dcl-s cursorOpen    ind;
      //    dcl-s eCMSeeno      like(hrtds.emeeno);
      //    dcl-s eCMScono      like(hrtds.emcono);

      //     Exec SQL
      //       declare c_fieldInfo cursor for
//                select replace(text, ' ', '_') , case when fld = '' then ''  else 'FOR column '|| fld end ,
// case
//      when upper(data_type) = 'DATE' then 'DATE'
//      when upper(data_type) = 'TIMESTMP' then 'TIMESTAMP'
//      when upper(data_type) = 'SMALLINT' then 'SMALLINT'
//      when upper(data_type) = 'BIGINT' then 'BIGINT'
//      when upper(data_type) = 'INTEGER' then 'INTEGER'
//      when upper(data_type) = 'DECIMAL' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
//      when upper(data_type) = 'NUMERIC' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
//      when upper(data_type) = 'FLOAT' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
//      else upper(trim(data_type))||'('||trim(char(data_size))|| ')' end , 'DEFAULT '|| case when data_type not in ('DATE', 'TIMESTAMP', 'SMALLINT', 'BIGINT', 'INTEGER', 'DECIMAL', 'NUMERIC', 'FLOAT')
//      and length(trim(default_val)) <> 0 then '''' || trim(default_val) ||'''' else '' end
// from   virginiak.table_fields_first ;



        //   DoW  fetchNextEmployeeToCheck(cursorOpen : eeno : eCMScono : eCMSssn);

        //      // The SSN should match the one for this employee in eCMS
        //      If eCMSssn <> WDssn;
        //        hasDiffSSN = *on;
        //        logWDError (file : runno : rowid : %char(eeno) :
        //                 'SSN does not match with this employee ' +
        //                 ' in eCMS in company ' + %char(eCMScono) + '. ');
        //     EndIf;

        //   EndDo;


        //  return ;
        // END-PROC ;


      //  // --------------------------------------------------
      //  // Procedure name: setFieldInfoCursor
      //  // Purpose:
      //  // Returns:
      //  // Parameter:      action
      //  // Parameter:      currentState
      //  // --------------------------------------------------
      //  DCL-PROC setFieldInfoCursor EXPORT;
      //    DCL-PI *N;
      //      eeno       like(hrtds.emeeno) const;
      //      action     ind const;
      //      cursorOpen ind;
      //    END-PI ;


      //        Select;
      //          when action = OPEN and not cursorOpen;

      //            Exec SQL
      //               open c_fieldInfo;

      //            If xSQLState2 <> Success_On_SQL;
      //               logMsgAndSQLError( program : %proc() : xSQLState :
      //                'Unable to open cursor for SSN' + '::');
      //            Else;
      //              cursorOpen = *on;
      //            EndIf;

      //          when action = CLOSE and cursorOpen;
      //               cursorOpen = *off;
      //            Exec SQL
      //               close  c_fieldInfo;
      //        EndSl;

      //    return ;
      //  END-PROC ;




      //  // --------------------------------------------------
      //  // Procedure name: fetchNextEmployeeToCheck
      //  // Purpose:
      //  // Returns:
      //  // --------------------------------------------------
      //  DCL-PROC fetchNextEmployeeToCheck EXPORT;
      //    DCL-PI *N IND;
      //      cursorOpen ind;
      //      eeno       like(hrtds.emeeno) const;
      //      eCMScono   like(hrtds.emcono);
      //      eCMSssn    like(hrtds.emssno);
      //    END-PI ;


      //    dcl-s fileEnd ind inz(*off);

      //    setFieldInfoCursor( eeno : OPEN : cursorOpen);

      //    If cursorOpen;

      //      Exec SQL
      //        fetch next from c_fieldInfo
      //        into :eCMScono, :eCMSssn;


      //      fileEnd = (sqlstt = NO_MORE_ROWS or sqlstt <> '00000');

      //     //  checkAndLogErrorMsg(%proc() : xSQLState :
      //     //       'Failed when checking for matching ssn ' +
      //     //          'for employee ' + %char(eeno) + ':' : *on);
      //     logMsgAndSQLError( program : %proc() : xSQLState :
      //         'failed when fetching field info.')

      //      If fileEnd;
      //         setFieldInfoCursor( eeno : CLOSE : cursorOpen );
      //      EndIf;

      //    EndIf;

      //  return not fileEnd;
      //  END-PROC ;




       // --------------------------------------------------
       // Procedure name: makeHeader
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC makeHeader EXPORT;
         DCL-PI *N IND;
           tableName     char(50) const; // long name
           sysTableName  char(10) const; // short name
         END-PI ;

        Dcl-S pValue Char(250);
        Dcl-s USADate CHAR(10) ;
        Dcl-s isOK    ind inz(*on);

        Dcl-s upperTableName char(50);
        Dcl-s lowerSysTableName char(10);

        Exec SQL
           set :upperTableName = upper(:tableName);

        Exec SQL
           set :lowerSysTableName = lower(:sysTableName);

        Exec SQL
           set :USADate = VARCHAR_FORMAT(current date,'MM/DD/YYYY');

        // USADate =  %char(%date() :*USA);

          pvalue =  '--*================================================================================';
         writeLine(pvalue);

          pvalue =  '--* Copyright (C) Wright Service Corp. - '+ %char(%subdt(%date():*YEARS)) +'  All Rights Reserved';
         writeLine(pvalue);

          pvalue =  '--*';
         writeLine(pvalue);

          pvalue =  '--* Object....: ' + %trim(lowerSysTableName) + '.sql';
         writeLine(pvalue);

          pvalue =  '--* Purpose...: Create table ' + %trim(upperTableName) + ' (' +  %trim(lowerSysTableName) + ')';
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
       // Procedure name: getFileHandle
       //  repoAddr      char(128) const; // where to write this
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC getFileHandle EXPORT;
         DCL-PI *N IND;
           filePath      char(150) const; // where to write this
           sysTableName  char(10) const; // short name
         END-PI ;

         Dcl-s isOK ind inz(*on);


          callp unlink(%trim(filePath));

           gBuildFile.PathFile = %trim(filePath) + x'00' ;
           gBuildFile.OpenMode = 'ab, ccsid=819' + x'00';
           gBuildFile.FilePtr  = OpenFile(%addr(gBuildFile.PathFile)
                                         :%addr(gBuildFile.OpenMode));
         If gBuildFile.FilePtr = *null ;
           logMsgAndSQLError( program : %proc() : xSQLState :
              'failed when getting file pointer for '+ gBuildFile.PathFile + '.');

//           writeErrorLog( ps_proc_name : %proc() : 'Getting pointer: ' +
//                 %Char( getErrno()  ) + ': ' +   Strerror  );
                 errnoException( 'Getting pointer: ':  getErrno()  ) ;
              isOK = *off;
         EndIf;
         return isOK;
       END-PROC ;

       // -----------------------------------------------------
       // Procedure name: writeLine
       // Parameter: pValue -- line to write
       // Purpose:
       // Returns:
       // --------------------------------------------------
       Dcl-Proc writeLine EXPORT;
         DCL-PI *N ;
           pValue Char(250) const; // short name
         END-PI ;

         Dcl-s writeValue Char(252);
         writeValue = %trim(pValue) + x'25';

         WriteFile(%Addr(writeValue)
           :%Len(%TrimR(writeValue))
           :1
           :gBuildFile.FilePtr);

//           writeErrorLog( ps_proc_name : %proc() : 'Writing line: ' +
//                 %Char( getErrno()  ) + ': ' +   Strerror   );
            errnoException( 'writing line: ':  getErrno()  ) ;
       return;
       END-PROC ;

      //------------------------------------------------------------
//        DCL-PROC writeLine2;
//    DCL-PI *n;
//       descriptor LIKE(descriptor_t) VALUE;
//       pBuf pointer VALUE;
//       bufLen INT(10) VALUE;
//       stackOffsetToRpg INT(10) VALUE;
//    END-PI;
//    DCL-S lineFeed CHAR(1) INZ(STREAM_LINE_FEED);
//    DCL-S bytesWritten INT(10);
//
//    bytesWritten = write (descriptor : pbuf : bufLen);
//    IF bytesWritten < 0;
//       errnoException ('Could not write data.'
//                     : getErrno ()
//                     : stackOffsetToRpg + 1);
//       // Control will not return here
//    ELSE;
//       bytesWritten = write (descriptor : %ADDR(lineFeed) : 1);
//       IF bytesWritten < 0;
//          errnoException ('Could not write line-feed.'
//                        : getErrno ()
//                        : stackOffsetToRpg + 1);
//          // Control will not return here
//       ENDIF;
//    ENDIF;
// END-PROC writeLine;

      //-- Get runtime error number: -----------------------------------
//       Dcl-Proc Errno;
//         DCL-PI *N int(10);
//         End-Pi;
//
//         dcl-pr sys_errno ExtProc('__errno') end-pr;
//         dcl-s  Error int(10) based(pError) NoOpt;
//
//         pError = sys_errno;
//
//         Return  Error;
//       End-Proc;

        DCL-PROC getErrno;
          DCL-PI *n INT(10) END-PI;
          DCL-PR getErrnoPtr pointer extproc('__errno') END-PR;
          DCL-S pErrno pointer static INZ(*null);
          DCL-S errno INT(10) BASED(pErrno);

          IF pErrno = *null;
             pErrno = getErrnoPtr();
          ENDIF;

          return errno;
        END-PROC getErrno;

 //----------------------------------------------------
      DCL-PROC errnoException;
        DCL-PI *n;
           msg CHAR(500) CONST;
           errnoVal INT(10) VALUE;
        END-PI;
        DCL-S errnoMsg VARCHAR(200);
        DCL-S pErrnoMsg pointer;
        DCL-PR strerror pointer extproc(*dclcase);
           errnoVal INT(10) VALUE;
        END-PR;



        pErrnoMsg = strError (errnoVal);
        IF pErrnoMsg <> *null;
           errnoMsg = ' ' + %STR(pErrnoMsg);
        ENDIF;
        errnoMsg += ' (errno = ' + %CHAR(errnoVal) + ')';

        writeErrorLog( ps_proc_name : %proc() : %trim(msg) + ' : ' +
                  errnoMsg  );

     END-PROC errnoException;
     //-- Get runtime error text: --------------------------------------**
//P Strerror        B
//D                 Pi           128a   Varying
//D sys_strerror    Pr              *   ExtProc( 'strerror' )
//D                               10i 0 Value
//C                   Return    %Str( sys_strerror( Errno ))
//P Strerror        E

         Dcl-Proc strError;
         DCL-PI *N varchar(128);
           errnoVal INT(10) value;
                  End-Pi;

         dcl-pr sys_strerror varchar(128) ExtProc('strerror') ;
           value int(10);
         end-pr;


         Return      sys_strerror( errnoVal )  ;
       End-Proc;
//     ---------------------------------------------------
//P Errno           B
//D                 Pi            10i 0
//D sys_errno       Pr              *   ExtProc( '__errno' )
//D Error           s             10i 0 Based( pError ) NoOpt
//C                   Eval      pError = sys_errno
//C                   Return    Error
//P Errno           E
//
//**-- Get runtime error text: --------------------------------------**
//P Strerror        B
//D                 Pi           128a   Varying
//D sys_strerror    Pr              *   ExtProc( 'strerror' )
//D                               10i 0 Value
//C                   Return    %Str( sys_strerror( Errno ))
//P Strerror        E


