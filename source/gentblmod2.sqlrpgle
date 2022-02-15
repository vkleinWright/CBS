**Free
     //*================================================================================
     //*
     //*
     //* Object....: gentblmod
     //* Purpose...: procedures to create a table from source in qtemp
     //*
     //*
     //* Author....: Virginia Klein
     //* Date......: 1-12-2022
     //*
     //*--------------------------------------------------------------------------------
     //* Modifications:
     //* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     //* Proj#    Date      Init  Description
     //* -----   ---------- ----  -----------
     //*
     //*
     //*--------------------------------------------------------------------------------
    Ctl-Opt Option(*Srcstmt: *Nodebugio) nomain;

     /copy 'header/logpr'
     /copy 'header/sqlerrhndl'
     /copy 'header/pgmds'
     /copy *libl/qrpglesrc,IFSIO_H


     /copy 'header/gentblmpr2'


     Dcl-c REGULAR  'R';
     Dcl-c TEMPORAL 'T';
     Dcl-c WORK     'W';
     Dcl-c OPEN_     *on;
     Dcl-c CLOSE_    *off;
     Dcl-c SPACES_   '    ';
     Dcl-c crlf      x'0D25';
     Dcl-c lf        x'25';
     Dcl-c COLUMN_SPACES   '           ';
     Dcl-c TWO_SPACES      '  ';
     Dcl-c COLUMN_TEXT     'FOR COLUMN ';
     Dcl-c JUST_SPACES      '                                                 ';
     Dcl-c NOT_NULL        'NOT NULL';
     Dcl-c COMPLETE        'COMPLETE';

     Dcl-s longName   char(50);
     Dcl-s shortName  char(10);
     Dcl-s fd         Int(10);

     Dcl-ds fld_ds extname('TABLEDEF') qualified End-Ds;

       // --------------------------------------------------
       // Procedure name: buildTable
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC buildTable EXPORT;
         Dcl-Pi *N;
           repoAddr          char(128) const; // where to write this
           tableName         like(longName) const;
           sysTableName      like(shortName) const;
           tableText         like(longName) const;  // table description
           type              char(1) const;  // R - regular  T - temporal
         End-Pi ;

         Dcl-s upperTableName    char(50);
         Dcl-s lowerSysTableName like(shortName);
         Dcl-s upperSysTableName like(longName);
         Dcl-s filePath char(150);
         Dcl-s longNameLength  packed(3:0);
         Dcl-s shortNameLength packed(3:0);
         Dcl-s typeDefLength packed(3:0);
         Dcl-ds fldDefs likeds(fld_ds) dim(MAX_ARRAY);

         Exec SQL
            set :upperTableName = upper(replace(trim(:tableName), ' ','_'));

         Exec SQL
            set :upperSysTableName = upper(:sysTableName);

         Exec SQL
            set :lowerSysTableName = lower(:sysTableName);



         fetchMaxFieldLengths(sysTableName : longNameLength : shortNameLength : typeDefLength);

         filePath =  %trim(repoAddr) + '/source/' + %trim(sysTableName) + '.sql' ;

         // get the column definitions
         getColumnDefinitions(sysTableName : fldDefs);


         // open the file
         @open_file(filePath );

         // make header
         // and table declarations
         makeHeader( upperTableName : upperSysTableName : lowerSysTableName : type);

         // add ID field for regular table
         If type = REGULAR;
            AddIDColumn(upperSysTableName);
         EndIf;

         // add fields
         buildFieldDefinitions(sysTableName : longNameLength : shortNameLength : typeDefLength : fldDefs) ;

         // addHousekeeping
         Select;
         When type = TEMPORAL;
            AddIDColumn(upperSysTableName);
            // add user
            addUserID();
            addUpdatedBy();
            AddTempTmsps();
         When type = REGULAR;
            addRegularHousekeeping();
         EndSl;


         addPrimaryKey(sysTableName);

         // add constraints - unique & foreign key
         If type = REGULAR;
           addUniqueKey(UpperTableName : upperSysTableName);
           writeForeignKeys(upperTableName : fldDefs);
         EndIf;


         // close Column List
         closeColumnList(upperSysTableName);


         // add lables
         writeLabels(fldDefs : upperTableName : upperSysTableName : longNameLength : tableText : type);

         // add temporal history
         If type = TEMPORAL;
           addTempHistory( upperTableName : upperSysTableName);
         EndIf;

         // add log
         addLog( upperTableName );

          @close_file() ;

          updatePStatus(sysTableName : COMPLETE);

          return ;
        END-PROC ;

        // --------------------------------------------------
        // Procedure name: getColumnDefinitions
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC getColumnDefinitions EXPORT;
          Dcl-Pi *N;
            sysTableName    like(shortName) const; // short name
            fldDefs likeds(fld_ds) Dim(MAX_ARRAY);
          End-Pi ;

          Dcl-s cursorOpen      ind;
          Dcl-s idx             packed(3 : 0) inz(1);
          Dcl-ds FldImpDs       likeds(fld_ds) ;


           Exec SQL
             declare c_fieldInfo cursor for
                select tabname, upper(longfld), upper(fld), upper(data_type),
                       data_size, numscale, defval, ctext, keys, frnkey,
                       upper(fkeytbl), user_id, pstatus
                    from   tabledef t
                      where lower(table_name) =  lower(:sysTableName)
                      and pstatus = 'NEW'
                      order by  rrn(t);



           DoW  fetchNextColumn( sysTableName : cursorOpen : FldImpDs);

            fldDefs(idx) = FldImpDs;
            idx += 1;

           EndDo;


          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: setColumnInfoCursor
        // Purpose:
        // Returns:
        // Parameter:      action
        // Parameter:      currentState
        // --------------------------------------------------
        DCL-PROC setColumnInfoCursor EXPORT;
          Dcl-Pi *N;
            sysTableName      like(shortName) const;
            action     ind const;
            cursorOpen ind;
          End-Pi ;

                Select;
                when action = OPEN_ and not cursorOpen;

                  Exec SQL
                     open c_fieldInfo;

                  If xSQLState2 <> Success_On_SQL;
                     logMsgAndSQLError( program : %proc() : xSQLState :
                      'Unable to open table cursor ');
                  Else;
                    cursorOpen = *on;
                  EndIf;

                when action = CLOSE_ and cursorOpen;
                     cursorOpen = *off;
                  Exec SQL
                     close  c_fieldInfo;
              EndSl;

          return ;
        END-PROC ;


        // --------------------------------------------------
        // Procedure name: fetchNextColumn
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC fetchNextColumn EXPORT;
          Dcl-Pi *N IND;
            sysTableName  like(shortName) const;
            cursorOpen    ind;
            FldImpDs      likeds(fld_ds) ;
          End-Pi ;

          Dcl-s fileEnd ind inz(*off);


          setColumnInfoCursor( sysTableName : OPEN_ : cursorOpen );

          If cursorOpen;

            Exec SQL
              fetch next from c_fieldInfo
              into :FldImpDs;


            fileEnd = (sqlstt = NO_MORE_ROWS or sqlstt <> '00000');

            If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when fetching column info.')  ;
            EndIf;

            If fileEnd;
               setColumnInfoCursor(  sysTableName : CLOSE_ : cursorOpen );
            EndIf;

          EndIf;

        return not fileEnd;
        END-PROC ;

        // --------------------------------------------------
        // Procedure name: buildFieldDefinitions
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC buildFieldDefinitions EXPORT;
          Dcl-Pi *N;
            sysTableName    like(shortName) const;
            longNameLength  packed(3:0) const;
            shortNameLength packed(3:0) const;
            typeDefLength   packed(3:0) const;
            fldDefs likeds(fld_ds) Dim(MAX_ARRAY);
          End-Pi ;

          Dcl-s lineToWrite     char(200);
          Dcl-s CCSIDstr        char(8);
          Dcl-s longfldName     like(longName);
          Dcl-s shortFldName    like(shortName);
          Dcl-s dataTypeStr     char(12);
          Dcl-s defaultStr      char(12);
          Dcl-s idx             packed(3:0) inz(1);


          DoW getDefinitionForNextColumn(FldDefs : idx : longfldName :
                                         shortfldName :  dataTypeStr : defaultStr);

            If %scan ('CHAR' : dataTypeStr) > 0;
              CCSIDstr = 'CCSID 37';
            Else;
              CCSIDstr = '        ';
            EndIf;

           select;
               when shortNameLength > 0 and shortFldName = *blanks;
                    lineToWrite = SPACES_ + %trim(longfldName) +
                                  // padding before short name
                                  %subst(JUST_SPACES : 1 : %int(longNameLength) - %len(%trim(longfldName)))  +
                                  TWO_SPACES + COLUMN_SPACES  +
                                  // padding for missing short name
                                  %subst(JUST_SPACES : 1 : %int(shortNameLength)) +
                                  //%trim(column_def)  +  '  ' +
                                  TWO_SPACES + %trim(dataTypeStr) +
                                  // padding after type def
                                  %subst(JUST_SPACES : 1 : %int(typeDefLength) - %len(%trim(dataTypeStr))) +
                                  // ccsid or spaces
                                  TWO_SPACES + CCSIDstr + TWO_SPACES +
                                  NOT_NULL + TWO_SPACES + %trim(defaultStr) + ',';

               when shortNameLength > 0 ;
                    lineToWrite = SPACES_ + %trim(longfldName) +
                                  // padding before short name
                                  %subst(JUST_SPACES : 1 : %int(longNameLength) - %len(%trim(longfldName)))  +
                                  // 'FOR COLUMN'
                                  TWO_SPACES + COLUMN_TEXT +
                                  // short name
                                  %trim(shortFldName)    +
                                  // padding after short name
                                  %subst(JUST_SPACES : 1 : %int(shortNameLength) - %len(%trim(shortFldName))) +
                                  //%trim(column_def)  +  '  ' +
                                  TWO_SPACES + %trim(dataTypeStr) +
                                  // padding after type def
                                  %subst(JUST_SPACES : 1 : %int(typeDefLength) - %len(%trim(dataTypeStr))) +
                                  // ccsid or spaces
                                  TWO_SPACES + CCSIDstr + TWO_SPACES +
                                  NOT_NULL + TWO_SPACES + %trim(defaultStr) + ',' ;

             EndSl;

             WriteLine(lineToWrite);
             idx += 1;

           EndDo;


          return ;
         END-PROC ;

       // --------------------------------------------------
       // Procedure name: makeHeader
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC makeHeader EXPORT;
         Dcl-Pi *N IND;
           upperTableName    like(longName) const; // long name
           upperSysTableName like(longName) const;  // short name
           lowerSysTableName like(shortName) const;  // short name
           type              char(1) const;
         End-Pi ;

        Dcl-s pValue char(250);
        Dcl-s USADate char(10) ;
        Dcl-s isOK    ind inz(*on);



        Exec SQL
           set :USADate = VARCHAR_FORMAT(current date,'MM/DD/YYYY');


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

          If  %trim(upperTableName) <> %trim(upperSysTableName)  ;

            pvalue =  ' Create or Replace table FILELIB/' + %trim(upperTableName) + ' for system name ' +  %trim(upperSysTableName) + '(';

          Else;
            pvalue =  ' Create or Replace table FILELIB/' + %trim(upperTableName) +  '(';
          EndIf;
          writeLine(pValue);

        return isOK;
       END-PROC ;


      // -----------------------------------------------------
      // Procedure name: @open_file
      // Parameter: Get file handle
      // Purpose:
      // Returns:
      // --------------------------------------------------
        DCL-PROC @open_file ;
          Dcl-Pi *N ;
            filePath      char(150) const; // where to write this
          End-Pi;

           callp unlink(%trim(filePath));


           fd = open(%trim(filePath)
                 : O_WRONLY+O_CREAT+O_TRUNC+O_CCSID
                 : S_IRGRP + S_IWGRP + S_IXGRP
                 : 819);

          callp close(fd);

          fd = open(%trim(filePath):O_WRONLY+O_TEXTDATA);

        return;
      End-proc;

      // -----------------------------------------------------
      // Procedure name: @close_file
      // Parameter: Close file handle
      // Purpose:
      // Returns:
      // --------------------------------------------------
       DCL-PROC @close_file ;
         Dcl-Pi *N;
         End-Pi;

         callp close(fd);
         return;
       End-Proc;


       // -----------------------------------------------------
       // Procedure name: writeLine
       // Parameter: pValue -- line to write
       // Purpose:
       // Returns:
       // --------------------------------------------------
        Dcl-Proc writeLine EXPORT;
         Dcl-Pi *N ;
           pValue char(250) const; // short name
         End-Pi ;

          Dcl-s writeValue char(252);
           writeValue = %trimr(pValue) + ' ' +  lf;

           callp write(fd: %addr(writeValue): %len(%trimr(writeValue)) );

        End-Proc;


       // --------------------------------------------------
       // Procedure name: AddIDColumn
       // Purpose:        Add ID column
       // Returns:
       // Parameter:      UpperSysTableName
       // --------------------------------------------------
       DCL-PROC AddIDColumn EXPORT;
         Dcl-Pi *N;
           UpperSysTableName like(shortName) const;
         End-Pi ;

         Dcl-s pValue  char(200);
         Dcl-s longID  like(longName);
         Dcl-s shortID like(shortName);

         getIDName(UpperSysTableName :  longID : shortID);

         If longID = shortID or shortID = *blanks;
           pValue =  SPACES_ +  %trim(longID) + ' BIGINT GENERATED ALWAYS AS IDENTITY (';
         Else;
           pValue =  SPACES_ +  %trim(longID) + ' FOR COLUMN ' +
                     %trim(shortID) + ' BIGINT GENERATED ALWAYS AS IDENTITY (';
         EndIf;

          writeLine(pvalue);
          pvalue =  SPACES_ +  '    START WITH 1 INCREMENT BY 1';
          writeLine(pvalue);
          pvalue =  SPACES_ +  '    NO MINVALUE NO MAXVALUE';
          writeLine(pvalue);
          pvalue =  SPACES_ + '    NO CYCLE NO ORDER';
          writeLine(pvalue);
          pvalue =  SPACES_ + '    CACHE 20 )  ,';
          writeLine(pvalue);

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: closeColumnList
       // Purpose:        Close column list
       // Returns:
       // Parameter:      upperSysTableName
       // --------------------------------------------------
       DCL-PROC closeColumnList ;
         Dcl-Pi *N;
           upperSysTableName like(shortName) const;
         End-Pi ;

         Dcl-s pValue char(200);

         pValue = ')';
         writeLine(pvalue);

         pValue = SPACES_ +  'RCDFMT ' + upperSysTableName + ';' ;
         writeLine(pvalue);
         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: AddTempTmsps
       // Purpose:        Add tumestamps for Temporal tables
       // Returns:
       // --------------------------------------------------
       DCL-PROC AddTempTmsps EXPORT;

          Dcl-s pValue char(200);

         pValue =  SPACES_ + 'START_TS   TIMESTAMP(12) NOT NULL GENERATED ALWAYS AS ROW BEGIN, ';
         writeLine(pvalue);
         pValue =  SPACES_ + 'END_TS     TIMESTAMP(12) NOT NULL GENERATED ALWAYS AS ROW END,';
         writeLine(pvalue);
         pValue =  SPACES_ + 'TS_ID      TIMESTAMP(12) GENERATED ALWAYS AS TRANSACTION START ID,';
         writeLine(pvalue);
         pValue =  SPACES_ + 'PERIOD SYSTEM_TIME (START_TS, END_TS)';
         writeLine(pvalue);

         return ;
       END-PROC ;



       // --------------------------------------------------
       // Procedure name: addUserID
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC addUserID EXPORT;
         Dcl-s pValue char(200);
         Dcl-s just_spaces char(50);
         Dcl-s spaces varchar(50);
         Dcl-s gap int(3) inz(15);

         spaces = %subst(just_spaces : 1 : gap);

         pValue =  SPACES_ + 'USERID' + SPACES_ + ' VARCHAR(18)  CCSID 37     NOT NULL  DEFAULT USER,';
         writeLine(pvalue);

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: addUpdatedBy
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC addUpdatedBy EXPORT;
         Dcl-s pValue char(200);

         pValue =  SPACES_ + 'UPDATED_BY ' +
                         'FOR COLUMN UPDATEDBY  VARCHAR(18)  CCSID 37     NOT NULL  DEFAULT USER,';
         writeLine(pvalue);

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: addRegularHousekeeping
       // Purpose:        Add regular housekeeping fields
       // Returns:
       // --------------------------------------------------
       DCL-PROC addRegularHousekeeping EXPORT;
         Dcl-s pValue char(200);
         Dcl-s gap int(3) inz(10);


         pValue =  SPACES_ + 'ADDED_BY   FOR COLUMN ADDEDBY    VARCHAR(18) ALLOCATE(18) CCSID 37 NOT NULL DEFAULT USER , ';
         writeLine(pvalue);

         pValue =  SPACES_ + 'ADDED_ON   FOR COLUMN ADDEDON    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,';
         writeLine(pvalue);

         addUpdatedBy();

         pValue =  SPACES_ + 'UPDATED_ON FOR COLUMN UPDATEDON  TIMESTAMP ' +
                   'GENERATED BY DEFAULT FOR EACH ROW ON UPDATE AS ROW CHANGE TIMESTAMP NOT NULL, -- comma?? <===<<<';
         writeLine(pvalue);

         return ;
       END-PROC ;



       // --------------------------------------------------
       // Procedure name: loadFieldsFromCSV
       // Purpose:        Load field definition from csv
       // Returns:
       // Parameter:      sysTableName
       // --------------------------------------------------
       DCL-PROC loadFieldsFromCSV EXPORT;
         Dcl-Pi *N ind;
           sysTableName like(shortName) const;
         End-Pi ;

         Dcl-s isOK ind inz(*on);
         Dcl-s mypath          varchar(158);

          mypath =  '/home/VIRGINIAK/TableDefs/'+ %trim(sysTableName) + '.csv';


         Exec SQL
           delete from  tabledef
              where lower(table_name) =  lower(:sysTableName);

         logMsgAndSQLError( program : %proc() : xSQLState :
                   'Here is what we get when deleting csv data for table ' + %trim(sysTableName) + '.');

         Exec SQL
          insert into tabledef
            (table_name, longfld, fld, data_type, data_size, numeric_scale,
                                                     default_val, column_text, keys, frnkey, fkeytbl)
            select
              lower(:sysTableName),
              --the second  -- long name
              cast( substr(line, 1, locate_in_string(line, ',') -1) as varchar(50)),
              --the third -- short name
              cast( substr(line,   locate_in_string(line, ',') +1, locate_in_string(line, ',',1,  2) - locate_in_string(line, ',') -1) as char(10)),
              --the fourth -- data type
              cast(  substr(line,   locate_in_string(line, ',', 1,2) +1, locate_in_string(line, ',',1,  3) - locate_in_string(line, ',', 1,2) -1)  as char(12)),
               --the fifth  -- size
              cast(  substr(line,   locate_in_string(line, ',', 1,3) +1, locate_in_string(line, ',',1,  4) - locate_in_string(line, ',', 1,3) -1) as char(12)),
               --the sixth -- decimals
              cast( substr(line,   locate_in_string(line, ',', 1,4) +1, locate_in_string(line, ',',1,  5) - locate_in_string(line, ',', 1,4) -1) as char(12)),
               --the seventh   -- default value
             cast( substr(line,   locate_in_string(line, ',', 1,5) +1, locate_in_string(line, ',',1,  6) - locate_in_string(line, ',', 1,5) -1) as char(1)),
              --the eight   -- text
              cast( substr(line,   locate_in_string(line, ',', 1,6) +1, locate_in_string(line, ',',1, 7) - locate_in_string(line, ',', 1,6) -1) as char(50)),
              -- the ninth -- key order
              cast( substr(line,   locate_in_string(line, ',', 1,7) +1, locate_in_string(line, ',',1, 8) - locate_in_string(line, ',', 1,7) -1) as char(1)),
              -- the tenth -- is foreign key
              cast( substr(line,   locate_in_string(line, ',', 1,8) +1 , locate_in_string(line, ',',1, 9) - locate_in_string(line, ',', 1,8) -1) as char(1)),
              -- the eleventh -- is  table the foreign key connects to
              cast(substr(line,   locate_in_string(line, ',', 1,9) +1 ) as char(10)) FROM
              TABLE(QSYS2/IFS_READ(
               PATH_NAME => :mypath))x
                 where substr(trim(line),1,1) in ('A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e',
                                                  'F', 'f', 'G', 'g', 'H', 'h', 'I', 'i', 'J', 'j',
                                                  'K', 'k', 'L', 'l', 'M', 'm', 'N', 'n', 'O', 'o',
                                                  'P', 'p', 'Q', 'q', 'R', 'r', 'S', 's', 'T', 't',
                                                  'U', 'u', 'V', 'v', 'W', 'w', 'X', 'x', 'Y', 'y',
                                                  'Z', 'z');


                logMsgAndSQLError( program : %proc() : xSQLState :
                   'Here is what we get when writing csv data for table ' + %trim(sysTableName) + '.');

                If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when writing csv data for table ' + %trim(sysTableName) + '.');

                isOK = *off;
            EndIf;

         return isOK;
       END-PROC ;


        // --------------------------------------------------
        // Procedure name: runLabelCursor
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC runLabelCursor EXPORT;
          Dcl-Pi *N;
            fldDefs        likeds(fld_ds) Dim(MAX_ARRAY) const;
            sysTableName   like(shortName) const; // short name
            textFlag       char(1) const;
            longNameLength packed(3:0) const;
          End-Pi ;

          Dcl-s field_def     like(longName);
          Dcl-s field_lbl     like(longName);
          Dcl-s field_text    like(longName);
          Dcl-s column_text   like(longName);
          Dcl-s idx             packed(3:0) inz(1);

           If textFlag <> 'Y';
             // for alignment
              writeLine ('--' + SPACES_ +  %subst(JUST_SPACES : 1 : %int(longNameLength) - 2) +
                                     ' IS ''' +
                                     'AAAAA               BBBBB               CCCCC               ''');
           EndIf;

           DoW  getNextLabel( fldDefs : idx : field_def : field_lbl : field_text : column_text);
            If textFlag = 'Y';

               If column_text = '';
                 column_text = field_text;
               EndIf;
               writeLine( SPACES_ + %trim(field_def) +
                                  // padding before short name
                                  %subst(JUST_SPACES : 1 : %int(longNameLength) - %len(%trim(field_def)))  +
                                  ' TEXT IS ''' +  %trim(column_text)  + ''',') ;
            Else ;

               writeLine( SPACES_ +  %trim(field_def) +
                                     // padding before short name
                                     %subst(JUST_SPACES : 1 : %int(longNameLength) - %len(%trim(field_def)))  +
                                     ' IS ''' +  %trim(field_lbl)  + ''',');
            EndIf;

            idx += 1;

           EndDo;


          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: getNextLabel
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC getNextLabel EXPORT;
          Dcl-Pi *N IND;
            fldDefs       likeds(fld_ds) Dim(MAX_ARRAY) const;
            idx           packed(3:0);
            field_def     like(longName);
            field_lbl     like(longName);
            field_text    like(longName);
            column_text   like(longName);
          End-Pi ;

          Dcl-s fileEnd ind inz(*off);
          Dcl-s shortFldID like(shortName);

         Select;

           when idx > MAX_ARRAY;
             fileEnd = *on;

           when fldDefs(idx).FRNKEY = 'Y';

              getIDName( fldDefs(idx).fkeytbl : field_def : shortFldID);
              field_lbl = field_def;
              field_text = field_def;
              column_text = 'ID - key to table ' + fldDefs(idx).fld;


           when fldDefs(idx).TABNAME <> *blanks;
              field_def = %ScanRpl(' ' : '_' : %trim(fldDefs(idx).longfld));
              field_lbl  = fldDefs(idx).longfld;
              field_text = fldDefs(idx).longfld;
              column_text = fldDefs(idx).ctext;

              Exec SQL
               set :field_lbl  = proper_case(:field_lbl) ;

              Exec SQL
               set :field_text = upper(substr(:field_text, 1, 1))||lower(substr(:field_text,2)) ;

              Exec SQL
               set :column_text = upper(substr(:column_text, 1, 1))||lower(substr(:column_text,2));

           other;
             fileEnd = *on;
         EndSl;


        return not fileEnd;
        END-PROC ;


       // --------------------------------------------------
       // Procedure name: writeLabels
       // Purpose:        Write labels
       // Returns:
       // Parameter:      sysTableName
       // --------------------------------------------------
       DCL-PROC writeLabels EXPORT;
         Dcl-Pi *N;
           fldDefs           likeds(fld_ds) Dim(MAX_ARRAY) const;
           upperTableName    like(longName) const;
         //  tableName         like(shortName) const;
           upperSysTableName like(shortName) const;
           longNameLength    packed(3:0) const;
           tableText         like(longName) const;  // table description
           type              char(1) const;  // R - regular  T - temporal
         End-Pi ;

         Dcl-s numberOfSpaces packed(3:0);
         Dcl-s paddedLength   packed(3:0);



         writeLine (' ');
         writeLine (' ');
         writeLine ('LABEL ON TABLE FILELIB/' + %trim(upperTableName));
         writeLine ('  IS ''' + %trim(tableText) + '''; ');


         writeLine (' ');
         writeLine (' ');
         writeLine ('LABEL ON COLUMN FILELIB/' + %trim(upperTableName));
         writeLine ('(');

         If type = REGULAR and longNameLength < 10;
            paddedLength = 10;
          Else;
            paddedLength = longNameLength;
          EndIf;

         // here's where we add the labels for the fields
         runLabelCursor(fldDefs : upperSysTableName : 'N' : paddedLength);

         If type <> WORK;
            If longNameLength > %len(%trim(UpperSysTableName)) + 2;
              numberOfSpaces = paddedLength - %len(%trim(UpperSysTableName))  -1;
            EndIf;

            writeLine( SPACES_ + %trim(UpperSysTableName) + 'ID' +
                                 %subst(JUST_SPACES : 1 : paddedLength) +
                                 'IS ''' +  %trim(UpperSysTableName) + ' ID'',');
         EndIf;

         If type = REGULAR;

            If paddedLength > 8;
              numberOfSpaces = paddedLength - 7;
            EndIf;

            writeLine ( SPACES_ + 'ADDED_BY' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'IS ''Added by'' , ');
            writeLine ( SPACES_ + 'ADDED_ON' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'IS ''Added on'' , ');

            If paddedLength > 10;
              numberOfSpaces = paddedLength - 9;
            EndIf;
            writeLine ( SPACES_ + 'UPDATED_BY' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'IS ''Updated by'' , ');
            writeLine ( SPACES_ + 'UPDATED_ON' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'IS ''Updated on'' ');
         EndIf;
         writeLine (');');

         writeLine (' ');

         writeLine ('LABEL ON COLUMN FILELIB/' + %trim(upperTableName));
         writeLine ('(');

         // here's where we add the labels for the fields
         runLabelCursor(fldDefs : upperSysTableName : 'Y' : paddedLength);

         If type <> WORK;
            If longNameLength > %len(%trim(UpperSysTableName)) + 2;
              numberOfSpaces = paddedLength - %len(%trim(UpperSysTableName)) -1 ;
            EndIf;
           writeLine( SPACES_ + %trim(UpperSysTableName) + 'ID' +
                                %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                'TEXT IS ''' +
                                %trim(UpperSysTableName) + ' ID'',');
         EndIf;
         If type = REGULAR;
            If longNameLength > 8;
              numberOfSpaces = paddedLength - 7;
            EndIf;
            writeLine ( SPACES_ + 'ADDED_BY' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'TEXT IS ''Added by'' , ');
            writeLine ( SPACES_ + 'ADDED_ON' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'TEXT IS ''Added on'' , ');
            If longNameLength > 10;
              numberOfSpaces = paddedLength - 9;
            EndIf;
            writeLine ( SPACES_ + 'UPDATED_BY' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'TEXT IS ''Updated by'' , ');
            writeLine ( SPACES_ + 'UPDATED_ON' +
                                   %subst(JUST_SPACES : 1 : numberOfSpaces) +
                                   'TEXT IS ''Updated on'' ');
         EndIf;
         writeLine (');');


         return ;
       END-PROC ;

       // --------------------------------------------------
       // Procedure name: AddTempHistory
       // Purpose:        Add lines for Temporal history table
       // Returns:
       // Parameter:      upperTableName
       // Parameter:      upperSysTableName
       // --------------------------------------------------
       DCL-PROC AddTempHistory EXPORT;
         Dcl-Pi *N;
           upperTableName    like(longName) const;
           upperSysTableName like(shortName) const;
         End-Pi ;

         writeLine ('');
         writeLine ('');
         writeLine ('CREATE OR REPLACE TABLE FILELIB/' + %trim(upperTableName) +
                        '_HIST FOR SYSTEM NAME ' + %trim(upperSysTableName) + 'H LIKE ' +
                        %trim(upperTableName) + ' ;                  -- <====<<<<<<');

         writeLine ('');
         writeLine ('LABEL ON TABLE FILELIB/' + %trim(upperTableName) +
                        '_HIST IS ''   hist'' ;                -- <====<<<<<< ');

          writeLine ('');
          writeLine ('ALTER TABLE FILELIB/' + %trim(upperTableName) +
                    ' ADD VERSIONING USE HISTORY TABLE ' + %trim(upperTableName) + '_HIST ;');

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: addLog
       // Purpose:        Add the log record
       // Returns:
       // Parameter:      upperTableName
       // --------------------------------------------------
       DCL-PROC addLog EXPORT;
         Dcl-Pi *N;
           upperTableName like(longName) const;
         End-Pi ;

         writeLine ('');
         writeLine ('');
         writeLine ('CALL LOG_SQL_BUILD(''' + %trim(upperTableName) +
                     ''', ''TABLE'', ''FILELIB'',  ''SOURCE_DOC'', ''SQL_LOC'');');

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: fetchMaxFieldLengths
       // Purpose:
       // Returns:
       // Parameter:      longNameLength
       // Parameter:      shortNameLength
       // --------------------------------------------------
       DCL-PROC fetchMaxFieldLengths EXPORT;
         DCL-PI *N;
           lowerSysTableName like(shortName) const;
           longNameLength packed(3:0);
           shortNameLength packed(3:0);
           typeDefLength packed(3:0);
         END-PI ;

         Exec SQL
          select max(length(trim(longfld)) + (case when frnkey = 'Y' then 2 else 0 end)),
                 -- we have to make allowances for adding 'ID' to foreign keys
                 max(min(length(trim(fld)) + (case when frnkey = 'Y' then 2 else 0 end), 10)),
                 max(length(trim(data_type))
                  + case when trim(data_size) <> '' then 2 else 0 end
                  + length(trim(data_size))
                  + case when numeric_scale = '' then 0
                         else length(trim(numeric_scale)) +1 end )
           into :longNameLength, :shortNameLength, :typeDefLength
           from tabledef
           where tabname = :lowerSysTableName and pstatus = 'NEW';

         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure name: addPrimaryKey
       // Purpose:
       // Returns:
       // Parameter:      sysTableName
       // --------------------------------------------------
       DCL-PROC addPrimaryKey EXPORT;
          Dcl-Pi *N;
            sysTableName  like(shortName) const; // short name
          End-Pi ;

          Dcl-s cursorOpen    ind;
          Dcl-s key           char(50);
          Dcl-s keyStr        varchar(2000);
          Dcl-s count         int(3) inz(1);


           Exec SQL
             declare c_keys cursor for
              select
                upper(cast(case when fld = '' then replace(trim(longfld), ' ', '_')  else   fld end  as char(50)))
              from tabledef
              where lower(table_name) =  lower(:sysTableName) and keys <> ''
              order by  keys;


           DoW  fetchNextKey(sysTableName : cursorOpen : key);
            If count = 1;
              keyStr = key;
            Else;
              keyStr = %trim(keyStr) + ', ' + key;
            EndIf;
            Count+=1;


           EndDo;

           If Count > 1;
            writeLine( ',   -- do we need a comma??? <======<<<<') ;
            writeLine(SPACES_ + 'PRIMARY KEY(' + %trim(keyStr) + ')') ;
           EndIf;

          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: setKeyCursor
        // Purpose:
        // Returns:
        // Parameter:      action
        // Parameter:      currentState
        // --------------------------------------------------
        DCL-PROC setKeyCursor EXPORT;
          Dcl-Pi *N;
            sysTableName  like(shortName) const;
            action     ind const;
            cursorOpen ind;
          End-Pi ;

              Select;
                when action = OPEN_ and not cursorOpen;

                  Exec SQL
                     open c_keys;

                  If xSQLState2 <> Success_On_SQL;
                     logMsgAndSQLError( program : %proc() : xSQLState :
                      'Unable to open key cursor ');
                  Else;
                    cursorOpen = *on;
                  EndIf;

                when action = CLOSE_ and cursorOpen;
                     cursorOpen = *off;
                  Exec SQL
                     close  c_keys;
              EndSl;

          return ;
        END-PROC ;

        // --------------------------------------------------
        // Procedure name: fetchNextKey
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC fetchNextKey EXPORT;
          Dcl-Pi *N IND;
            sysTableName  like(shortName) const;
            cursorOpen    ind;
            key           char(50);
          End-Pi ;

          Dcl-s fileEnd ind inz(*off);

          setKeyCursor( sysTableName : OPEN_ : cursorOpen);

          If cursorOpen;

            Exec SQL
              fetch next from c_keys
              into :key;


            fileEnd = (sqlstt = NO_MORE_ROWS or sqlstt <> '00000');

            If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when fetching primary key info.')  ;
            EndIf;

            If fileEnd;
               setKeyCursor( sysTableName : CLOSE_ : cursorOpen );
            EndIf;

          EndIf;

        return not fileEnd;
        END-PROC ;

       // --------------------------------------------------
       // Procedure name: writeForeignKeys
       // Purpose:        Write labels
       // Returns:
       // Parameter:      sysTableName
       // --------------------------------------------------
       DCL-PROC writeForeignKeys EXPORT;
         Dcl-Pi *N;
           tableName like(LongName);
           fldDefs Likeds(fld_ds) Dim(MAX_ARRAY) const;
         End-Pi ;

          Dcl-s keyName       like(longName);
          Dcl-s refTable      like(shortName);
          Dcl-s longRefName   like(longName);
          Dcl-s shortRefName  like(shortName);
          Dcl-s idx           packed(3:0) inz(1);

           DoW fetchNextForeignKey(FldDefs : idx : refTable : keyName : shortRefName);

//

              writeLine (SPACES_ + 'FOREIGN KEY (' + %trim(shortRefName) + ')'+
                          ' REFERENCES '+ %trim(refTable) +' (' + %trim(shortRefName) + ')' +
                          ' ON UPDATE NO ACTION   ON DELETE NO ACTION ,   -- comma??  <<=======<<<<<<');

             idx += 1;

           EndDo;

          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: fetchNextForeignKey
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC fetchNextForeignKey EXPORT;
          Dcl-Pi *N IND;
            fldDefs      Likeds(fld_ds) Dim(MAX_ARRAY) const;
            idx          Packed(3:0);
            refTable     like(shortName);
            keyName      like(shortName);
            shortRefName like(shortName);
           End-Pi ;

          Dcl-s fileEnd ind inz(*off);
          Dcl-s longRefName like(longName);


           DoW idx <=  MAX_ARRAY and
               fldDefs(idx).tabname <> ' ' and
               fldDefs(idx).FRNKEY <> 'Y' ;
             idx += 1;
           EndDo;

           If fldDefs(idx).FRNKEY = 'Y';
             keyName = fldDefs(idx).fld;
             refTable = fldDefs(idx).fkeytbl;
             getIDName( fldDefs(idx).fkeytbl : longRefName : shortRefName);

             // If the long and short names would be the same, the short name is blank
             If shortRefName = '' ;
               shortRefName = longRefName;
             EndIf;

           Else;
             fileEnd = *on;
           EndIf;


          return not fileEnd;
        END-PROC ;


        // --------------------------------------------------
       // Procedure name: updatePStatus
       // Purpose:
       // Returns:
       // Parameter:      sysTableName
       // --------------------------------------------------
       DCL-PROC updatePStatus EXPORT;
         DCL-PI *N;
           sysTableName like(shortName) const;
           newStatus    char(10) const;
         END-PI ;

         Exec SQL
           update tabledef
           set pstatus = :newStatus
           where lower(table_name) =  lower(:sysTableName);

         If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'Error when updating pstatus.')  ;
         EndIf;

         return ;
       END-PROC ;



       // --------------------------------------------------
       // Procedure name: getIDName
       // Purpose:
       // Returns:
       // Parameter:      tableName
       // Parameter:      longID
       // Parameter:      shortID
       // --------------------------------------------------
       DCL-PROC getIDName export;
         DCL-PI *N;
           sysTableName like(shortName) const;
           longID       like(longName);
           shortID      like(shortName);
         END-PI ;

         longID =  %trim(sysTableName) + 'ID';



         If %len(%trim(sysTableName)) < 9;
           // If the long and short names would be the same, blank out
           // the short name so it doesn't create a system name for the column
           shortID = '';
         Else;
           shortID =  %subst(sysTableName:1:8) + 'ID';
         EndIf;



         return ;
       END-PROC ;


       // --------------------------------------------------
       // Procedure Name: GetNextColumn
       // Purpose:
       // Returns:
       // Parameter:      fldDefs
       // Parameter:      idx
       // Parameter:      flddef
       // --------------------------------------------------
       Dcl-Proc GetNextColumn Export;
         Dcl-Pi *N ind;
           fldDefs Likeds(fld_ds) Dim(MAX_ARRAY) const;
           idx Packed(3:0) const;
           fldDef Like(fld_ds);
         End-Pi ;

         Dcl-s hasColumn ind inz(*on);

         Select;

           when idx > MAX_ARRAY;
             hasColumn = *off;

           when fldDefs(idx).TABNAME <> *blanks;
            fldDef = fldDefs(idx);

           other;
             hasColumn = *off;
         EndSl;


         Return hasColumn;
       End-Proc ;


       // --------------------------------------------------
       // Procedure Name: getDefinitionForNextColumn
       // Purpose:
       // Returns:
       // Parameter:      fldDefs
       // Parameter:      idx
       // Parameter:      flddef
       // --------------------------------------------------
       Dcl-Proc getDefinitionForNextColumn Export;
         Dcl-Pi *N ind;
           fldDefs      Likeds(fld_ds) Dim(MAX_ARRAY) const;
           idx          Packed(3:0) const;
           longFldName  like(longName);
           shortFldName like(shortName);
           dataTypeStr  char(12);
           defaultStr   char(12);
        End-Pi ;

         Dcl-s hasColumn ind inz(*on);
         Dcl-ds fldDef Likeds(fld_ds);

        If GetNextColumn(fldDefs : idx : fldDef);

           longFldName = %ScanRpl(' ' : '_' : %trim(fldDef.longfld));

           Select;
             when fldDef.frnkey = 'Y';
               getIDName(fldDef.fkeytbl : longFldName : shortFldName);
             when fldDef.fld = fldDef.longfld;
               shortFldName = '';
            other;
              shortFldName = fldDef.fld ;
           EndSl;



           Select;
             when fldDef.data_type = 'DATE';
                dataTypeStr =  'DATE';
             when %scan(fldDef.data_type : 'TIMESTMPTIMESTAMPTMSP') > 0;
                dataTypeStr =  'TIMESTAMP';
             when fldDef.data_type = 'SMALLINT';
                dataTypeStr =  'SMALLINT';
             when fldDef.data_type = 'BIGINT';
                dataTypeStr =  'BIGINT';
             when fldDef.data_type = 'INTEGER';
                dataTypeStr =  'INTEGER';
             when %scan(fldDef.data_type :  'DECIMALNUMERICFLOAT') > 0;
                dataTypeStr = %trim(fldDef.data_type) + '(' + %trim(%char(fldDef.data_size)) +
                ',' + %trim(%char(fldDef.numscale)) + ')';
             other ;
               dataTypeStr =  %trim(fldDef.data_type) + '(' + %trim(%char(fldDef.data_size)) + ')';
           EndSl;

            If  fldDef.defval <> *blanks and
                fldDef.data_type <> 'DATE' and
                fldDef.data_type <> 'TIMESTAMP' and
                fldDef.data_type <> 'SMALLINT' and
                fldDef.data_type <> 'BIGINT' and
                fldDef.data_type <> 'INTEGER' and
                fldDef.data_type <> 'DECIMAL' and
                fldDef.data_type <> 'NUMERIC' and
                fldDef.data_type <> 'FLOAT';
              defaultStr = 'DEFAULT ''' + %trim(fldDef.defval) + ''' ';
            Else;
              defaultStr = 'DEFAULT';
            EndIf;

         Else;
           hasColumn = *off;
         EndIf;


         Return hasColumn;
       End-Proc ;


       // --------------------------------------------------
       // Procedure name: addUniqueKey
       // Purpose:
       // Returns:
       // Parameter:      fldDs
       // Parameter:      longFldName
       // Parameter:      shortFldName
       // --------------------------------------------------
       DCL-PROC addUniqueKey export;
         DCL-PI *N;
           tableName    like(longName) const;
           shortFldName like(shortName) const;
         END-PI ;

           // writeLine ('');
           //  writeLine ('ALTER TABLE ' + tableName + '
             writeLine (SPACES_ + 'CONSTRAINT FILELIB_' + %trim(tableName) +
               '_UNIQUE UNIQUE(' + %trim(shortFldName) + 'ID)     -- comma?   <<=======<<<<<<');


         return ;
       END-PROC ;



