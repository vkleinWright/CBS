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
     /copy *libl/qrpglesrc,IFSIO_H


     /copy 'header/gentablepr'

     Dcl-c REGULAR  'R';
     Dcl-c TEMPORAL 'T';
     Dcl-c WORK     'W';
     Dcl-c OPEN_     *on;
     Dcl-c CLOSE_    *off;
     Dcl-c SPACES_   '    ';
     Dcl-c crlf      x'0D25';
     Dcl-c lf        x'25';

     Dcl-s longName   char(50);
     Dcl-s shortName  char(10);

      // ----------------------------------------------------------------
     // Prototype for procedure: main
     // ----------------------------------------------------------------

     Dcl-Pr Main EXTPGM('gentable');
        repoAddr      char(128) const; // where to write this
        tableName     like(longName) const;  // long name
        sysTableName  like(shortName) const;  // short name
        type          char(1) const;   // R - regular  T - temporal
     End-Pr ;

     Dcl-s fd   Int(10);

       // ----------------------- Main procedure interface
     Dcl-Pi  main;
        repoAddr      char(128) const; // where to write this
        tableName     like(longName) const; // long name
        sysTableName  char(10) const; // short name
        type          char(1) const;  // R - regular  T - temporal
     End-Pi ;

         Dcl-s isOK             ind inz(*on);
         Dcl-s upperTableName    char(50);
         Dcl-s lowerSysTableName like(shortName);
         Dcl-s upperSysTableName like(longName);

         Exec SQL
            set :upperTableName = upper(:tableName);

         Exec SQL
            set :upperSysTableName = upper(:sysTableName);

         Exec SQL
            set :lowerSysTableName = lower(:sysTableName);

        // load the fields from the csv
        If loadFieldsFromCSV(lowerSysTableName);
           // call buildTable
           buildTable(repoAddr : upperTableName : upperSysTableName : lowerSysTableName : type);
        EndIf;

       @close_file() ;

      *inlr = *on;

       // --------------------------------------------------
       // Procedure name: buildTable
       // Purpose:
       // Returns:
       // --------------------------------------------------
       DCL-PROC buildTable EXPORT;
         Dcl-Pi *N;
           repoAddr          char(128) const; // where to write this
           upperTableName    like(longName) const;
           upperSysTableName like(longName) const;
           lowerSysTableName like(shortName) const;
           type              char(1) const;  // R - regular  T - temporal
         End-Pi ;


         Dcl-s filePath char(150);

         filePath =  %trim(repoAddr) + '/source/' + %trim(sysTableName) + '.sql' ;


         // open the file
         @open_file(filePath );

         // make header
         // and table declarations
         makeHeader( upperTableName : upperSysTableName : lowerSysTableName);

         // add ID field for regular table
         If type = REGULAR;
            AddIDColumn(upperSysTableName);
         EndIf;

         // add fields (for R and T)

         runFieldCursor(sysTableName);

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

         // close Column List
         closeColumnList(upperSysTableName);

         // add lables
         writeLabels(upperTableName : tableName : upperSysTableName : type);

         // add temporal history
         If type = TEMPORAL;
           addTempHistory( upperTableName : upperSysTableName);
         EndIf;

         // add log
         addLog( upperTableName );

          return ;
        END-PROC ;

        // --------------------------------------------------
        // Procedure name: runFieldCursor
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC runFieldCursor EXPORT;
          Dcl-Pi *N;
            sysTableName  like(shortName) const; // short name
          End-Pi ;

          Dcl-s cursorOpen    ind;
          Dcl-s field_def     like(longName);
          Dcl-s column_def    char(22);
          Dcl-s type_def      char(12);
          Dcl-s default_def   char(12);

           Exec SQL
             declare c_fieldInfo cursor for
                select upper(cast( replace(text, ' ', '_') as char(50))) ,
                       upper(cast(case when fld = '' then ''  else 'FOR column '|| fld end  as char(22))),
                       cast(case
                          when upper(data_type) = 'DATE' then 'DATE'
                          when upper(data_type) = 'TIMESTMP' then 'TIMESTAMP'
                          when upper(data_type) = 'SMALLINT' then 'SMALLINT'
                          when upper(data_type) = 'BIGINT' then 'BIGINT'
                          when upper(data_type) = 'INTEGER' then 'INTEGER'
                          when upper(data_type) = 'DECIMAL' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
                          when upper(data_type) = 'NUMERIC' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
                          when upper(data_type) = 'FLOAT' then upper(trim(data_type))||'('||trim(char(data_size))|| ','|| trim(char(numeric_scale))||')'
                          else upper(trim(data_type))||'('||trim(char(data_size))|| ')' end  as char(12)),
                       cast('DEFAULT '|| case when data_type not in ('DATE', 'TIMESTAMP', 'SMALLINT', 'BIGINT', 'INTEGER', 'DECIMAL', 'NUMERIC', 'FLOAT')
                          and length(trim(default_val)) <> 0 then '''' || trim(default_val) ||'''' else '' end  as char(12))
                    from   tabledef t
                      where table_name =  :sysTableName
                      order by  rrn(t);



           DoW  fetchNextEmployeeToCheck(cursorOpen : field_def : column_def : type_def : default_def);

             writeLine( SPACES_ + %trim(field_def) +  '  ' +  %trim(column_def)  +  '  ' +
                        %trim(type_def) +  '  ' +  %trim(default_def) );

           EndDo;


          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: setFieldInfoCursor
        // Purpose:
        // Returns:
        // Parameter:      action
        // Parameter:      currentState
        // --------------------------------------------------
        DCL-PROC setFieldInfoCursor EXPORT;
          Dcl-Pi *N;
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
        // Procedure name: fetchNextEmployeeToCheck
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC fetchNextEmployeeToCheck EXPORT;
          Dcl-Pi *N IND;
            cursorOpen  ind;
            field_def   like(longName);
            column_def  char(22);
            type_def    char(12);
            default_def char(12);
          End-Pi ;

          Dcl-s fileEnd ind inz(*off);

          setFieldInfoCursor( OPEN_ : cursorOpen);

          If cursorOpen;

            Exec SQL
              fetch next from c_fieldInfo
              into :field_def, :column_def, :type_def, :default_def;


            fileEnd = (sqlstt = NO_MORE_ROWS or sqlstt <> '00000');

            If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when fetching field info.')  ;
            EndIf;

            If fileEnd;
               setFieldInfoCursor( CLOSE_ : cursorOpen );
            EndIf;

          EndIf;

        return not fileEnd;
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

          pvalue =  ' Create or Replace table FILELIB/' + %trim(upperTableName) + ' for system name ' +  %trim(upperSysTableName) + '(';

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

         Dcl-s pValue char(200);

         If %len(%trim(UpperSysTableName)) < 9;
           pValue =  SPACES_ +  %trim(UpperSysTableName) + 'ID BIGINT GENERATED ALWAYS AS IDENTITY (';
         Else;
           pValue =  SPACES_ +  %trim(UpperSysTableName) + 'ID  FOR COLUMN NAME ' +
                     %subst(UpperSysTableName:1:8) + 'ID GENERATED ALWAYS AS IDENTITY (';
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

         pValue =  SPACES_ + 'START_TS  TIMESTAMP(12) NOT NULL GENERATED ALWAYS AS ROW BEGIN, ';
         writeLine(pvalue);
         pValue =  SPACES_ + 'END_TS    TIMESTAMP(12) NOT NULL GENERATED ALWAYS AS ROW END,';
         writeLine(pvalue);
         pValue =  SPACES_ + 'TS_ID     TIMESTAMP(12) GENERATED ALWAYS AS TRANSACTION START ID,';
         writeLine(pvalue);
         pValue =  SPACES_ + 'PERIOD SYSTEM_TIME (START_TS, END_TS),';
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

         pValue =  SPACES_ + 'USERID' + SPACES_ + 'VARchar(18)  CCSID 37     NOT NULL  DEFAULT USER,';
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
         Dcl-s just_spaces char(50);
        // Dcl-s addSpaces varchar(50);
        // Dcl-s gap int(3) inz(5);

        // addSpaces = %subst(just_spaces : 1 : gap);

         pValue =  SPACES_ + 'UPDATED_BY' +
                         'FOR COLUMN    UPDATEDBY  VARchar(18)  CCSID 37     NOT NULL  DEFAULT USER,';
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


         pValue =  SPACES_ + 'ADDED_BY FOR COLUMN ADDEDBY    VARchar(18) ALLOCATE(18) CCSID 37 NOT NULL DEFAULT USER , ';
         writeLine(pvalue);


         pValue =  SPACES_ + 'ADDED_ON FOR COLUMN ADDEDON    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,';
         writeLine(pvalue);
         addUpdatedBy();
         pValue =  SPACES_ + 'UPDATED_ON FOR COLUMN UPDATEDON  TIMESTAMP GENERATED BY DEFAULT FOR EACH ROW ON UPDATE AS ROW CHANGE TIMESTAMP NOT NULL';
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
              where table_name =  :sysTableName;

         Exec SQL
          insert into tabledef
            (table_name, text, fld, data_type, data_size, numeric_scale, default_val)
            select
              :sysTableName,
              cast( substr(line, 1, locate_in_string(line, ',') -1) as varchar(50)),
               --the second
              cast( substr(line,   locate_in_string(line, ',') +1, locate_in_string(line, ',',1,  2) - locate_in_string(line, ',') -1) as char(10)),
               --the third
              cast(  substr(line,   locate_in_string(line, ',', 1,2) +1, locate_in_string(line, ',',1,  3) - locate_in_string(line, ',', 1,2) -1)  as char(12)),
               --the fourth
              cast(  substr(line,   locate_in_string(line, ',', 1,3) +1, locate_in_string(line, ',',1,  4) - locate_in_string(line, ',', 1,3) -1) as char(12)),
               --the fifth
              cast( substr(line,   locate_in_string(line, ',', 1,4) +1, locate_in_string(line, ',',1,  5) - locate_in_string(line, ',', 1,4) -1) as char(12)),
               --the sixth
              cast( substr(line,   locate_in_string(line, ',', 1,5) +1, 1) as char(1))
             FROM
              TABLE(QSYS2/IFS_READ(
               PATH_NAME => :mypath))x  ;


                If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when adding field info.')  ;

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
            sysTableName  like(shortName) const; // short name
            textFlag      char(1) const;
          End-Pi ;

          Dcl-s cursorOpen    ind;
          Dcl-s field_def     like(longName);
          Dcl-s field_lbl     like(longName);
          Dcl-s field_text    like(longName);


           Exec SQL
             declare c_labels cursor for
                select  upper( replace(text, ' ', '_') ) ,
                       proper_case(text), upper(substr(text, 1,1))||lower(substr(text,2))
                    from   tabledef t
                      where table_name =  :sysTableName
                      order by  rrn(t);



           DoW  fetchNextLabel(cursorOpen : field_def : field_lbl : field_text);
            If textFlag = 'Y';
             writeLine( SPACES_ + %trim(field_def) +  ' IS TEXT ''' +  %trim(field_text)  + ''',') ;
            Else ;
               writeLine( SPACES_ +  %trim(field_def) +  ' IS ''' +  %trim(field_lbl)  + ''',');
            EndIf;

           EndDo;


          return ;
         END-PROC ;


        // --------------------------------------------------
        // Procedure name: setLabelCursor
        // Purpose:
        // Returns:
        // Parameter:      action
        // Parameter:      currentState
        // --------------------------------------------------
        DCL-PROC setLabelCursor EXPORT;
          Dcl-Pi *N;
            action     ind const;
            cursorOpen ind;
          End-Pi ;


              Select;
                when action = OPEN_ and not cursorOpen;

                  Exec SQL
                     open c_labels;

                  If xSQLState2 <> Success_On_SQL;
                     logMsgAndSQLError( program : %proc() : xSQLState :
                      'Unable to open label cursor ');
                  Else;
                    cursorOpen = *on;
                  EndIf;

                when action = CLOSE_ and cursorOpen;
                     cursorOpen = *off;
                  Exec SQL
                     close  c_labels;
              EndSl;

          return ;
        END-PROC ;

        // --------------------------------------------------
        // Procedure name: fetchNextLabel
        // Purpose:
        // Returns:
        // --------------------------------------------------
        DCL-PROC fetchNextLabel EXPORT;
          Dcl-Pi *N IND;
            cursorOpen    ind;
            field_def     like(longName);
            field_lbl     like(longName);
            field_text    like(longName);
          End-Pi ;

          Dcl-s fileEnd ind inz(*off);

          setLabelCursor( OPEN_ : cursorOpen);

          If cursorOpen;

            Exec SQL
              fetch next from c_labels
              into :field_def, :field_lbl, :field_text;


            fileEnd = (sqlstt = NO_MORE_ROWS or sqlstt <> '00000');

            If (sqlstt <> NO_MORE_ROWS and sqlstt <> '00000');
               logMsgAndSQLError( program : %proc() : xSQLState :
                   'failed when fetching label info.')  ;
            EndIf;

            If fileEnd;
               setLabelCursor( CLOSE_ : cursorOpen );
            EndIf;

          EndIf;

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
           upperTableName    like(longName) const;
           tableName         like(shortName) const;
           upperSysTableName like(shortName) const;
           type              char(1) const;  // R - regular  T - temporal
         End-Pi ;

         writeLine (' ');
         writeLine (' ');
         writeLine ('LABEL ON TABLE FILELIB/' + %trim(upperTableName));
         writeLine ('  IS ''   ''');


         writeLine (' ');
         writeLine (' ');
         writeLine ('LABEL ON COLUMN FILELIB/' + %trim(upperTableName));
         writeLine ('(');
         runLabelCursor(tableName : 'N');
         If type <> WORK;
            writeLine( SPACES_ + %trim(UpperSysTableName) + 'ID  IS ''' + %trim(UpperSysTableName) + ' ID'',');
         EndIf;

         If type = REGULAR;
            writeLine ( SPACES_ + 'ADDED_BY IS ''Added by'' , ');
            writeLine ( SPACES_ + 'ADDED_ON IS ''Added on'' , ');
            writeLine ( SPACES_ + 'UPDATED_BY IS ''Updated by'' , ');
            writeLine ( SPACES_ + 'UPDATED_ON IS ''Updated on'' ');
         EndIf;
         writeLine (');');

         writeLine (' ');

         writeLine ('LABEL ON COLUMN FILELIB/' + %trim(upperTableName));
         writeLine ('(');
         runLabelCursor(tableName : 'Y');
         If type <> WORK;
           writeLine( SPACES_ + %trim(UpperSysTableName) + 'ID TEXT  IS ''' + %trim(UpperSysTableName) + ' ID'',');
         EndIf;
         If type = REGULAR;
            writeLine ( SPACES_ + 'ADDED_BY TEXT IS ''Added by'' , ');
            writeLine ( SPACES_ + 'ADDED_ON TEXT IS ''Added on'' , ');
            writeLine ( SPACES_ + 'UPDATED_BY TEXT IS ''Updated by'' , ');
            writeLine ( SPACES_ + 'UPDATED_ON TEXT IS ''Updated on'' ');
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
                        %trim(upperTableName) + ' ;');

         writeLine ('');
         writeLine ('LABEL ON TABLE FILELIB/' + %trim(upperTableName) +
                        '_HIST IS ''   hist'' ;');

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

