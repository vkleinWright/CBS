**Free

       /copy *libl/qrpglesrc,IFSIO_H
       /copy 'header/pgmds'

       dcl-s fd   Int(10);
       dcl-c crlf    x'0D25';
       dcl-s $xml  char(512);

       dcl-s $file_name  char(50);
       dcl-s $file_path  char(100);

//d ProgStatus     sds
//d  Parms            *PARMS
//d  ProgName         *PROC
//d  ErrMsgID              40     46
//d  ErrMsg                91    169
//d  JobName              244    253
//d  Userid               254    263
//d  JobNumber            264    269

Dcl-Pr Main EXTPGM('gentable');
        repoAddr      char(128) const; // where to write this
        tableName     char(50) const;  // long name
        sysTableName  char(10) const;  // short name
        type          char(1) const;   // R - regular  T - temporal
     End-Pr ;

  Dcl-Pi  main;
        repoAddr      char(128) const; // where to write this
        tableName     char(50) const; // long name
        sysTableName  char(10) const; // short name
        type          char(1) const;  // R - regular  T - temporal
     End-Pi ;

     dcl-s isOK      ind inz(*on);
               @open_file();
               @write_data();
               @close_file();
            *inlr = *on;
         return;




 ///*----- Open IFS file
    DCL-PROC @open_file ;
         DCL-PI *N End-PI;

               $file_name = 'Mydata.sql' ;
               $file_path = '/home/VIRGINIAK/repos/play/CBS/source/' +
                                         %trim($file_name)  ;


       fd = open(%trim($file_path)
             : O_WRONLY+O_CREAT+O_TRUNC+O_CCSID
             : S_IRGRP + S_IWGRP + S_IXGRP
             : 819);

    callp close(fd);
   fd = open(%trim($file_path):O_WRONLY+O_TEXTDATA);

   $xml = '<?xml version="1.0" encoding="UTF-8"?>' + crlf +
          '<MyData>' + crlf;
   callp write(fd: %addr($xml): %len(%trim($xml)));


  return;
End-proc;
// *
// *----- Close IFS file
// *
 DCL-PROC @close_file ;
         DCL-PI *N;
         End-PI;

   $xml = '</MyData>' + crlf;
   callp write(fd: %addr($xml): %len(%trim($xml)));
   callp close(fd);
   return;
 End-Proc;

// *
// *----- Write Data into the IFS file
//*
 DCL-PROC @write_data ;
         DCL-PI *N;
           End-PI;

   $xml = '<Name>' +
            'My name is $user_id' +
          '</Name>' + crlf;
   callp write(fd: %addr($xml): %len(%trim($xml)));

   $xml = '<Date>' +
            'Today is ' + %char(%date()) +
          '</Date>' + crlf;
   callp write(fd: %addr($xml): %len(%trim($xml)));

End-Proc;
