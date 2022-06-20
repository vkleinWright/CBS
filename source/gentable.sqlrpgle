**Free
//     *================================================================================
//     *
//     *
//     * Object....: gentable
//     * Purpose...: Create a table from source in qtemp
//     *
//     *
//     * Author....: Virginia Klein
//     * Date......: 11-04-2021
//     *
//     *--------------------------------------------------------------------------------
//     * Modifications:
//     * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//     * Proj#    Date      Init  Description
//     * -----   ---------- ----  -----------
//     *
//     *
//     *--------------------------------------------------------------------------------
    Ctl-Opt Option(*Srcstmt: *Nodebugio);

     /copy 'header/gentblmpr'

      Dcl-s longName   char(50);
      Dcl-s shortName  char(10);

      // ----------------------------------------------------------------
     // Prototype for procedure: main
     // ----------------------------------------------------------------

     Dcl-Pr Main EXTPGM('gentable');
        repoAddr      char(128) const; // where to write this
        tableName     like(longName) const;  // long name
        sysTableName  like(shortName) const;  // short name
        tableText     like(longName) const;  // table description
        type          char(1) const;   // R - regular  T - temporal
        source        char(3) const;   // CSV - for spreadsheet FIL - skip
                                       //the load and just use the file
     End-Pr ;


       // ----------------------- Main procedure interface
     Dcl-Pi  main;
        repoAddr      char(128) const; // where to write this
        tableName     like(longName) const; // long name
        sysTableName  char(10) const; // short name
        tableText     like(longName) const;  // table description
        type          char(1) const;  // R - regular  T - temporal  W - work
        source        char(3) const;  // CSV - for spreadsheet  FIL - skip the
                                      // load and just use the file
     End-Pi ;

        Dcl-s isOK             ind inz(*on);


        If source = 'CSV';
          // load the fields from the csv
          IsOK = loadFieldsFromCSV(sysTableName);
        EndIf;

        If isOK;
           // call buildTable
           buildTable(repoAddr : tableName : sysTableName : tableText : type);
        EndIf;



      *inlr = *on;

