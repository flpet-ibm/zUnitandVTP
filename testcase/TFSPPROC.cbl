       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU),NOSEQ
      *+---------------------------------------------------------------+
      *| TFSPPROC                                                      |
      *| UNIT TEST FOR Z/OS: TEST CASE PROGRAM                         |
      *| TEST CASE VERSION: 202                                        |
      *| DATE GENERATED: 07/01/2025 12:31                              |
      *| ID: ea999784-42dd-4a56-a7c6-df377acb273f                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: TEST_TEST1                                |
      *|     FOR TEST TEST1                                            |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'FSPIVA65'.
       01 AZ-CSECT       PIC X(72) VALUE 'FSPPROCI'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 BZUGETEP          PIC X(8) VALUE 'BZUGETEP'.
       01 AZ-EP-PTR         USAGE IS POINTER.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-RC-WORK             PIC S9(4) USAGE BINARY.
       01 AZ-SUB-GETARG     PIC X(8)  VALUE 'BZUGTARG'.
       01 AZ-SUB-PROGRAM    PIC X(8)  VALUE 'FSPIVA65'.
       01 AZ-SUB-CSECT      PIC X(72) VALUE 'FSPPROCI'.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
      *  *** INPUT-AREA : ZUT00000037
       1 ZUT00000037.
      *    *** IN-BLANK : ZUT00000038
         2 ZUT00000038 PIC  X(80).
      *    *** IN-TEXT : ZUT00000039
         2 ZUT00000039 REDEFINES ZUT00000038.
      *    *** IN-COMMAND : ZUT0000003A
         3 ZUT0000003A PIC  X(8).
      *    *** TEMP-COMMAND : ZUT0000003B
         3 ZUT0000003B REDEFINES ZUT0000003A.
      *    *** TEMP-IOCMD : ZUT0000003C
         4 ZUT0000003C PIC  X(3).
      *    *** FILLER : ZUT0000003D
         4 ZUT0000003D PIC  X(5).
      *    *** IN-LAST-NAME : ZUT0000003E
         3 ZUT0000003E PIC  X(10).
      *    *** IN-FIRST-NAME : ZUT0000003F
         3 ZUT0000003F PIC  X(10).
      *    *** IN-EXTENSION : ZUT00000040
         3 ZUT00000040 PIC  X(10).
      *    *** IN-ZIP-CODE : ZUT00000041
         3 ZUT00000041 PIC  X(7).
      *    *** INFILL : ZUT00000042
         3 ZUT00000042 PIC  X(35).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
      *  *** GIPCB : ZUT00000056
       1 ZUT00000056.
      *    *** DBD-NAME : ZUT00000057
         2 ZUT00000057 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000058
         2 ZUT00000058 PIC  X(2).
      *    *** GI-STATUS : ZUT00000059
         2 ZUT00000059 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT0000005A
         2 ZUT0000005A PIC  X(4).
      *    *** RESERVE-DLI : ZUT0000005B
         2 ZUT0000005B PIC  X(4).
      *    *** SEG-NAME-FB : ZUT0000005C
         2 ZUT0000005C PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT0000005D
         2 ZUT0000005D PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT0000005E
         2 ZUT0000005E PIC  9(4).
      *    *** KEY-FB-AREA : ZUT0000005F
         2 ZUT0000005F PIC  X(17).
      *  *** GOPCB : ZUT00000060
       1 ZUT00000060.
      *    *** DBD-NAME : ZUT00000061
         2 ZUT00000061 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000062
         2 ZUT00000062 PIC  X(2).
      *    *** GO-STATUS : ZUT00000063
         2 ZUT00000063 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000064
         2 ZUT00000064 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000065
         2 ZUT00000065 PIC  x(4).
      *    *** SEG-NAME-FB : ZUT00000066
         2 ZUT00000066 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000067
         2 ZUT00000067 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000068
         2 ZUT00000068 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000069
         2 ZUT00000069 PIC  X(17).
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY EQAITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       1 AZ-PSB-ADDRS.
           3 AZ-PCB1 POINTER.
           3 AZ-PCB2 POINTER.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
       01  AZ-SUB-PGM-LIST.
         03  AZ-SUB-PGM-COUNT       PIC 9(4) COMP-4.
         03  AZ-SUB-PGM-ADDRS  OCCURS 1 TO 100 TIMES
                       DEPENDING ON AZ-SUB-PGM-COUNT.
           05  AZ-SUB-PGM-ADDR      USAGE POINTER.
           05  AZ-SUB-PGM-LGTH      PIC 9(8) COMP-4.
       01  AZ-LINKPARM1             PIC X(32768).
       01  AZ-LINKPARM2             PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-PSB-ADDRS AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET ADDRESS FOR DBPCB
           SET ADDRESS OF ZUT0000004C TO AZ-PCB2
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR FSPPROCI'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM FSPPROCI NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 5
                MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
                STRING 'SUB PROGRAM ARGUMENT COUNT DOES NOT MATCH.'
                  DELIMITED BY SIZE
                  INTO MESSAGE-TXT OF BZ-ASSERT
                  WITH POINTER MESSAGE-LEN OF BZ-ASSERT
                END-STRING
                SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
                PERFORM THROW-ASSERTION-M
              END-IF
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(1)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000037
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(1)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(1))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(2)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000043
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(2)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(2))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(4)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000056
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(4)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(4))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(5)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT00000060
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(5)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(5))
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL FSPIVA65 (CSECT:FSPPROCI)'
           CALL BZUGETEP USING BY REFERENCE PROGRAM-NAME AZ-CSECT
             RETURNING AZ-EP-PTR.
           IF AZ-EP-PTR = NULL THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'UNABLE TO GET THE ENTRY POINT BY BZUGETEP.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
             GOBACK
           END-IF
           SET ADDRESS OF AZ-PROC-PTR TO AZ-EP-PTR.
           CALL AZ-PROC-PTR
           USING BY REFERENCE ZUT00000037 ZUT00000043 BY VALUE AZ-PCB2
           BY REFERENCE ZUT00000056 ZUT00000060
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       INITIALIZE-PARM.
           INITIALIZE ZUT00000037
           INITIALIZE ZUT00000043
           INITIALIZE ZUT00000056
           INITIALIZE ZUT00000060
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM TEST_TEST1.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_TEST                                  |
      *|     CALLBACK DEFINITION FOR TEST                              |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'FSPIVA65'.
       01 AZ-CSECT       PIC X(72) VALUE 'FSPPROCI'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 ASSERT-ST.
         03 ASSERT-RC PIC 9(9) BINARY VALUE 4.
         03 ASSERT-TEXT PIC 9(4) BINARY VALUE 0.
       01 AZ-TEST-NAME-LEN       PIC S9(9) COMP-5.
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RC-WORK          PIC S9(4) USAGE BINARY.
       01 AZ-OUTPUT-COUNT-STR PIC X(5).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
      *  *** INPUT-AREA : ZUT00000037
       1 ZUT00000037.
      *    *** IN-BLANK : ZUT00000038
         2 ZUT00000038 PIC  X(80).
      *    *** IN-TEXT : ZUT00000039
         2 ZUT00000039 REDEFINES ZUT00000038.
      *    *** IN-COMMAND : ZUT0000003A
         3 ZUT0000003A PIC  X(8).
      *    *** TEMP-COMMAND : ZUT0000003B
         3 ZUT0000003B REDEFINES ZUT0000003A.
      *    *** TEMP-IOCMD : ZUT0000003C
         4 ZUT0000003C PIC  X(3).
      *    *** FILLER : ZUT0000003D
         4 ZUT0000003D PIC  X(5).
      *    *** IN-LAST-NAME : ZUT0000003E
         3 ZUT0000003E PIC  X(10).
      *    *** IN-FIRST-NAME : ZUT0000003F
         3 ZUT0000003F PIC  X(10).
      *    *** IN-EXTENSION : ZUT00000040
         3 ZUT00000040 PIC  X(10).
      *    *** IN-ZIP-CODE : ZUT00000041
         3 ZUT00000041 PIC  X(7).
      *    *** INFILL : ZUT00000042
         3 ZUT00000042 PIC  X(35).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** GIPCB : ZUT00000056
       1 ZUT00000056.
      *    *** DBD-NAME : ZUT00000057
         2 ZUT00000057 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000058
         2 ZUT00000058 PIC  X(2).
      *    *** GI-STATUS : ZUT00000059
         2 ZUT00000059 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT0000005A
         2 ZUT0000005A PIC  X(4).
      *    *** RESERVE-DLI : ZUT0000005B
         2 ZUT0000005B PIC  X(4).
      *    *** SEG-NAME-FB : ZUT0000005C
         2 ZUT0000005C PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT0000005D
         2 ZUT0000005D PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT0000005E
         2 ZUT0000005E PIC  9(4).
      *    *** KEY-FB-AREA : ZUT0000005F
         2 ZUT0000005F PIC  X(17).
      *  *** GOPCB : ZUT00000060
       1 ZUT00000060.
      *    *** DBD-NAME : ZUT00000061
         2 ZUT00000061 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000062
         2 ZUT00000062 PIC  X(2).
      *    *** GO-STATUS : ZUT00000063
         2 ZUT00000063 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000064
         2 ZUT00000064 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000065
         2 ZUT00000065 PIC  x(4).
      *    *** SEG-NAME-FB : ZUT00000066
         2 ZUT00000066 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000067
         2 ZUT00000067 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000068
         2 ZUT00000068 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000069
         2 ZUT00000069 PIC  X(17).
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_FSPIVA65_FSPPROCI" USING AZ-TEST
           AZ-INFO-BLOCK
           ZUT00000037 ZUT00000043 ZUT0000004C ZUT00000056 ZUT00000060.
           DISPLAY
           'AZU0000I PGM_INPT_FSPIVA65_FSPPROCI INPUT VALUES...' .
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_FSPIVA65_FSPPROCI" USING AZ-TEST
           AZ-INFO-BLOCK
           ZUT00000037 ZUT00000043 ZUT0000004C ZUT00000056 ZUT00000060.
           DISPLAY
           'AZU0000I PGM_OUTP_FSPIVA65_FSPPROCI CHECK VALUES...' .
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM CHECK-REC-TEST1
             MOVE 4 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           EXIT.
       CHECK-REC-TEST1.
      * CHECK RECORD COUNT FOR TEST1
      * FOR FSPISRTO
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN FSPISRTO.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI ISRT ''DBPCB''
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI ISRT ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI GU ''DBPCB''
           MOVE 3 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 1 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''1''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI GU ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI GHU ''DBPCB''
           MOVE 4 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI GHU ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI REPL ''DBPCB''
           MOVE 5 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI REPL ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI DLET ''DBPCB''
           MOVE 6 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 0 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''0''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI DLET ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       THROW-ASSERTION-M.
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           DISPLAY 'AZU2001W THE TEST "' AZ-TEST(1:AZ-TEST-NAME-LEN) '"
      -    'FAILED DUE TO AN ASSERTION.'
           DISPLAY 'AZU1101I ' MESSAGE-TXT OF BZ-ASSERT(1:MESSAGE-LEN
           OF BZ-ASSERT)
           DISPLAY 'AZU0000I *******************************************
      -    '*************************************'
           CALL BZUASSRT USING BZ-P1 BZ-P2 BZ-P3 BZ-ASSERT
           EXIT.
       END PROGRAM BZU_TEST.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_INIT                                  |
      *|     INITIAL PROCEDURE                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE 'ea999784-42dd-4a56-a7c6-df377acb273f'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-TEST-ID
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_INIT: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           DISPLAY 'AZU0000I TEST CASE VERSION: 202'
           DISPLAY 'AZU0001I FOR TEST RUNNER: latest'
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_TERM                                  |
      *|     TERMINATION PROCEDURE                                     |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: FSPISRTO                                  |
      *|     SUB PROGRAM CALLED FROM PROGRAM UNDER TEST                |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_FSPISRTO'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM STUB CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN       PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** OUTPUT-AREA : ZUT00000021
       1 ZUT00000021.
      *    *** OUT-BLANK : ZUT00000022
         2 ZUT00000022 PIC  X(85).
      *    *** OUT-TEXT : ZUT00000023
         2 ZUT00000023 REDEFINES ZUT00000022.
      *    *** OUT-MESSAGE : ZUT00000024
         3 ZUT00000024 PIC  X(40).
      *    *** OUT-COMMAND : ZUT00000025
         3 ZUT00000025 PIC  X(8).
      *    *** OUT-DATA : ZUT00000026
         3 ZUT00000026.
      *    *** OUT-LAST-NAME : ZUT00000027
         4 ZUT00000027 PIC  X(10).
      *    *** OUT-FIRST-NAME : ZUT00000028
         4 ZUT00000028 PIC  X(10).
      *    *** OUT-EXTENSION : ZUT00000029
         4 ZUT00000029 PIC  X(10).
      *    *** OUT-ZIP-CODE : ZUT0000002A
         4 ZUT0000002A PIC  X(7).
      *    *** OUT-SEGMENT-NO : ZUT0000002B
         2 ZUT0000002B PIC  9(4).
      *    *** OUT-FILL : ZUT0000002C
         2 ZUT0000002C PIC  X(32).
      *  *** GIPCB : ZUT00000056
       1 ZUT00000056.
      *    *** DBD-NAME : ZUT00000057
         2 ZUT00000057 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000058
         2 ZUT00000058 PIC  X(2).
      *    *** GI-STATUS : ZUT00000059
         2 ZUT00000059 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT0000005A
         2 ZUT0000005A PIC  X(4).
      *    *** RESERVE-DLI : ZUT0000005B
         2 ZUT0000005B PIC  X(4).
      *    *** SEG-NAME-FB : ZUT0000005C
         2 ZUT0000005C PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT0000005D
         2 ZUT0000005D PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT0000005E
         2 ZUT0000005E PIC  9(4).
      *    *** KEY-FB-AREA : ZUT0000005F
         2 ZUT0000005F PIC  X(17).
      *  *** GOPCB : ZUT00000060
       1 ZUT00000060.
      *    *** DBD-NAME : ZUT00000061
         2 ZUT00000061 PIC  X(8).
      *    *** SEG-LEVEL : ZUT00000062
         2 ZUT00000062 PIC  X(2).
      *    *** GO-STATUS : ZUT00000063
         2 ZUT00000063 PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000064
         2 ZUT00000064 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000065
         2 ZUT00000065 PIC  x(4).
      *    *** SEG-NAME-FB : ZUT00000066
         2 ZUT00000066 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000067
         2 ZUT00000067 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000068
         2 ZUT00000068 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000069
         2 ZUT00000069 PIC  X(17).
      *
       PROCEDURE DIVISION.
       ENTRY_INPT.
      * ENTRY FOR CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_FSPISRTO" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000021 ZUT00000056 ZUT00000060.
           DISPLAY 'AZU0000I PGM_INPT_FSPISRTO CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
      * ENTRY FOR CHECK OUTPUT VALUE WITH CSECT
           ENTRY "PGM_INPT_FSPIVA65_FSPISRTO" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000021 ZUT00000056 ZUT00000060.
           DISPLAY
           'AZU0000I PGM_INPT_FSPIVA65_FSPISRTO CHECK VALUES...' .
           PERFORM PROC_INPT.
           GOBACK.
       ENTRY_OUTP.
      * ENTRY FOR SET INPUT VALUE
           ENTRY "PGM_OUTP_FSPISRTO" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000021 ZUT00000056 ZUT00000060.
           DISPLAY 'AZU0000I PGM_OUTP_FSPISRTO INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
      * ENTRY FOR SET INPUT VALUE WITH CSECT
           ENTRY "PGM_OUTP_FSPIVA65_FSPISRTO" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000021 ZUT00000056 ZUT00000060.
           DISPLAY
           'AZU0000I PGM_OUTP_FSPIVA65_FSPISRTO INPUT VALUES...' .
           PERFORM PROC_OUTP.
           GOBACK.
       PROC_INPT.
      * CHECK OUTPUT VALUE
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       PROC_OUTP.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 1 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-WK-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_FSPISRTO END.'
           EXIT.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'PGM_FSPISRTO'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: GTMEMRC                                   |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'GTMEMRC'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZUGTMEM            PIC X(8) VALUE 'BZUGTMEM'.
       01 DATA-SIZE           PIC 9(8) COMP-4.
       LINKAGE SECTION.
       01 AZ-TC-WORK-AREA        PIC X(256).
       01 AZ-GRP-INDEX        PIC 9(8).
       01 AZ-FLAG-IN          PIC 9(1).
       01 AZ-RECORD-PTR       POINTER.
       01 AZ-RECORD-PTR-VALUE
            REDEFINES AZ-RECORD-PTR  PIC S9(9) COMP-5.
       01 DATA-PTR            POINTER.
       01 DATA-PTR-VALUE
            REDEFINES DATA-PTR  PIC S9(9) COMP-5.
       01 DATA-AREA.
         03 RECORD-COUNT-IO OCCURS 6.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       PROCEDURE DIVISION USING AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
       MAINPROC.
      * ENTRY FOR CALLBACK
           ENTRY "PGM_INPT_GTMEMRC" USING AZ-TEST AZ-INFO-BLOCK
             AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR.
      * GET DATA AREA
           SET ADDRESS OF DATA-PTR TO ADDRESS OF AZ-TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 6
             CALL BZUGTMEM USING DATA-SIZE RETURNING DATA-PTR
             SET ADDRESS OF DATA-AREA TO DATA-PTR
             DISPLAY 'AZU0000I AREA ALLOCATED FOR RECORD COUNT:'
           DATA-SIZE
           END-IF
           SET AZ-RECORD-PTR TO DATA-PTR
           COMPUTE AZ-RECORD-PTR-VALUE = AZ-RECORD-PTR-VALUE +
                 LENGTH OF WK-RECORD-COUNT * 2 * (AZ-GRP-INDEX - 1)
           IF AZ-FLAG-IN = 1 THEN
             ADD LENGTH OF WK-RECORD-COUNT TO AZ-RECORD-PTR-VALUE
           END-IF
           SET ADDRESS OF WK-RECORD-COUNT TO AZ-RECORD-PTR
           GOBACK.
       END PROGRAM 'GTMEMRC'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: AZU_GENERIC_IMS                           |
      *|   GENERIC IMS CALLBACK EXIT POINT                             |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_IMS'.
       DATA DIVISION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT.
           ENTRY 'IMS_INPT' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I IMS_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * IMS_OUTP.
           ENTRY 'IMS_OUTP' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I IMS_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_IMS'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: ISRT                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_ISRT_FSPIVA65'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM IMS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN        PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT        PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT           PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE           PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR       POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9) VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-COUNT       PIC 9(8) COMP-4.
       01 AZ-ACMDVA          PIC X(4).
       01 AZ-APCBVA          PIC X(44).
       01 FILLER  REDEFINES AZ-APCBVA.
         03 DBPCBDBD         PIC X(8).
         03 DBPCBLEV         PIC X(2).
         03 DBPCBSTC         PIC X(2).
         03 DBPCBPRO         PIC X(4).
         03 DBPCBPFX         USAGE POINTER.
         03 DBPCBSFD         PIC X(8).
         03 DBPCBLKY         PIC 9(8) COMP-4.
         03 DBPCBNSS         PIC 9(8) COMP-4.
       01 AZ-PCB-PREFIX.
         03 DBPCBLEN         PIC 9(4) COMP-4.
         03 DBPCBPLN         PIC 9(4) COMP-4.
         03 DBPCBNUM         PIC 9(4) COMP-4.
         03 DBPCBFLG         PIC X.
         03 DBPCBFL2         PIC X.
       01 AZ-DBPCB.
         03 DBD-NAME         PIC  X(8).
         03 SEG-LEVEL        PIC  X(2).
         03 DBSTATUS         PIC  X(2).
         03 PROC-OPTIONS     PIC  X(4).
         03 RESERVE-DLI      PIC  X(4).
         03 SEG-NAME-FB      PIC  X(8).
         03 LENGTH-FB-KEY    PIC  9(4).
         03 NUMB-SENS-SEGS   PIC  9(4).
         03 KEY-FB-AREA      PIC  X(17).
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
      *  *** SSA1 : ZUT0000001B
       77 ZUT0000001B PIC X(9).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_ISRT_FSPIVA65.
           ENTRY 'IMS_INPT_ISRT_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT0000001B.
           DISPLAY 'AZU0000I IMS_ISRT_FSPIVA65 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI ISRT (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (DBPCB:ARG=4)'
             MOVE 2 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * IMS_OUTP_ISRT_FSPIVA65.
           ENTRY 'IMS_OUTP_ISRT_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT0000001B.
           DISPLAY 'AZU0000I IMS_ISRT_FSPIVA65 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI ISRT (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (DBPCB:ARG=4)'
             MOVE 2 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I IMS_ISRT_FSPIVA65 END.'
           EXIT.
       END PROGRAM 'IMS_ISRT_FSPIVA65'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: GU                                           |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_GU_FSPIVA65'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM IMS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN        PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT        PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT           PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE           PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR       POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9) VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-COUNT       PIC 9(8) COMP-4.
       01 AZ-ACMDVA          PIC X(4).
       01 AZ-APCBVA          PIC X(44).
       01 FILLER  REDEFINES AZ-APCBVA.
         03 DBPCBDBD         PIC X(8).
         03 DBPCBLEV         PIC X(2).
         03 DBPCBSTC         PIC X(2).
         03 DBPCBPRO         PIC X(4).
         03 DBPCBPFX         USAGE POINTER.
         03 DBPCBSFD         PIC X(8).
         03 DBPCBLKY         PIC 9(8) COMP-4.
         03 DBPCBNSS         PIC 9(8) COMP-4.
       01 AZ-PCB-PREFIX.
         03 DBPCBLEN         PIC 9(4) COMP-4.
         03 DBPCBPLN         PIC 9(4) COMP-4.
         03 DBPCBNUM         PIC 9(4) COMP-4.
         03 DBPCBFLG         PIC X.
         03 DBPCBFL2         PIC X.
       01 AZ-DBPCB.
         03 DBD-NAME         PIC  X(8).
         03 SEG-LEVEL        PIC  X(2).
         03 DBSTATUS         PIC  X(2).
         03 PROC-OPTIONS     PIC  X(4).
         03 RESERVE-DLI      PIC  X(4).
         03 SEG-NAME-FB      PIC  X(8).
         03 LENGTH-FB-KEY    PIC  9(4).
         03 NUMB-SENS-SEGS   PIC  9(4).
         03 KEY-FB-AREA      PIC  X(17).
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
      *  *** SSA : ZUT00000032
       1 ZUT00000032.
      *    *** SEGMENT-NAME : ZUT00000033
         2 ZUT00000033 PIC X(8).
      *    *** SEG-KEY-NAME : ZUT00000034
         2 ZUT00000034 PIC X(11).
      *    *** SSA-KEY : ZUT00000035
         2 ZUT00000035 PIC X(10).
      *    *** FILLER : ZUT00000036
         2 ZUT00000036 PIC X.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_GU_FSPIVA65.
           ENTRY 'IMS_INPT_GU_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT00000032.
           DISPLAY 'AZU0000I IMS_GU_FSPIVA65 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (DBPCB:ARG=4)'
             MOVE 3 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * IMS_OUTP_GU_FSPIVA65.
           ENTRY 'IMS_OUTP_GU_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT00000032.
           DISPLAY 'AZU0000I IMS_GU_FSPIVA65 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (DBPCB:ARG=4)'
             MOVE 3 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I IMS_GU_FSPIVA65 END.'
           EXIT.
       END PROGRAM 'IMS_GU_FSPIVA65'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: GHU                                          |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_GHU_FSPIVA65'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM IMS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN        PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT        PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT           PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE           PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR       POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9) VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-COUNT       PIC 9(8) COMP-4.
       01 AZ-ACMDVA          PIC X(4).
       01 AZ-APCBVA          PIC X(44).
       01 FILLER  REDEFINES AZ-APCBVA.
         03 DBPCBDBD         PIC X(8).
         03 DBPCBLEV         PIC X(2).
         03 DBPCBSTC         PIC X(2).
         03 DBPCBPRO         PIC X(4).
         03 DBPCBPFX         USAGE POINTER.
         03 DBPCBSFD         PIC X(8).
         03 DBPCBLKY         PIC 9(8) COMP-4.
         03 DBPCBNSS         PIC 9(8) COMP-4.
       01 AZ-PCB-PREFIX.
         03 DBPCBLEN         PIC 9(4) COMP-4.
         03 DBPCBPLN         PIC 9(4) COMP-4.
         03 DBPCBNUM         PIC 9(4) COMP-4.
         03 DBPCBFLG         PIC X.
         03 DBPCBFL2         PIC X.
       01 AZ-DBPCB.
         03 DBD-NAME         PIC  X(8).
         03 SEG-LEVEL        PIC  X(2).
         03 DBSTATUS         PIC  X(2).
         03 PROC-OPTIONS     PIC  X(4).
         03 RESERVE-DLI      PIC  X(4).
         03 SEG-NAME-FB      PIC  X(8).
         03 LENGTH-FB-KEY    PIC  9(4).
         03 NUMB-SENS-SEGS   PIC  9(4).
         03 KEY-FB-AREA      PIC  X(17).
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
      *  *** SSA : ZUT00000032
       1 ZUT00000032.
      *    *** SEGMENT-NAME : ZUT00000033
         2 ZUT00000033 PIC X(8).
      *    *** SEG-KEY-NAME : ZUT00000034
         2 ZUT00000034 PIC X(11).
      *    *** SSA-KEY : ZUT00000035
         2 ZUT00000035 PIC X(10).
      *    *** FILLER : ZUT00000036
         2 ZUT00000036 PIC X.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_GHU_FSPIVA65.
           ENTRY 'IMS_INPT_GHU_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT00000032.
           DISPLAY 'AZU0000I IMS_GHU_FSPIVA65 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GHU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GHU (DBPCB:ARG=4)'
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * IMS_OUTP_GHU_FSPIVA65.
           ENTRY 'IMS_OUTP_GHU_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043
           ZUT00000032.
           DISPLAY 'AZU0000I IMS_GHU_FSPIVA65 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GHU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GHU (DBPCB:ARG=4)'
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I IMS_GHU_FSPIVA65 END.'
           EXIT.
       END PROGRAM 'IMS_GHU_FSPIVA65'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: REPL                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_REPL_FSPIVA65'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM IMS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN        PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT        PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT           PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE           PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR       POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9) VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-COUNT       PIC 9(8) COMP-4.
       01 AZ-ACMDVA          PIC X(4).
       01 AZ-APCBVA          PIC X(44).
       01 FILLER  REDEFINES AZ-APCBVA.
         03 DBPCBDBD         PIC X(8).
         03 DBPCBLEV         PIC X(2).
         03 DBPCBSTC         PIC X(2).
         03 DBPCBPRO         PIC X(4).
         03 DBPCBPFX         USAGE POINTER.
         03 DBPCBSFD         PIC X(8).
         03 DBPCBLKY         PIC 9(8) COMP-4.
         03 DBPCBNSS         PIC 9(8) COMP-4.
       01 AZ-PCB-PREFIX.
         03 DBPCBLEN         PIC 9(4) COMP-4.
         03 DBPCBPLN         PIC 9(4) COMP-4.
         03 DBPCBNUM         PIC 9(4) COMP-4.
         03 DBPCBFLG         PIC X.
         03 DBPCBFL2         PIC X.
       01 AZ-DBPCB.
         03 DBD-NAME         PIC  X(8).
         03 SEG-LEVEL        PIC  X(2).
         03 DBSTATUS         PIC  X(2).
         03 PROC-OPTIONS     PIC  X(4).
         03 RESERVE-DLI      PIC  X(4).
         03 SEG-NAME-FB      PIC  X(8).
         03 LENGTH-FB-KEY    PIC  9(4).
         03 NUMB-SENS-SEGS   PIC  9(4).
         03 KEY-FB-AREA      PIC  X(17).
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_REPL_FSPIVA65.
           ENTRY 'IMS_INPT_REPL_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043.
           DISPLAY 'AZU0000I IMS_REPL_FSPIVA65 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI REPL (DBPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI REPL (DBPCB:ARG=3)'
             MOVE 5 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * IMS_OUTP_REPL_FSPIVA65.
           ENTRY 'IMS_OUTP_REPL_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043.
           DISPLAY 'AZU0000I IMS_REPL_FSPIVA65 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI REPL (DBPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI REPL (DBPCB:ARG=3)'
             MOVE 5 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I IMS_REPL_FSPIVA65 END.'
           EXIT.
       END PROGRAM 'IMS_REPL_FSPIVA65'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: DLET                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_DLET_FSPIVA65'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM IMS CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN        PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT        PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT           PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE           PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR       POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9) VALUE 0.
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-ARG-COUNT       PIC 9(8) COMP-4.
       01 AZ-ACMDVA          PIC X(4).
       01 AZ-APCBVA          PIC X(44).
       01 FILLER  REDEFINES AZ-APCBVA.
         03 DBPCBDBD         PIC X(8).
         03 DBPCBLEV         PIC X(2).
         03 DBPCBSTC         PIC X(2).
         03 DBPCBPRO         PIC X(4).
         03 DBPCBPFX         USAGE POINTER.
         03 DBPCBSFD         PIC X(8).
         03 DBPCBLKY         PIC 9(8) COMP-4.
         03 DBPCBNSS         PIC 9(8) COMP-4.
       01 AZ-PCB-PREFIX.
         03 DBPCBLEN         PIC 9(4) COMP-4.
         03 DBPCBPLN         PIC 9(4) COMP-4.
         03 DBPCBNUM         PIC 9(4) COMP-4.
         03 DBPCBFLG         PIC X.
         03 DBPCBFL2         PIC X.
       01 AZ-DBPCB.
         03 DBD-NAME         PIC  X(8).
         03 SEG-LEVEL        PIC  X(2).
         03 DBSTATUS         PIC  X(2).
         03 PROC-OPTIONS     PIC  X(4).
         03 RESERVE-DLI      PIC  X(4).
         03 SEG-NAME-FB      PIC  X(8).
         03 LENGTH-FB-KEY    PIC  9(4).
         03 NUMB-SENS-SEGS   PIC  9(4).
         03 KEY-FB-AREA      PIC  X(17).
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** DBPCB : ZUT0000004C
       1 ZUT0000004C.
      *    *** DBD-NAME : ZUT0000004D
         2 ZUT0000004D PIC  X(8).
      *    *** SEG-LEVEL : ZUT0000004E
         2 ZUT0000004E PIC  X(2).
      *    *** DBSTATUS : ZUT0000004F
         2 ZUT0000004F PIC  X(2).
      *    *** PROC-OPTIONS : ZUT00000050
         2 ZUT00000050 PIC  X(4).
      *    *** RESERVE-DLI : ZUT00000051
         2 ZUT00000051 PIC  X(4).
      *    *** SEG-NAME-FB : ZUT00000052
         2 ZUT00000052 PIC  X(8).
      *    *** LENGTH-FB-KEY : ZUT00000053
         2 ZUT00000053 PIC  9(4).
      *    *** NUMB-SENS-SEGS : ZUT00000054
         2 ZUT00000054 PIC  9(4).
      *    *** KEY-FB-AREA : ZUT00000055
         2 ZUT00000055 PIC  X(17).
      *  *** IOAREA : ZUT00000043
       1 ZUT00000043.
      *    *** IO-BLANK : ZUT00000044
         2 ZUT00000044 PIC  X(37).
      *    *** IO-DATA : ZUT00000045
         2 ZUT00000045 REDEFINES ZUT00000044.
      *    *** IO-LAST-NAME : ZUT00000046
         3 ZUT00000046 PIC  X(10).
      *    *** IO-FIRST-NAME : ZUT00000047
         3 ZUT00000047 PIC  X(10).
      *    *** IO-EXTENSION : ZUT00000048
         3 ZUT00000048 PIC  X(10).
      *    *** IO-ZIP-CODE : ZUT00000049
         3 ZUT00000049 PIC  X(7).
      *    *** IO-FILLER : ZUT0000004A
         2 ZUT0000004A PIC  X(3).
      *    *** IO-COMMAND : ZUT0000004B
         2 ZUT0000004B PIC  X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_DLET_FSPIVA65.
           ENTRY 'IMS_INPT_DLET_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043.
           DISPLAY 'AZU0000I IMS_DLET_FSPIVA65 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI DLET (DBPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI DLET (DBPCB:ARG=3)'
             MOVE 6 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * IMS_OUTP_DLET_FSPIVA65.
           ENTRY 'IMS_OUTP_DLET_FSPIVA65' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000043.
           DISPLAY 'AZU0000I IMS_DLET_FSPIVA65 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI DLET (DBPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI DLET (DBPCB:ARG=3)'
             MOVE 6 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(1)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I IMS_DLET_FSPIVA65 END.'
           EXIT.
       END PROGRAM 'IMS_DLET_FSPIVA65'.
