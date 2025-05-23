       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU),NOSEQ
      *+---------------------------------------------------------------+
      *| TDFSIVP3                                                      |
      *| UNIT TEST FOR Z/OS: TEST CASE PROGRAM                         |
      *| TEST CASE VERSION: 202                                        |
      *| DATE GENERATED: 05/14/2025 10:57                              |
      *| ID: f1f0d935-d12c-49ee-8aa0-29257f819169                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: TEST_TEST2                                |
      *|     FOR TEST TEST2                                            |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'DFSIVP34'.
       01 AZ-CSECT       PIC X(72) VALUE SPACES.
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
       01 AZ-SUB-PROGRAM    PIC X(8)  VALUE 'DFSIVP34'.
       01 AZ-SUB-CSECT      PIC X(72) VALUE SPACES.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
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
      *  *** IOPCB : ZUT0000005D
       1 ZUT0000005D.
      *    *** LTERM-NAME : ZUT0000005E
         2 ZUT0000005E PICTURE X(8).
      *    *** FILLER : ZUT0000005F
         2 ZUT0000005F PICTURE X(2).
      *    *** TPSTATUS : ZUT00000060
         2 ZUT00000060 PICTURE XX.
      *    *** FILLER : ZUT00000061
         2 ZUT00000061 PICTURE X(20).
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
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
           DISPLAY 'AZU0000I TEST_TEST2 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET ADDRESS FOR IOPCB
           SET ADDRESS OF ZUT0000005D TO AZ-PCB1
      * SET ADDRESS FOR DBPCB
           SET ADDRESS OF ZUT00000062 TO AZ-PCB2
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR DFSIVP34'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM DFSIVP34 NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 2
                MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
                STRING 'SUB PROGRAM ARGUMENT COUNT DOES NOT MATCH.'
                  DELIMITED BY SIZE
                  INTO MESSAGE-TXT OF BZ-ASSERT
                  WITH POINTER MESSAGE-LEN OF BZ-ASSERT
                END-STRING
                SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
                PERFORM THROW-ASSERTION-M
              END-IF
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL DFSIVP34'
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
           USING BY VALUE AZ-PCB1 AZ-PCB2
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST2 END.'
           GOBACK.
       INITIALIZE-PARM.
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
       END PROGRAM TEST_TEST2.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: BZU_TEST                                  |
      *|     CALLBACK DEFINITION FOR TEST                              |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'DFSIVP34'.
       01 AZ-CSECT       PIC X(72) VALUE SPACES.
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
      *  *** IOPCB : ZUT0000005D
       1 ZUT0000005D.
      *    *** LTERM-NAME : ZUT0000005E
         2 ZUT0000005E PICTURE X(8).
      *    *** FILLER : ZUT0000005F
         2 ZUT0000005F PICTURE X(2).
      *    *** TPSTATUS : ZUT00000060
         2 ZUT00000060 PICTURE XX.
      *    *** FILLER : ZUT00000061
         2 ZUT00000061 PICTURE X(20).
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_DFSIVP34" USING AZ-TEST AZ-INFO-BLOCK
           ZUT0000005D ZUT00000062.
           DISPLAY 'AZU0000I PGM_INPT_DFSIVP34 INPUT VALUES...'.
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
           ENTRY "PGM_OUTP_DFSIVP34" USING AZ-TEST AZ-INFO-BLOCK
           ZUT0000005D ZUT00000062.
           DISPLAY 'AZU0000I PGM_OUTP_DFSIVP34 CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST2'
             PERFORM CHECK-REC-TEST2
             MOVE 4 TO RETURN-CODE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           EXIT.
       CHECK-REC-TEST2.
      * CHECK RECORD COUNT FOR TEST2
      * FOR CBLTDLI GU ''IOPCB''
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
               ' IN CBLTDLI GU ''IOPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI GN ''IOPCB''
           MOVE 2 TO AZ-GRP-INDEX
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
               ' IN CBLTDLI GN ''IOPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI ISRT ''DBPCB''
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
               ' IN CBLTDLI ISRT ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI GU ''DBPCB''
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
               ' IN CBLTDLI GU ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI GHU ''DBPCB''
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
               ' IN CBLTDLI GHU ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI REPL ''DBPCB''
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
               ' IN CBLTDLI REPL ''DBPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI DLET ''DBPCB''
           MOVE 7 TO AZ-GRP-INDEX
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
      * FOR CBLTDLI ISRT ''IOPCB''
           MOVE 8 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           IF AZ-RECORD-COUNT NOT EQUAL 2 THEN
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             MOVE AZ-RECORD-COUNT TO AZ-OUTPUT-COUNT-STR
             STRING
               'EXPECTED RECORD COUNT IS ''2''. '
               'BUT REAL RECORD COUNT IS ''' AZ-OUTPUT-COUNT-STR ''''
               ' IN CBLTDLI ISRT ''IOPCB''.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
      * FOR CBLTDLI ISRT ''IOPCB''
           MOVE 9 TO AZ-GRP-INDEX
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
               ' IN CBLTDLI ISRT ''IOPCB''.'
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
           VALUE 'f1f0d935-d12c-49ee-8aa0-29257f819169'.
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
         03 RECORD-COUNT-IO OCCURS 9.
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
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 9
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
      *|    DLI FUNCTION: GU                                           |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_GU_DFSIVP34'.
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
         03 AZ-RECORD-COUNT-OT OCCURS 2 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 2 PIC 9(5) COMP-5 VALUE 0.
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
      *  *** IOPCB : ZUT0000005D
       1 ZUT0000005D.
      *    *** LTERM-NAME : ZUT0000005E
         2 ZUT0000005E PICTURE X(8).
      *    *** FILLER : ZUT0000005F
         2 ZUT0000005F PICTURE X(2).
      *    *** TPSTATUS : ZUT00000060
         2 ZUT00000060 PICTURE XX.
      *    *** FILLER : ZUT00000061
         2 ZUT00000061 PICTURE X(20).
      *  *** SPA : ZUT0000003F
       1 ZUT0000003F.
      *    *** SPA-LL : ZUT00000040
         2 ZUT00000040 PICTURE X(2).
      *    *** SPA-ZZ : ZUT00000041
         2 ZUT00000041 PICTURE X(4).
      *    *** SPA-TRANCODE : ZUT00000042
         2 ZUT00000042 PICTURE X(8).
      *    *** SPA-CALL : ZUT00000043
         2 ZUT00000043 PICTURE X(2).
      *    *** SPA-COMMAND : ZUT00000044
         2 ZUT00000044 PICTURE X(8).
      *    *** SPA-DATA : ZUT00000045
         2 ZUT00000045.
      *    *** SPA-LAST-NAME : ZUT00000046
         4 ZUT00000046 PIC X(10).
      *    *** SPA-FIRST-NAME : ZUT00000047
         4 ZUT00000047 PIC X(10).
      *    *** SPA-EXTENSION : ZUT00000048
         4 ZUT00000048 PIC X(10).
      *    *** SPA-ZIP-CODE : ZUT00000049
         4 ZUT00000049 PIC X(7).
      *    *** FILLER : ZUT0000004A
         2 ZUT0000004A PICTURE X(19).
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
      *  *** IOAREA : ZUT00000036
       1 ZUT00000036.
      *    *** IO-LINE : ZUT00000037
         2 ZUT00000037 PICTURE X(37).
      *    *** IO-DATA : ZUT00000038
         2 ZUT00000038 REDEFINES ZUT00000037.
      *    *** IO-LAST-NAME : ZUT00000039
         4 ZUT00000039 PIC X(10).
      *    *** IO-FIRST-NAME : ZUT0000003A
         4 ZUT0000003A PIC X(10).
      *    *** IO-EXTENSION : ZUT0000003B
         4 ZUT0000003B PIC X(10).
      *    *** IO-ZIP-CODE : ZUT0000003C
         4 ZUT0000003C PIC X(7).
      *    *** IO-FILLER : ZUT0000003D
         2 ZUT0000003D PIC X(3).
      *    *** IO-COMMAND : ZUT0000003E
         2 ZUT0000003E PIC X(8).
      *  *** SSA : ZUT00000050
       1 ZUT00000050.
      *    *** SEGMENT-NAME : ZUT00000051
         2 ZUT00000051 PIC X(8).
      *    *** SEG-KEY-NAME : ZUT00000052
         2 ZUT00000052 PIC X(11).
      *    *** SSA-KEY : ZUT00000053
         2 ZUT00000053 PIC X(10).
      *    *** FILLER : ZUT00000054
         2 ZUT00000054 PIC X.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_GU_DFSIVP34.
           ENTRY 'IMS_INPT_GU_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT0000003F
           ZUT00000050.
           DISPLAY 'AZU0000I IMS_GU_DFSIVP34 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GU (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (IOPCB:ARG=3)'
             MOVE 1 TO AZ-GRP-INDEX
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
      * CBLTDLI GU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (DBPCB:ARG=4)'
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(2)
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
      * IMS_OUTP_GU_DFSIVP34.
           ENTRY 'IMS_OUTP_GU_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT0000003F
           ZUT00000050.
           DISPLAY 'AZU0000I IMS_GU_DFSIVP34 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GU (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (IOPCB:ARG=3)'
             MOVE 1 TO AZ-GRP-INDEX
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
      * CBLTDLI GU (DBPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 2 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI GU (DBPCB:ARG=4)'
             MOVE 4 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(2)
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
           DISPLAY 'AZU0000I IMS_GU_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_GU_DFSIVP34'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: GN                                           |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_GN_DFSIVP34'.
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
      *  *** IOPCB : ZUT0000005D
       1 ZUT0000005D.
      *    *** LTERM-NAME : ZUT0000005E
         2 ZUT0000005E PICTURE X(8).
      *    *** FILLER : ZUT0000005F
         2 ZUT0000005F PICTURE X(2).
      *    *** TPSTATUS : ZUT00000060
         2 ZUT00000060 PICTURE XX.
      *    *** FILLER : ZUT00000061
         2 ZUT00000061 PICTURE X(20).
      *  *** INPUT-MSG : ZUT0000001D
       1 ZUT0000001D.
      *    *** IN-LL : ZUT0000001E
         2 ZUT0000001E PICTURE S9(3) COMP.
      *    *** IN-ZZ : ZUT0000001F
         2 ZUT0000001F PICTURE S9(3) COMP.
      *    *** IN-FILL : ZUT00000020
         2 ZUT00000020 PICTURE X(4).
      *    *** IN-COMMAND : ZUT00000021
         2 ZUT00000021 PICTURE X(8).
      *    *** TEMP-COMMAND : ZUT00000022
         2 ZUT00000022 REDEFINES ZUT00000021.
      *    *** TEMP-IOCMD : ZUT00000023
         4 ZUT00000023 PIC X(3).
      *    *** TEMP-FILLER : ZUT00000024
         4 ZUT00000024 PIC X(5).
      *    *** IN-LAST-NAME : ZUT00000025
         2 ZUT00000025 PICTURE X(10).
      *    *** IN-FIRST-NAME : ZUT00000026
         2 ZUT00000026 PICTURE X(10).
      *    *** IN-EXTENSION : ZUT00000027
         2 ZUT00000027 PICTURE X(10).
      *    *** IN-ZIP-CODE : ZUT00000028
         2 ZUT00000028 PICTURE X(7).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_GN_DFSIVP34.
           ENTRY 'IMS_INPT_GN_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT0000001D.
           DISPLAY 'AZU0000I IMS_GN_DFSIVP34 CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GN (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI GN (IOPCB:ARG=3)'
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
      * IMS_OUTP_GN_DFSIVP34.
           ENTRY 'IMS_OUTP_GN_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT0000001D.
           DISPLAY 'AZU0000I IMS_GN_DFSIVP34 INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF AZ-DBPCB TO ADDRESS OF AZ-APCBVA.
           SET ADDRESS OF AZ-PCB-PREFIX TO DBPCBPFX.
      * CBLTDLI GN (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI GN (IOPCB:ARG=3)'
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
           DISPLAY 'AZU0000I IMS_GN_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_GN_DFSIVP34'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: ISRT                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_ISRT_DFSIVP34'.
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
         03 AZ-RECORD-COUNT-OT OCCURS 3 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 3 PIC 9(5) COMP-5 VALUE 0.
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
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
      *  *** IOAREA : ZUT00000036
       1 ZUT00000036.
      *    *** IO-LINE : ZUT00000037
         2 ZUT00000037 PICTURE X(37).
      *    *** IO-DATA : ZUT00000038
         2 ZUT00000038 REDEFINES ZUT00000037.
      *    *** IO-LAST-NAME : ZUT00000039
         4 ZUT00000039 PIC X(10).
      *    *** IO-FIRST-NAME : ZUT0000003A
         4 ZUT0000003A PIC X(10).
      *    *** IO-EXTENSION : ZUT0000003B
         4 ZUT0000003B PIC X(10).
      *    *** IO-ZIP-CODE : ZUT0000003C
         4 ZUT0000003C PIC X(7).
      *    *** IO-FILLER : ZUT0000003D
         2 ZUT0000003D PIC X(3).
      *    *** IO-COMMAND : ZUT0000003E
         2 ZUT0000003E PIC X(8).
      *  *** SSA1 : ZUT00000017
       77 ZUT00000017 PICTURE X(9).
      *  *** IOPCB : ZUT0000005D
       1 ZUT0000005D.
      *    *** LTERM-NAME : ZUT0000005E
         2 ZUT0000005E PICTURE X(8).
      *    *** FILLER : ZUT0000005F
         2 ZUT0000005F PICTURE X(2).
      *    *** TPSTATUS : ZUT00000060
         2 ZUT00000060 PICTURE XX.
      *    *** FILLER : ZUT00000061
         2 ZUT00000061 PICTURE X(20).
      *  *** SPA : ZUT0000003F
       1 ZUT0000003F.
      *    *** SPA-LL : ZUT00000040
         2 ZUT00000040 PICTURE X(2).
      *    *** SPA-ZZ : ZUT00000041
         2 ZUT00000041 PICTURE X(4).
      *    *** SPA-TRANCODE : ZUT00000042
         2 ZUT00000042 PICTURE X(8).
      *    *** SPA-CALL : ZUT00000043
         2 ZUT00000043 PICTURE X(2).
      *    *** SPA-COMMAND : ZUT00000044
         2 ZUT00000044 PICTURE X(8).
      *    *** SPA-DATA : ZUT00000045
         2 ZUT00000045.
      *    *** SPA-LAST-NAME : ZUT00000046
         4 ZUT00000046 PIC X(10).
      *    *** SPA-FIRST-NAME : ZUT00000047
         4 ZUT00000047 PIC X(10).
      *    *** SPA-EXTENSION : ZUT00000048
         4 ZUT00000048 PIC X(10).
      *    *** SPA-ZIP-CODE : ZUT00000049
         4 ZUT00000049 PIC X(7).
      *    *** FILLER : ZUT0000004A
         2 ZUT0000004A PICTURE X(19).
      *  *** OUTPUT-AREA : ZUT00000029
       1 ZUT00000029.
      *    *** OUT-LL : ZUT0000002A
         2 ZUT0000002A PICTURE S9(3) COMP.
      *    *** OUT-ZZ : ZUT0000002B
         2 ZUT0000002B PICTURE S9(3) COMP.
      *    *** OUTPUT-LINE : ZUT0000002C
         2 ZUT0000002C PICTURE X(85).
      *    *** OUTPUT-DATA : ZUT0000002D
         2 ZUT0000002D REDEFINES ZUT0000002C.
      *    *** OUT-MESSAGE : ZUT0000002E
         4 ZUT0000002E PIC X(40).
      *    *** OUT-COMMAND : ZUT0000002F
         4 ZUT0000002F PIC X(8).
      *    *** OUT-DATA-TYPE : ZUT00000030
         4 ZUT00000030.
      *    *** OUT-LAST-NAME : ZUT00000031
         6 ZUT00000031 PIC X(10).
      *    *** OUT-FIRST-NAME : ZUT00000032
         6 ZUT00000032 PIC X(10).
      *    *** OUT-EXTENSION : ZUT00000033
         6 ZUT00000033 PIC X(10).
      *    *** OUT-ZIP-CODE : ZUT00000034
         6 ZUT00000034 PIC X(7).
      *    *** OUT-SEGMENT-NO : ZUT00000035
         2 ZUT00000035 PICTURE X(4).
      *  *** MODNAME : ZUT00000018
       77 ZUT00000018 PICTURE X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_ISRT_DFSIVP34.
           ENTRY 'IMS_INPT_ISRT_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036
           ZUT00000017.
           DISPLAY 'AZU0000I IMS_ISRT_DFSIVP34 CHECK VALUES...'
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
      * CBLTDLI ISRT (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (IOPCB:ARG=3)'
             MOVE 8 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(2)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
      * CBLTDLI ISRT (IOPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (IOPCB:ARG=4)'
             MOVE 9 TO AZ-GRP-INDEX
             MOVE 0 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-OT(3)
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
      * IMS_OUTP_ISRT_DFSIVP34.
           ENTRY 'IMS_OUTP_ISRT_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036
           ZUT00000017.
           DISPLAY 'AZU0000I IMS_ISRT_DFSIVP34 INPUT VALUES...'
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
      * CBLTDLI ISRT (IOPCB:ARG=3)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 3 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (IOPCB:ARG=3)'
             MOVE 8 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(2)
             EVALUATE AZ-TEST(1:AZ-TEST-LEN)
               WHEN SPACE
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
      * CBLTDLI ISRT (IOPCB:ARG=4)
           IF DBD-NAME OF AZ-DBPCB NOT = 'DFSAIB' AND
             DBPCBNUM = 1 AND
             AZ-ARG-COUNT = 4 THEN
             DISPLAY 'AZU0000I CBLTDLI ISRT (IOPCB:ARG=4)'
             MOVE 9 TO AZ-GRP-INDEX
             MOVE 1 TO AZ-FLAG-IN
             MOVE RETURN-CODE TO AZ-RC-WORK
             CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
               AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
             SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
             MOVE AZ-RC-WORK TO RETURN-CODE
             ADD 1 TO AZ-WK-RECORD-COUNT
             MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT-IN(3)
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
           DISPLAY 'AZU0000I IMS_ISRT_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_ISRT_DFSIVP34'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: GHU                                          |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_GHU_DFSIVP34'.
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
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
      *  *** IOAREA : ZUT00000036
       1 ZUT00000036.
      *    *** IO-LINE : ZUT00000037
         2 ZUT00000037 PICTURE X(37).
      *    *** IO-DATA : ZUT00000038
         2 ZUT00000038 REDEFINES ZUT00000037.
      *    *** IO-LAST-NAME : ZUT00000039
         4 ZUT00000039 PIC X(10).
      *    *** IO-FIRST-NAME : ZUT0000003A
         4 ZUT0000003A PIC X(10).
      *    *** IO-EXTENSION : ZUT0000003B
         4 ZUT0000003B PIC X(10).
      *    *** IO-ZIP-CODE : ZUT0000003C
         4 ZUT0000003C PIC X(7).
      *    *** IO-FILLER : ZUT0000003D
         2 ZUT0000003D PIC X(3).
      *    *** IO-COMMAND : ZUT0000003E
         2 ZUT0000003E PIC X(8).
      *  *** SSA : ZUT00000050
       1 ZUT00000050.
      *    *** SEGMENT-NAME : ZUT00000051
         2 ZUT00000051 PIC X(8).
      *    *** SEG-KEY-NAME : ZUT00000052
         2 ZUT00000052 PIC X(11).
      *    *** SSA-KEY : ZUT00000053
         2 ZUT00000053 PIC X(10).
      *    *** FILLER : ZUT00000054
         2 ZUT00000054 PIC X.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_GHU_DFSIVP34.
           ENTRY 'IMS_INPT_GHU_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036
           ZUT00000050.
           DISPLAY 'AZU0000I IMS_GHU_DFSIVP34 CHECK VALUES...'
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
      * IMS_OUTP_GHU_DFSIVP34.
           ENTRY 'IMS_OUTP_GHU_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036
           ZUT00000050.
           DISPLAY 'AZU0000I IMS_GHU_DFSIVP34 INPUT VALUES...'
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
           DISPLAY 'AZU0000I IMS_GHU_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_GHU_DFSIVP34'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: REPL                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_REPL_DFSIVP34'.
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
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
      *  *** IOAREA : ZUT00000036
       1 ZUT00000036.
      *    *** IO-LINE : ZUT00000037
         2 ZUT00000037 PICTURE X(37).
      *    *** IO-DATA : ZUT00000038
         2 ZUT00000038 REDEFINES ZUT00000037.
      *    *** IO-LAST-NAME : ZUT00000039
         4 ZUT00000039 PIC X(10).
      *    *** IO-FIRST-NAME : ZUT0000003A
         4 ZUT0000003A PIC X(10).
      *    *** IO-EXTENSION : ZUT0000003B
         4 ZUT0000003B PIC X(10).
      *    *** IO-ZIP-CODE : ZUT0000003C
         4 ZUT0000003C PIC X(7).
      *    *** IO-FILLER : ZUT0000003D
         2 ZUT0000003D PIC X(3).
      *    *** IO-COMMAND : ZUT0000003E
         2 ZUT0000003E PIC X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_REPL_DFSIVP34.
           ENTRY 'IMS_INPT_REPL_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036.
           DISPLAY 'AZU0000I IMS_REPL_DFSIVP34 CHECK VALUES...'
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
      * IMS_OUTP_REPL_DFSIVP34.
           ENTRY 'IMS_OUTP_REPL_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036.
           DISPLAY 'AZU0000I IMS_REPL_DFSIVP34 INPUT VALUES...'
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
           DISPLAY 'AZU0000I IMS_REPL_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_REPL_DFSIVP34'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: DLI                                       |
      *|    DLI FUNCTION: DLET                                         |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'IMS_DLET_DFSIVP34'.
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
      *  *** DBPCB : ZUT00000062
       1 ZUT00000062.
      *    *** DBNAME : ZUT00000063
         2 ZUT00000063 PICTURE X(8).
      *    *** SEG-LEVEL-NO : ZUT00000064
         2 ZUT00000064 PICTURE X(2).
      *    *** DBSTATUS : ZUT00000065
         2 ZUT00000065 PICTURE XX.
      *    *** FILLER : ZUT00000066
         2 ZUT00000066 PICTURE X(20).
      *  *** IOAREA : ZUT00000036
       1 ZUT00000036.
      *    *** IO-LINE : ZUT00000037
         2 ZUT00000037 PICTURE X(37).
      *    *** IO-DATA : ZUT00000038
         2 ZUT00000038 REDEFINES ZUT00000037.
      *    *** IO-LAST-NAME : ZUT00000039
         4 ZUT00000039 PIC X(10).
      *    *** IO-FIRST-NAME : ZUT0000003A
         4 ZUT0000003A PIC X(10).
      *    *** IO-EXTENSION : ZUT0000003B
         4 ZUT0000003B PIC X(10).
      *    *** IO-ZIP-CODE : ZUT0000003C
         4 ZUT0000003C PIC X(7).
      *    *** IO-FILLER : ZUT0000003D
         2 ZUT0000003D PIC X(3).
      *    *** IO-COMMAND : ZUT0000003E
         2 ZUT0000003E PIC X(8).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * IMS_INPT_DLET_DFSIVP34.
           ENTRY 'IMS_INPT_DLET_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036.
           DISPLAY 'AZU0000I IMS_DLET_DFSIVP34 CHECK VALUES...'
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
             MOVE 7 TO AZ-GRP-INDEX
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
      * IMS_OUTP_DLET_DFSIVP34.
           ENTRY 'IMS_OUTP_DLET_DFSIVP34' USING AZ-TEST
           AZ-INFO-BLOCK AZ-ARG-COUNT AZ-ACMDVA AZ-APCBVA ZUT00000036.
           DISPLAY 'AZU0000I IMS_DLET_DFSIVP34 INPUT VALUES...'
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
             MOVE 7 TO AZ-GRP-INDEX
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
           DISPLAY 'AZU0000I IMS_DLET_DFSIVP34 END.'
           EXIT.
       END PROGRAM 'IMS_DLET_DFSIVP34'.
