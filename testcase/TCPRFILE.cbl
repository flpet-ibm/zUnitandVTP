       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| TCPRFILE                                                      |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| TEST CASE VERSION: 103                                        |
      *| DATE GENERATED: 02/06/2024 12:33                              |
      *| ID: 2f24dbed-c866-46bf-8a3a-a29b7ab9d798                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST1                                              |
      *|     THIS PROGRAM IS FOR TEST TEST1                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AMOUNTIN
           ASSIGN TO AMOUNTIN
           FILE STATUS IS AZ-FS-CODE
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD AMOUNTIN
           RECORDING MODE F.
      *  *** AMOUNT-RECORD : ZUT00000003
       1 ZUT00000003.
      *    *** AMOUNT1 : ZUT00000004
         5 ZUT00000004 PIC ZZZ.ZZZ.ZZ9,99.
      *    *** FILLER : ZUT00000005
         5 ZUT00000005 PIC X(66).
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'CPRFILE'.
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
       01 AZ-SUB-PROGRAM    PIC X(8)  VALUE 'CPRFILE'.
       01 AZ-SUB-CSECT      PIC X(72) VALUE SPACES.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       01 AZ-FS-CODE        PIC X(2).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY BZUITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       01  AZ-SUB-PGM-LIST.
         03  AZ-SUB-PGM-COUNT       PIC 9(4) COMP-4.
         03  AZ-SUB-PGM-ADDRS  OCCURS 1 TO 100 TIMES
                       DEPENDING ON AZ-SUB-PGM-COUNT.
           05  AZ-SUB-PGM-ADDR      USAGE POINTER.
           05  AZ-SUB-PGM-LGTH      PIC 9(8) COMP-4.
       01  AZ-LINKPARM1             PIC X(32768).
       01  AZ-LINKPARM2             PIC X(32768).
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR CPRFILE'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM CPRFILE NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 0
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
           DISPLAY 'AZU0000I CALL CPRFILE'
           CALL PROGRAM-NAME
           .
      * CLOSE AMOUNTIN
           CLOSE AMOUNTIN
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
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
      *| ZUNIT BZU_TEST                                                |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'CPRFILE'.
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
          COPY BZUITERC.
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_CPRFILE" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_INPT_CPRFILE INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * EVALUATE OUTPUT VALUE
           ENTRY "PGM_OUTP_CPRFILE" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_OUTP_CPRFILE CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
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
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           GOBACK.
       CHECK-REC-TEST1.
      * CHECK RECORD COUNT FOR TEST1
      * FOR CPRCHECK
           MOVE 3 TO AZ-GRP-INDEX
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
               ' IN CPRCHECK.'
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
      *| ZUNIT BZU_INIT                                                |
      *|     INITIAL PROCEDURE                                         |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_INIT'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 AZ-TESTCASE-ID        PIC X(36)
           VALUE '2f24dbed-c866-46bf-8a3a-a29b7ab9d798'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-TEST-ID            PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-TEST-ID
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_INIT: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           DISPLAY 'AZU0000I TEST CASE VERSION: 103'
           DISPLAY 'AZU0001I FOR TEST RUNNER: <no_value> (NOT FOR:CICSCO
      -    'UNT,DLL,CSECT)'
           MOVE AZ-TESTCASE-ID TO AZ-TEST-ID
           GOBACK.
       END PROGRAM BZU_INIT.
      *+---------------------------------------------------------------+
      *| ZUNIT BZU_TERM                                                |
      *|     TERMINATION PROCEDURE                                     |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TERM'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AZ-TEST-NAME-LEN      PIC S9(9) COMP-5.
       01 BZUASSRT      PIC X(8) VALUE 'BZUASSRT'.
       01 BZ-P1         PIC S9(9) COMP-4 VALUE 4.
       01 BZ-P2         PIC S9(9) COMP-4 VALUE 2001.
       01 BZ-P3         PIC X(3) VALUE 'AZU'.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM TEST CALLBACK'.
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * DISPLAY WARNING FOR STOP RUN
           MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
           STRING
             '''STOP RUN'' IS NOT SUPPORTED. '
             'TEST RESULTS MAY BE INCORRECT.'
             DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
             WITH POINTER MESSAGE-LEN OF BZ-ASSERT
           END-STRING
           SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
           PERFORM THROW-ASSERTION-M.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
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
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR CPRCHECK                                    |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_CPRCHECK'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
          COPY BZUITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
      *  *** WS-CPR : ZUT0000000A
       1 ZUT0000000A PIC X(10).
      *  *** WS-AGE : ZUT0000000B
       1 ZUT0000000B PIC S9(4) BINARY.
      *  *** WS-GENDER : ZUT0000000E
       1 ZUT0000000E PIC X.
      *  *** WS-RC : ZUT00000010
       1 ZUT00000010 PIC X.
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_CPRFILE_CPRCHECK" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A ZUT0000000B ZUT0000000E ZUT00000010.
           DISPLAY 'AZU0000I PGM_INPT_CPRFILE_CPRCHECK CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-OT
           MOVE 3 TO AZ-GRP-INDEX
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
      * SET INPUT VALUE
           ENTRY "PGM_OUTP_CPRFILE_CPRCHECK" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT0000000A ZUT0000000B ZUT0000000E ZUT00000010.
           DISPLAY 'AZU0000I PGM_OUTP_CPRFILE_CPRCHECK INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * SET AREA ADDRESS TO POINTER
           MOVE ITER OF AZ-INFO-BLOCK TO AZ-RECORD-COUNT-IN
           MOVE 3 TO AZ-GRP-INDEX
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
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_CPRCHECK END.'
           GOBACK.
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
       END PROGRAM 'PGM_CPRCHECK'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR FILE                                        |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'FILE_CPRFILE'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST              PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-INFO-DDNM         PIC X(8).
       01 AZ-INFO-STAT         PIC X(1).
       01 AZ-INFO-COND         PIC X(1).
       01 AZ-ACMDVA            PIC X(4).
       01 AZ-PARM              PIC X(80).
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * SET INPUT VALUE
      * QSAM_OUTP_READ_CPRFILE.
           ENTRY 'QSAM_OUTP_READ_CPRFILE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM.
           DISPLAY 'AZU0000I QSAM_OUTP_READ_CPRFILE INPUT VALUES...'
           IF AZ-INFO-STAT = X'00' THEN
              PERFORM READ-INPUT
           END-IF.
           PERFORM TEARDOWN.
      * END
       READ-OUTPUT.
           EXIT.
       READ-INPUT.
      *    FILE "FILEIN" (DD:FILEIN)"
           IF AZ-INFO-DDNM = "FILEIN" THEN
             CALL 'FILEIN_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           END-IF.
      *    FILE "AMOUNTIN" (DD:AMOUNTIN)"
           IF AZ-INFO-DDNM = "AMOUNTIN" THEN
             CALL 'AMOUNTIN_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           END-IF.
           EXIT.
       WRITE-OUTPUT.
           EXIT.
       WRITE-INPUT.
           EXIT.
       TEARDOWN.
           DISPLAY 'AZU0000I FILE_CPRFILE END.'
           GOBACK.
       END PROGRAM 'FILE_CPRFILE'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR FILE QSAM                                   |
      *|   FILE NAME: FILEIN                                           |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'QSAM_FILEIN_CPRFILE'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
         03 AZ-RECORD-COUNT PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-INFO-DDNM        PIC X(8).
       01 AZ-INFO-STAT        PIC X(1).
       01 AZ-INFO-COND        PIC X(1).
       01 AZ-ACMDVA           PIC X(4).
       01 AZ-PARM             PIC X(10).
       01 AZ-WK-RECORD-COUNT  PIC 9(5) COMP-5.
      *  *** IN-RECORD : ZUT00000000
       1 ZUT00000000.
      *    *** IN-FDATO : ZUT00000001
         3 ZUT00000001 PIC X(6).
      *    *** IN-CHECKDIGIT : ZUT00000002
         3 ZUT00000002 PIC X(4).
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY 'FILEIN_OUTPUT_WRITE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE FILEIN CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY 'FILEIN_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE FILEIN INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000000 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 1 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
      *     DISPLAY 'AZU0000I QSAM_FILEIN_CPRFILE END.'
           GOBACK.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'QSAM_FILEIN_CPRFILE'.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR FILE QSAM                                   |
      *|   FILE NAME: AMOUNTIN                                         |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'QSAM_AMOUNTIN_CPRFILE'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
         03 AZ-RECORD-COUNT PIC 9(5) COMP-5 VALUE 0.
       01 AZ-GRP-INDEX      PIC 9(8).
       01 AZ-FLAG-IN        PIC 9(1).
       01 AZ-RECORD-PTR     POINTER.
       01 AZ-RC-WORK        PIC S9(4) USAGE BINARY.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       01 AZ-INFO-DDNM        PIC X(8).
       01 AZ-INFO-STAT        PIC X(1).
       01 AZ-INFO-COND        PIC X(1).
       01 AZ-ACMDVA           PIC X(4).
       01 AZ-PARM             PIC X(80).
       01 AZ-WK-RECORD-COUNT  PIC 9(5) COMP-5.
      *  *** AMOUNT-RECORD : ZUT00000003
       1 ZUT00000003.
      *    *** AMOUNT1 : ZUT00000004
         5 ZUT00000004 PIC ZZZ.ZZZ.ZZ9,99.
      *    *** FILLER : ZUT00000005
         5 ZUT00000005 PIC X(66).
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY 'AMOUNTIN_OUTPUT_WRITE' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE AMOUNTIN CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000003 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY 'AMOUNTIN_INPUT_READ' USING AZ-TEST
             AZ-INFO-BLOCK AZ-INFO-DDNM AZ-INFO-STAT AZ-INFO-COND
             AZ-ACMDVA AZ-PARM
           DISPLAY 'AZU0000I FILE AMOUNTIN INPUT VALUES...'.
           MOVE 0 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           SET ADDRESS OF ZUT00000003 TO ADDRESS OF AZ-PARM
      * SET AREA ADDRESS TO POINTER
           MOVE 2 TO AZ-GRP-INDEX
           MOVE 0 TO AZ-FLAG-IN
           MOVE RETURN-CODE TO AZ-RC-WORK
           CALL 'GTMEMRC' USING TC-WORK-AREA OF AZ-INFO-BLOCK
             AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR
           SET ADDRESS OF AZ-WK-RECORD-COUNT TO AZ-RECORD-PTR
           MOVE AZ-RC-WORK TO RETURN-CODE
           ADD 1 TO AZ-WK-RECORD-COUNT
           MOVE AZ-WK-RECORD-COUNT TO AZ-RECORD-COUNT
           EVALUATE AZ-TEST(1:AZ-TEST-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-INPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
      *     DISPLAY 'AZU0000I QSAM_AMOUNTIN_CPRFILE END.'
           GOBACK.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-INPUT-TEST1.
           IF AZ-RECORD-COUNT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'QSAM_AMOUNTIN_CPRFILE'.
      *+---------------------------------------------------------------+
      *| ZUNIT GTMEMRC                                                 |
      *|     GET DATA AREA FOR RECORD COUNT OF SUBSYSTEM GROUP         |
      *| TEST CASE VERSION: 103                                        |
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
         03 RECORD-COUNT-IO OCCURS 3.
           05 RECORD-COUNT-OT PIC 9(5) COMP-5.
           05 RECORD-COUNT-IN PIC 9(5) COMP-5.
       01 WK-RECORD-COUNT     PIC 9(5) COMP-5.
       01 AZ-TEST             PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN
           AZ-RECORD-PTR.
       MAINPROC SECTION.
      * GET DATA AREA
           SET ADDRESS OF DATA-PTR TO ADDRESS OF AZ-TC-WORK-AREA.
           IF DATA-PTR-VALUE = 0 THEN
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 3
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
       CB-ENTRY.
      * ENTRY FOR CALLBACK
           ENTRY "PGM_INPT_GTMEMRC" USING AZ-TEST AZ-INFO-BLOCK
             AZ-TC-WORK-AREA AZ-GRP-INDEX AZ-FLAG-IN AZ-RECORD-PTR.
           PERFORM MAINPROC.
           EXIT.
       END PROGRAM 'GTMEMRC'.
      *+---------------------------------------------------------------+
      *| ZUNIT AZU_GENERIC_FILE                                        |
      *|   GENERIC FILE CALLBACK EXIT POINT                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_FILE'.
       PROCEDURE DIVISION.
      * QSAM_INPT.
           ENTRY 'QSAM_INPT'.
           DISPLAY 'AZU0000I QSAM_INPT ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
      * QSAM_OUTP.
           ENTRY 'QSAM_OUTP'.
           DISPLAY 'AZU0000I QSAM_OUTP ...'
           MOVE 4 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_FILE'.
