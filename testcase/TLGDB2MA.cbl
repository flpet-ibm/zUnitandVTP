       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU),NOSEQ
      *+---------------------------------------------------------------+
      *| TLGDB2MA                                                      |
      *| UNIT TEST FOR Z/OS: TEST CASE PROGRAM                         |
      *| TEST CASE VERSION: 202                                        |
      *| DATE GENERATED: 06/20/2025 14:44                              |
      *| ID: c938bcaa-70ff-490e-a58f-42178c447326                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: TEST_TEST1                                |
      *|     FOR TEST TEST1                                            |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'LGDB2MAI'.
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
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-ARG-LIST.
         03 ARG-LENGTH PIC 9(4) COMP-4.
         03 ARG-DATA PIC X(256).
       01 AZ-INFO-BLOCK.
         COPY EQAITERC.
       01 AZ-PROC-PTR       USAGE IS PROCEDURE-POINTER.
       PROCEDURE DIVISION USING AZ-TEST AZ-ARG-LIST AZ-INFO-BLOCK.
      * START
           DISPLAY 'AZU0000I TEST_TEST1 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
      * SET AREA ADDRESS TO POINTER
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL LGDB2MAI'
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
           .
      * EVALUATE OUTPUT VALUE
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
      *| UNIT TEST FOR Z/OS: BZU_TEST                                  |
      *|     CALLBACK DEFINITION FOR TEST                              |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'LGDB2MAI'.
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
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_LGDB2MAI" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_INPT_LGDB2MAI INPUT VALUES...'.
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
           ENTRY "PGM_OUTP_LGDB2MAI" USING AZ-TEST AZ-INFO-BLOCK
           .
           DISPLAY 'AZU0000I PGM_OUTP_LGDB2MAI CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM P-OUTPUT-TEST1
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
           GOBACK.
       TEARDOWN.
           DISPLAY 'AZU0000I BZU_TEST END.'
           EXIT.
       P-OUTPUT-TEST1.
           MOVE 0 TO RETURN-CODE
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
           VALUE 'c938bcaa-70ff-490e-a58f-42178c447326'.
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
      *| UNIT TEST FOR Z/OS: LGDB2AGE                                  |
      *|     SUB PROGRAM CALLED FROM PROGRAM UNDER TEST                |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_LGDB2AGE'.
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
          COPY EQAITERC.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       1 AZU00000000.
      *    *** DATEOFBIRTH : ZUT0000001B
         10 ZUT0000001B PIC X(10).
      *  *** AGE : ZUT00000023
       1 ZUT00000023 PIC S9(5)V99.
      *
       PROCEDURE DIVISION.
       ENTRY_INPT.
      * ENTRY FOR CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_LGDB2AGE" USING
              AZ-TEST AZ-INFO-BLOCK
           AZU00000000 ZUT00000023.
           DISPLAY 'AZU0000I PGM_INPT_LGDB2AGE CHECK VALUES...'.
           PERFORM PROC_INPT.
           GOBACK.
      * ENTRY FOR CHECK OUTPUT VALUE WITH CSECT
           ENTRY "PGM_INPT_LGDB2MAI_LGDB2AGE" USING
              AZ-TEST AZ-INFO-BLOCK
           AZU00000000 ZUT00000023.
           DISPLAY
           'AZU0000I PGM_INPT_LGDB2MAI_LGDB2AGE CHECK VALUES...' .
           PERFORM PROC_INPT.
           GOBACK.
       ENTRY_OUTP.
      * ENTRY FOR SET INPUT VALUE
           ENTRY "PGM_OUTP_LGDB2AGE" USING
              AZ-TEST AZ-INFO-BLOCK
           AZU00000000 ZUT00000023.
           DISPLAY 'AZU0000I PGM_OUTP_LGDB2AGE INPUT VALUES...'.
           PERFORM PROC_OUTP.
           GOBACK.
      * ENTRY FOR SET INPUT VALUE WITH CSECT
           ENTRY "PGM_OUTP_LGDB2MAI_LGDB2AGE" USING
              AZ-TEST AZ-INFO-BLOCK
           AZU00000000 ZUT00000023.
           DISPLAY
           'AZU0000I PGM_OUTP_LGDB2MAI_LGDB2AGE INPUT VALUES...' .
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
           DISPLAY 'AZU0000I PGM_LGDB2AGE END.'
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
       END PROGRAM 'PGM_LGDB2AGE'.
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
         03 RECORD-COUNT-IO OCCURS 4.
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
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 4
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
      *| UNIT TEST FOR Z/OS: AZU_GENERIC_DB2                           |
      *|   GENERIC DB2 CALLBACK EXIT POINT                             |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'AZU_GENERIC_DB2'.
       DATA DIVISION.
       LINKAGE SECTION.
       01 AZ-TEST                   PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT.
           ENTRY 'DB2_INPT' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I DB2_INPT ...'
           MOVE 0 TO RETURN-CODE.
           GOBACK.
      * DB2_OUTP.
           ENTRY 'DB2_OUTP' USING AZ-TEST
                                  AZ-INFO-BLOCK.
           DISPLAY 'AZU0000I DB2_OUTP ...'
           MOVE 0 TO RETURN-CODE.
           GOBACK.
       END PROGRAM 'AZU_GENERIC_DB2'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: EXEC SQL OPEN                             |
      *|    FUNCTION CODE: 0003                                        |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0003_LGDB2MAI'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
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
       01 AZ-APLIST.
          COPY EQADB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-SQLDA.
          COPY EQADB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0003_LGDB2MAI.
           ENTRY 'DB2_INPT_0003_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0003_LGDB2MAI CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL OPEN : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL OPEN'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
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
      * DB2_OUTP_0003_LGDB2MAI.
           ENTRY 'DB2_OUTP_0003_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0003_LGDB2MAI INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL OPEN : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL OPEN'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
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
           DISPLAY 'AZU0000I DB2_0003_LGDB2MAI END.'
           EXIT.
       END PROGRAM 'DB2_0003_LGDB2MAI'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: EXEC SQL FETCH                            |
      *|    FUNCTION CODE: 0004                                        |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0004_LGDB2MAI'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
       01 AZ-GRP-INDEX       PIC 9(8).
       01 AZ-FLAG-IN         PIC 9(1).
       01 AZ-RECORD-PTR      POINTER.
       01 AZ-RC-WORK         PIC S9(4) USAGE BINARY.
       1 AZ-TEST-INPUT-DATA-VALUE.
          3 AZU00000000.
            5 PIC X(4) DISPLAY VALUE 'John'.
            5 PIC X(6) DISPLAY VALUE SPACES.
          3 AZU00000001.
            5 PIC X(10) DISPLAY VALUE '1990-01-01'.
       LOCAL-STORAGE SECTION.
       01 AZ-HOSTVAR-PTR     POINTER.
       01 AZ-HOSTVAR-PTR-ADDR
           REDEFINES AZ-HOSTVAR-PTR PIC 9(9) COMP-5.
       LINKAGE SECTION.
       01 AZ-TEST            PIC X(80).
       01 AZ-INFO-BLOCK.
          COPY EQAITERC.
       01 AZ-APLIST.
          COPY EQADB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 ARGI1          .
          COPY EQADB2CV.
       01 ARGI2          .
          COPY EQADB2CV.
       01 ARGI3          .
          COPY EQADB2CV.
       01 ARGI4          .
          COPY EQADB2CV.
       01 ARGI5          .
          COPY EQADB2CV.
       01 ARGI6          .
          COPY EQADB2CV.
       01 ARGI7          .
          COPY EQADB2CV.
       01 ARGI8          .
          COPY EQADB2CV.
       01 ARGI9          .
          COPY EQADB2CV.
       01 ARGI10         .
          COPY EQADB2CV.
       1 AZU00000002.
      *    *** CUSTOMERNUMBER : ZUT00000018
         10 ZUT00000018 PIC S9(9) USAGE COMP-5.
       1 AZU00000003.
      *    *** FIRSTNAME : ZUT00000019
         10 ZUT00000019 PIC X(10).
       1 AZU00000004.
      *    *** LASTNAME : ZUT0000001A
         10 ZUT0000001A PIC X(20).
       1 AZU00000005.
      *    *** DATEOFBIRTH : ZUT0000001B
         10 ZUT0000001B PIC X(10).
       1 AZU00000006.
      *    *** HOUSENAME : ZUT0000001C
         10 ZUT0000001C PIC X(20).
       1 AZU00000007.
      *    *** HOUSENUMBER : ZUT0000001D
         10 ZUT0000001D PIC X(4).
       1 AZU00000008.
      *    *** POSTCODE : ZUT0000001E
         10 ZUT0000001E PIC X(8).
       1 AZU00000009.
      *    *** PHONEHOME : ZUT0000001F
         10 ZUT0000001F PIC X(20).
       1 AZU0000000A.
      *    *** PHONEMOBILE : ZUT00000020
         10 ZUT00000020 PIC X(20).
       1 AZU0000000B.
      *    *** EMAILADDRESS : ZUT00000021
         10 ZUT00000021 PIC X(100).
      *  *** SQLCA : ZUT00000000
       1 ZUT00000000.
      *    *** SQLCAID : ZUT00000001
         5 ZUT00000001 PIC X(8).
      *    *** SQLCABC : ZUT00000002
         5 ZUT00000002 PIC S9(9) COMP-5.
      *    *** SQLCODE : ZUT00000003
         5 ZUT00000003 PIC S9(9) COMP-5.
      *    *** SQLERRM : ZUT00000004
         5 ZUT00000004.
      *    *** SQLERRML : ZUT00000005
         49 ZUT00000005 PIC S9(4) COMP-5.
      *    *** SQLERRMC : ZUT00000006
         49 ZUT00000006 PIC X(70).
      *    *** SQLERRP : ZUT00000007
         5 ZUT00000007 PIC X(8).
      *    *** SQLERRD : ZUT00000008
         5 ZUT00000008 OCCURS 6 TIMES PIC S9(9) COMP-5.
      *    *** SQLWARN : ZUT00000009
         5 ZUT00000009.
      *    *** SQLWARN0 : ZUT0000000A
         10 ZUT0000000A PIC X.
      *    *** SQLWARN1 : ZUT0000000B
         10 ZUT0000000B PIC X.
      *    *** SQLWARN2 : ZUT0000000C
         10 ZUT0000000C PIC X.
      *    *** SQLWARN3 : ZUT0000000D
         10 ZUT0000000D PIC X.
      *    *** SQLWARN4 : ZUT0000000E
         10 ZUT0000000E PIC X.
      *    *** SQLWARN5 : ZUT0000000F
         10 ZUT0000000F PIC X.
      *    *** SQLWARN6 : ZUT00000010
         10 ZUT00000010 PIC X.
      *    *** SQLWARN7 : ZUT00000011
         10 ZUT00000011 PIC X.
      *    *** SQLEXT : ZUT00000012
         5 ZUT00000012.
      *    *** SQLWARN8 : ZUT00000013
         10 ZUT00000013 PIC X.
      *    *** SQLWARN9 : ZUT00000014
         10 ZUT00000014 PIC X.
      *    *** SQLWARNA : ZUT00000015
         10 ZUT00000015 PIC X.
      *    *** SQLSTATE : ZUT00000016
         10 ZUT00000016 PIC X(5).
       01 AZ-SQLDA.
          COPY EQADB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0004_LGDB2MAI.
           ENTRY 'DB2_INPT_0004_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0004_LGDB2MAI CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           SET ADDRESS OF ZUT00000000 TO SQL-CODEPTR OF AZ-APLIST
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL FETCH : OUT=0 IN=10
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 10 THEN
             DISPLAY 'AZU0000I EXEC SQL FETCH'
              ' : OUT=' 0 ' IN=' 10
              ' L=' AZ-STMT-NUM
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
               WHEN 'TEST1'
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
      * SET INPUT VALUE
      * DB2_OUTP_0004_LGDB2MAI.
           ENTRY 'DB2_OUTP_0004_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST ARGI1 ARGI2 ARGI3 ARGI4 ARGI5 ARGI6
           ARGI7 ARGI8 ARGI9 ARGI10.
           DISPLAY 'AZU0000I DB2_0004_LGDB2MAI INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           SET ADDRESS OF ZUT00000000 TO SQL-CODEPTR OF AZ-APLIST
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL FETCH : OUT=0 IN=10
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 10 THEN
             DISPLAY 'AZU0000I EXEC SQL FETCH'
              ' : OUT=' 0 ' IN=' 10
              ' L=' AZ-STMT-NUM
             MOVE SQL-AVAR-ADDR OF ARGI1 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000002
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI2 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000003
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI3 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000004
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI4 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000005
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI5 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000006
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI6 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000007
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI7 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000008
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI8 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU00000009
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI9 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU0000000A
             TO AZ-HOSTVAR-PTR
             MOVE SQL-AVAR-ADDR OF ARGI10 TO AZ-HOSTVAR-PTR-ADDR
             SET ADDRESS OF AZU0000000B
             TO AZ-HOSTVAR-PTR
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
               WHEN 'TEST1'
                 PERFORM I00040-TEST1
                 CONTINUE
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-IF.
           PERFORM TEARDOWN.
           GOBACK.
       I00040-TEST1.
           IF AZ-RECORD-COUNT-IN(1) = 0 THEN
             CONTINUE
           ELSE IF AZ-RECORD-COUNT-IN(1) = 1
           MOVE 1 TO ZUT00000018 OF AZU00000002
           MOVE AZU00000000 TO ZUT00000019 OF AZU00000003
           MOVE AZU00000001 TO ZUT0000001B OF AZU00000005
           ELSE IF AZ-RECORD-COUNT-IN(1) = 2
           MOVE 100 TO ZUT00000003 OF ZUT00000000
           ELSE
             CONTINUE
           END-IF
           END-IF
           END-IF.
       TEARDOWN.
           DISPLAY 'AZU0000I DB2_0004_LGDB2MAI END.'
           EXIT.
       END PROGRAM 'DB2_0004_LGDB2MAI'.
      *+---------------------------------------------------------------+
      *| UNIT TEST FOR Z/OS: EXEC SQL CLOSE                            |
      *|    FUNCTION CODE: 0005                                        |
      *| TEST CASE VERSION: 202                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'DB2_0005_LGDB2MAI'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BZ-ASSERT.
         03 MESSAGE-LEN PIC S9(4) COMP-4 VALUE 24.
         03 MESSAGE-TXT PIC X(254) VALUE 'HELLO FROM DB2 CALLBACK'.
       01  BZ-P1 PIC S9(9) COMP-4 VALUE 4.
       01  BZ-P2 PIC S9(9) COMP-4 VALUE 2001.
       01  BZ-P3 PIC X(3) VALUE 'AZU'.
       01 BZ-TRACE.
         03 TRACE-LEN       PIC S9(4) COMP-4 VALUE 5.
         03 TRACE-TXT       PIC X(254) VALUE 'TRACE'.
       01 BZUASSRT          PIC X(8) VALUE 'BZUASSRT'.
       01 BZUTRACE          PIC X(8) VALUE 'BZUTRACE'.
       01 AZ-TRACE-PTR      POINTER.
       01 AZ-TEST-LEN        PIC S9(8) COMP.
       01 AZ-RECORD.
         03 AZ-RECORD-COUNT-OT OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-RECORD-COUNT-IN OCCURS 1 PIC 9(5) COMP-5 VALUE 0.
         03 AZ-OUT-PARM-NUM  PIC 9(8).
         03 AZ-IN-PARM-NUM   PIC 9(8).
         03 AZ-STMT-NUM      PIC 9(9).
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
       01 AZ-APLIST.
          COPY EQADB2CP.
       01 AZ-WK-RECORD-COUNT PIC 9(5) COMP-5.
       01 AZ-SQLDA.
          COPY EQADB2CA.
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
      * DB2_INPT_0005_LGDB2MAI.
           ENTRY 'DB2_INPT_0005_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0005_LGDB2MAI CHECK VALUES...'
           MOVE 4 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL CLOSE : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL CLOSE'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
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
      * DB2_OUTP_0005_LGDB2MAI.
           ENTRY 'DB2_OUTP_0005_LGDB2MAI' USING AZ-TEST
           AZ-INFO-BLOCK AZ-APLIST .
           DISPLAY 'AZU0000I DB2_0005_LGDB2MAI INPUT VALUES...'
           MOVE 0 TO RETURN-CODE.
           MOVE SQL-STMT-NUM OF AZ-APLIST TO AZ-STMT-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-VPARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-OUT-PARM-NUM
           SET ADDRESS OF AZ-SQLDA TO SQL-APARMPTR
           MOVE SQLDA-NUM OF AZ-SQLDA TO AZ-IN-PARM-NUM
           MOVE 0 TO AZ-TEST-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * EXEC SQL CLOSE : OUT=0 IN=0
           IF AZ-OUT-PARM-NUM = 0 AND
              AZ-IN-PARM-NUM = 0 THEN
             DISPLAY 'AZU0000I EXEC SQL CLOSE'
              ' : OUT=' 0 ' IN=' 0
              ' L=' AZ-STMT-NUM
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
           DISPLAY 'AZU0000I DB2_0005_LGDB2MAI END.'
           EXIT.
       END PROGRAM 'DB2_0005_LGDB2MAI'.
