       PROCESS NODLL,NODYNAM,TEST(NOSEP),NOCICS,NOSQL,PGMN(LU)
      *+---------------------------------------------------------------+
      *| TCPRCHEC                                                      |
      *| PRODUCT: IBM DEVELOPER FOR Z/OS                               |
      *| COMPONENT: IBM Z/OS AUTOMATED UNIT TESTING FRAMEWORK (ZUNIT)  |
      *|   FOR ENTERPRISE COBOL AND PL/I                               |
      *| PROGRAM: ENTERPRISE COBOL ZUNIT TEST CASE FOR DYNAMIC RUNNER  |
      *| TEST CASE VERSION: 103                                        |
      *| DATE GENERATED: 02/13/2024 15:26                              |
      *| ID: ccf4c002-9892-40b8-88cf-d51be1e755ea                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| ZUNIT TEST_TEST1                                              |
      *|     THIS PROGRAM IS FOR TEST TEST1                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST1'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'CPRCHECK'.
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
       01 AZ-SUB-CSECT      PIC X(72) VALUE 'CPRCHECK'.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
      *  *** LS-CPR-NR : ZUT0000002C
       1 ZUT0000002C PIC X(10).
      *  *** LS-AGE : ZUT0000002D
       1 ZUT0000002D PIC S9(4) BINARY.
      *  *** LS-GENDER : ZUT0000002E
       1 ZUT0000002E PIC X.
      *  *** RC : ZUT0000002F
       1 ZUT0000002F PIC X.
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
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR CPRCHECK'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM CPRCHECK NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 4
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
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002C
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(1)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(1))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(2)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002D
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(2)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(2))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(3)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002E
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(3)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(3))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(4)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002F
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(4)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(4))
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL CPRCHECK'
           CALL BZUGETEP USING BY REFERENCE AZ-SUB-PROGRAM AZ-SUB-CSECT
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
           USING ZUT0000002C ZUT0000002D ZUT0000002E ZUT0000002F
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST1 END.'
           GOBACK.
       INITIALIZE-PARM.
           INITIALIZE ZUT0000002C
           INITIALIZE ZUT0000002D
           INITIALIZE ZUT0000002E
           INITIALIZE ZUT0000002F
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
      *| ZUNIT TEST_TEST2                                              |
      *|     THIS PROGRAM IS FOR TEST TEST2                            |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST_TEST2'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'CPRCHECK'.
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
       01 AZ-SUB-CSECT      PIC X(72) VALUE 'CPRCHECK'.
       01 AZ-SUB-ARG-LIST   USAGE POINTER.
       LOCAL-STORAGE SECTION.
      *  *** LS-CPR-NR : ZUT0000002C
       1 ZUT0000002C PIC X(10).
      *  *** LS-AGE : ZUT0000002D
       1 ZUT0000002D PIC S9(4) BINARY.
      *  *** LS-GENDER : ZUT0000002E
       1 ZUT0000002E PIC X.
      *  *** RC : ZUT0000002F
       1 ZUT0000002F PIC X.
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
           DISPLAY 'AZU0000I TEST_TEST2 STARTED...'
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
      * INITIALIZE PARAMETER
           PERFORM INITIALIZE-PARM
      * SET AREA ADDRESS TO POINTER
      * GET SUB PROGRAM ARGUMENT
           DISPLAY 'AZU0000I CALL BZUGTARG FOR CPRCHECK'
           CALL AZ-SUB-GETARG USING AZ-SUB-PROGRAM AZ-SUB-CSECT
             RETURNING AZ-SUB-ARG-LIST
           IF AZ-SUB-ARG-LIST = NULL
             MOVE 1 TO MESSAGE-LEN OF BZ-ASSERT
             STRING 'SUB PROGRAM CPRCHECK NOT FOUND.'
               DELIMITED BY SIZE
               INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           ELSE
              SET ADDRESS OF AZ-SUB-PGM-LIST TO AZ-SUB-ARG-LIST
              IF AZ-SUB-PGM-COUNT NOT EQUAL 4
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
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002C
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(1)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(1))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(2)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002D
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(2)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(2))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(3)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002E
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(3)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(3))
              SET ADDRESS OF AZ-LINKPARM1 TO AZ-SUB-PGM-ADDR(4)
              SET ADDRESS OF AZ-LINKPARM2 TO ADDRESS OF ZUT0000002F
              MOVE AZ-LINKPARM1(1:AZ-SUB-PGM-LGTH(4)) TO
                AZ-LINKPARM2(1:AZ-SUB-PGM-LGTH(4))
           END-IF.
      * SET INPUT VALUE
           MOVE 0 TO RETURN-CODE.
      * CALL TEST PROGRAM
           DISPLAY 'AZU0000I CALL CPRCHECK'
           CALL BZUGETEP USING BY REFERENCE AZ-SUB-PROGRAM AZ-SUB-CSECT
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
           USING ZUT0000002C ZUT0000002D ZUT0000002E ZUT0000002F
           .
      * EVALUATE OUTPUT VALUE
           MOVE 4 TO RETURN-CODE
      * END
           DISPLAY 'AZU0000I TEST_TEST2 END.'
           GOBACK.
       INITIALIZE-PARM.
           INITIALIZE ZUT0000002C
           INITIALIZE ZUT0000002D
           INITIALIZE ZUT0000002E
           INITIALIZE ZUT0000002F
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
      *| ZUNIT BZU_TEST                                                |
      *|     THIS PROGRAM IS CALLBACK DEFINITION FOR TEST              |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'BZU_TEST'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-NAME   PIC X(8)  VALUE 'CPRCHECK'.
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
      *  *** LS-CPR-NR : ZUT0000002C
       1 ZUT0000002C PIC X(10).
      *  *** LS-AGE : ZUT0000002D
       1 ZUT0000002D PIC S9(4) BINARY.
      *  *** LS-GENDER : ZUT0000002E
       1 ZUT0000002E PIC X.
      *  *** RC : ZUT0000002F
       1 ZUT0000002F PIC X.
       01 AZ-RECORD-COUNT     PIC 9(5) COMP-5.
       PROCEDURE DIVISION.
      * SET INPUT VALUE
           ENTRY "PGM_INPT_CPRFILE_CPRCHECK" USING AZ-TEST AZ-INFO-BLOCK
           ZUT0000002C ZUT0000002D ZUT0000002E ZUT0000002F.
           DISPLAY 'AZU0000I PGM_INPT_CPRFILE_CPRCHECK INPUT VALUES...'.
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
           ENTRY "PGM_OUTP_CPRFILE_CPRCHECK" USING AZ-TEST AZ-INFO-BLOCK
           ZUT0000002C ZUT0000002D ZUT0000002E ZUT0000002F.
           DISPLAY 'AZU0000I PGM_OUTP_CPRFILE_CPRCHECK CHECK VALUES...'.
           MOVE 4 TO RETURN-CODE.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR CHARACTERS
             BEFORE INITIAL SPACE.
           EVALUATE AZ-TEST(1:AZ-TEST-NAME-LEN)
           WHEN SPACE
             CONTINUE
           WHEN 'TEST1'
             PERFORM CHECK-REC-TEST1
             MOVE 4 TO RETURN-CODE
           WHEN 'TEST2'
             PERFORM CHECK-REC-TEST2
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
      * FOR CPRTODAY
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
               ' IN CPRTODAY.'
               DELIMITED BY SIZE INTO MESSAGE-TXT OF BZ-ASSERT
               WITH POINTER MESSAGE-LEN OF BZ-ASSERT
             END-STRING
             SUBTRACT 1 FROM MESSAGE-LEN OF BZ-ASSERT
             PERFORM THROW-ASSERTION-M
           END-IF.
           EXIT.
       CHECK-REC-TEST2.
      * CHECK RECORD COUNT FOR TEST2
      * FOR CPRTODAY
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
               ' IN CPRTODAY.'
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
           VALUE 'ccf4c002-9892-40b8-88cf-d51be1e755ea'.
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
           DISPLAY 'AZU0001I FOR TEST RUNNER: 100-20231122-1256'
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
       LINKAGE SECTION.
       01 AZ-TEST               PIC X(80).
       01 AZ-INFO-BLOCK.
           COPY BZUITERC.
       PROCEDURE DIVISION USING AZ-TEST
                                AZ-INFO-BLOCK.
           MOVE 0 TO AZ-TEST-NAME-LEN.
           INSPECT AZ-TEST TALLYING AZ-TEST-NAME-LEN FOR
           CHARACTERS BEFORE INITIAL SPACE.
           DISPLAY 'AZU0000I BZU_TERM: ' AZ-TEST(1:AZ-TEST-NAME-LEN)
           GOBACK.
       END PROGRAM BZU_TERM.
      *+---------------------------------------------------------------+
      *| ZUNIT PROGRAM FOR CPRTODAY                                    |
      *| TEST CASE VERSION: 103                                        |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'PGM_CPRTODAY'.
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
      *  *** WS-TODAY : ZUT00000025
       1 ZUT00000025 PIC 9(8).
      *
       PROCEDURE DIVISION.
      * CHECK OUTPUT VALUE
           ENTRY "PGM_INPT_CPRFILE_CPRTODAY" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000025.
           DISPLAY 'AZU0000I PGM_INPT_CPRFILE_CPRTODAY CHECK VALUES...'.
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
           WHEN 'TEST2'
             PERFORM P-OUTPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
      * SET INPUT VALUE
           ENTRY "PGM_OUTP_CPRFILE_CPRTODAY" USING
              AZ-TEST AZ-INFO-BLOCK
           ZUT00000025.
           DISPLAY 'AZU0000I PGM_OUTP_CPRFILE_CPRTODAY INPUT VALUES...'.
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
           WHEN 'TEST2'
             PERFORM P-INPUT-TEST2
           WHEN OTHER
             CONTINUE
           END-EVALUATE.
           PERFORM TEARDOWN.
       TEARDOWN.
           DISPLAY 'AZU0000I PGM_CPRTODAY END.'
           GOBACK.
       P-OUTPUT-TEST1.
           IF AZ-RECORD-COUNT-OT = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       P-OUTPUT-TEST2.
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
       P-INPUT-TEST2.
           IF AZ-RECORD-COUNT-IN = 0 THEN
             CONTINUE
           ELSE
             CONTINUE
           END-IF.
           EXIT.
       END PROGRAM 'PGM_CPRTODAY'.
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
         03 RECORD-COUNT-IO OCCURS 1.
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
             COMPUTE DATA-SIZE = LENGTH OF WK-RECORD-COUNT * 2 * 1
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
