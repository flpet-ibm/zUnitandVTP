       ID DIVISION.
       PROGRAM-ID. CPRCHECD.
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
      *
      *
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CPR         PIC X(10).
       01  WS-CPR-STRUC   REDEFINES WS-CPR.
           05 CPR-DAY    PIC 99.
           05 CPR-MONTH   PIC 99.
           05 CPR-YEAR    PIC 99.
           05 CPR-CONTROL PIC 9999.
           05 CPR-CENTURY-DIGIT REDEFINES CPR-CONTROl PIC 9.
                88 CPR-BIRTH-1800-2000 VALUE 5 6 7 8.
                88 CPR-BIRTH-1900      VALUE 0 1 2 3.
                88 CPR-BIRTH-1900-2000 VALUE 4 9.

       01 WS-CPR-DIGIT-TAB REDEFINES WS-CPR.
           05 WS-CPR-DIGIT PIC 9 OCCURS 10.
       01  WS-DIM-CONST.
           05 DIM-01       PIC S9(4) BINARY VALUE 31.
           05 DIM-02       PIC S9(4) BINARY VALUE 28.
           05 DIM-03       PIC S9(4) BINARY VALUE 31.
           05 DIM-04       PIC S9(4) BINARY VALUE 30.
           05 DIM-05       PIC S9(4) BINARY VALUE 31.
           05 DIM-06       PIC S9(4) BINARY VALUE 30.
           05 DIM-07       PIC S9(4) BINARY VALUE 31.
           05 DIM-08       PIC S9(4) BINARY VALUE 31.
           05 DIM-09       PIC S9(4) BINARY VALUE 30.
           05 DIM-10       PIC S9(4) BINARY VALUE 31.
           05 DIM-11       PIC S9(4) BINARY VALUE 30.
           05 DIM-12       PIC S9(4) BINARY VALUE 31.
       01  WS-DAY-IN-MONTH-TAB REDEFINES WS-DIM-CONST.
           05 WS-DAY-IN-MONTH PIC S9(4) BINARY OCCURS 12.

       01  WS-FACTOR-CONST.
           05 FACTOR-01       PIC S9(4) BINARY VALUE 4.
           05 FACTOR-02       PIC S9(4) BINARY VALUE 3.
           05 FACTOR-03       PIC S9(4) BINARY VALUE 2.
           05 FACTOR-04       PIC S9(4) BINARY VALUE 7.
           05 FACTOR-05       PIC S9(4) BINARY VALUE 6.
           05 FACTOR-06       PIC S9(4) BINARY VALUE 5.
           05 FACTOR-07       PIC S9(4) BINARY VALUE 4.
           05 FACTOR-08       PIC S9(4) BINARY VALUE 3.
           05 FACTOR-09       PIC S9(4) BINARY VALUE 2.
           05 FACTOR-10       PIC S9(4) BINARY VALUE 1.
       01  WS-FACTOR-TAB REDEFINES WS-FACTOR-CONST.
           05 WS-FACTOR       PIC S9(4) BINARY OCCURS 10.
       01  WS-MODULE       PIC X(8).
       01  WS-TODAY.
           05 WS-TODAY-YEAR   PIC 9999.
           05 WS-TODAY-MONTH  PIC 99.
           05 WS-TODAY-DAY    PIC 99.

       01  WS-I            PIC S9(8) BINARY.
       01  WS-BIRTH-YEAR   PIC 9999.
       01  WS-SUM          PIC S9(8) BINARY.
       01  WS-DUMMY        PIC S9(8) BINARY.
       01  WS-CHECK-DIGIT  PIC S9(8) BINARY.

       LINKAGE SECTION.

       01 LS-CPR-NR PIC X(10).

       01 LS-AGE    PIC S9(4) BINARY.
       01 LS-GENDER       PIC X.
       01 RC        PIC X.

       PROCEDURE DIVISION USING LS-CPR-NR LS-AGE LS-GENDER RC.
      *
       MAIN SECTION.
       MAIN1.
           MOVE LS-CPR-NR to ws-cpr.
           MOVE '0' TO RC.

           MOVE 'CPRTODAD' TO WS-MODULE.
           CALL WS-MODULE USING WS-TODAY.
           DISPLAY 'TODAY IS ' WS-TODAY.

           PERFORM CHECK-YEAR.
           IF RC = '0' THEN
              PERFORM CHECK-MONTH
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-DAY
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-CHECK-DIGIT
           END-IF.

           IF RC = '0' THEN
              PERFORM CALCULATE-AGE-AND-GENDER
           END-IF.
           EXIT PROGRAM.

       CHECK-DAY SECTION.
           IF CPR-DAY = 0 OR CPR-DAY > WS-DAY-IN-MONTH(CPR-MONTH) THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK-MONTH SECTION.

           IF CPR-MONTH = 0 OR CPR-MONTH > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

       CHECK-YEAR SECTION.

           IF CPR-YEAR IS NOT NUMERIC THEN
              MOVE '3' TO RC
           END-IF.

           EXIT.

        CHECK-CHECK-DIGIT SECTION.

           COMPUTE WS-SUM = 0.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
              COMPUTE WS-SUM = WS-SUM +
                     (WS-CPR-DIGIT(WS-I) * WS-FACTOR(WS-I) )
           END-PERFORM.
           DIVIDE WS-SUM BY 11 GIVING WS-DUMMY REMAINDER WS-CHECK-DIGIT
           IF WS-CHECK-DIGIT NOT EQUAL 0 THEN
              MOVE '9' TO RC
           END-IF
           EXIT.

       CALCULATE-AGE-AND-GENDER SECTION.

           EVALUATE TRUE
             WHEN CPR-BIRTH-1900
              COMPUTE WS-BIRTH-YEAR = 1900 + CPR-YEAR
             WHEN CPR-BIRTH-1800-2000
                IF CPR-MONTH <= 57 THEN
                   COMPUTE WS-BIRTH-YEAR = 2000 + CPR-YEAR
                ELSE
                   COMPUTE WS-BIRTH-YEAR = 1800 + CPR-YEAR
                END-IF
             WHEN CPR-BIRTH-1900-2000
                IF CPR-MONTH <= 36 THEN
                   COMPUTE WS-BIRTH-YEAR = 2000 + CPR-YEAR
                ELSE
                   COMPUTE WS-BIRTH-YEAR = 1900 + CPR-YEAR
                END-IF
           END-EVALUATE.

           COMPUTE LS-AGE = WS-TODAY-YEAR - WS-BIRTH-YEAR - 1.
           IF WS-TODAY-MONTH > CPR-MONTH OR
              (WS-TODAY-MONTH = CPR-MONTH AND WS-TODAY-DAY >= CPR-DAY)
           THEN
              ADD 1 TO LS-AGE
           END-IF.

           DIVIDE WS-CPR-DIGIT(10) BY 2 GIVING WS-DUMMY REMAINDER WS-I
           IF WS-I = 0 THEN
              MOVE 'F' TO LS-GENDER
           ELSE
              MOVE 'M' TO LS-GENDER
           END-IF.
       END PROGRAM CPRCHECD.