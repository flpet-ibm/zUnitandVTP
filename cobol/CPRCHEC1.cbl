       ID DIVISION.
       PROGRAM-ID. CPRCHECD.
      *     Changed some of the varaibles and paragrahs from
      *     descriptive names to non descriptive names.
      *
      *     This is to see of WCA4Z Explain can still provide help
      *     For this is still is able to say that it is a CPR check.
      *
      *    (C) 2024 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CPR         PIC X(10).
       01  WS-CPR-STRUC   REDEFINES WS-CPR.
           05 A          PIC 99.
           05 B           PIC 99.
           05 C           PIC 99.
           05 D           PIC 9999.
           05 CPR-CENTURY-DIGIT REDEFINES D           PIC 9.
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
       01  T       .
           05 TA              PIC 9999.
           05 TB              PIC 99.
           05 TC              PIC 99.

       01  WS-I            PIC S9(8) BINARY.
       01  Y               PIC 9999.
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
           CALL WS-MODULE USING T       .
           DISPLAY 'TODAY IS ' T       .

           PERFORM CHECK3    .
           IF RC = '0' THEN
              PERFORM CHECK2
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK1
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-CHECK-DIGIT
           END-IF.

           IF RC = '0' THEN
              PERFORM CALCULATE-AGE-AND-GENDER
           END-IF.
           EXIT PROGRAM.

       CHECK1    SECTION.
           IF A       = 0 OR A       > WS-DAY-IN-MONTH(B)         THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK2      SECTION.

           IF B         = 0 OR B         > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

       CHECK3     SECTION.

           IF C        IS NOT NUMERIC THEN
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
              COMPUTE Y             = 1900 + C
             WHEN CPR-BIRTH-1800-2000
                IF B         <= 57 THEN
                   COMPUTE Y             = 2000 + C
                ELSE
                   COMPUTE Y             = 1800 + C
                END-IF
             WHEN CPR-BIRTH-1900-2000
                IF B         <= 36 THEN
                   COMPUTE Y             = 2000 + C
                ELSE
                   COMPUTE Y             = 1900 + C
                END-IF
           END-EVALUATE.

           COMPUTE LS-AGE = TA            - Y             - 1.
           IF TB             > B         OR
              (TB             = B         AND TC           >= A)
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