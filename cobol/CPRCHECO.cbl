       ID DIVISION.
       PROGRAM-ID. CPRCHECO.
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
      *
      *    This program's variables and section names has been
      *    obfuscated on purpose to make the program hard to read and
      *    understand. WCA4Z Code Explain can help you understand what
      *    this program is doing
      *
      *    (C) 2025 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  A              PIC X(10).
       01  B              REDEFINES A     .
           05 B1         PIC 99.
           05 B2          PIC 99.
           05 B3          PIC 99.
           05 B4          PIC 9999.
           05 B5                REDEFINES B4          PIC 9.
                88 B5-OPTION-1         VALUE 5 6 7 8.
                88 B5-OPTION-2         VALUE 0 1 2 3.
                88 B5-OPTION-3         VALUE 4 9.

       01 A-TAB            REDEFINES A     .
           05 A-TAB-DIGIT  PIC 9 OCCURS 10.
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
       01  DIM-TAB             REDEFINES WS-DIM-CONST.
           05 DIM             PIC S9(4) BINARY OCCURS 12.

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
       01  WS-T    .
           05 WS-T1           PIC 9999.
           05 WS-T2           PIC 99.
           05 WS-T3           PIC 99.

       01  WS-I            PIC S9(8) BINARY.
       01  WS-J            PIC 9999.
       01  WS-K            PIC S9(8) BINARY.
       01  WS-L            PIC S9(8) BINARY.
       01  WS-M            PIC S9(8) BINARY.

       LINKAGE SECTION.

       01 LS-1      PIC X(10).

       01 LS-2      PIC S9(4) BINARY.
       01 LS-3            PIC X.
       01 RC        PIC X.

       PROCEDURE DIVISION USING LS-1      LS-2   LS-3      RC.
      *
       MAIN SECTION.
       MAIN1.
           MOVE LS-1      to A     .
           MOVE '0' TO RC.

           MOVE 'CPRTODAD' TO WS-MODULE.
           CALL WS-MODULE USING WS-T    .

           PERFORM CHECK-1   .
           IF RC = '0' THEN
              PERFORM CHECK-2
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-3
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-4
           END-IF.

           IF RC = '0' THEN
              PERFORM CALCULATe
           END-IF.
           EXIT PROGRAM.

       CHECK-3   SECTION.
           IF B1      = 0 OR B1      > DIM(B2)                    THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK-2     SECTION.

           IF B2        = 0 OR B2        > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

       CHECK-1    SECTION.

           IF B3       IS NOT NUMERIC THEN
              MOVE '3' TO RC
           END-IF.

           EXIT.

        CHECK-4           SECTION.

           COMPUTE WS-K   = 0.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
              COMPUTE WS-K   = WS-K   +
                     (A-TAB-DIGIT(WS-I)  * WS-FACTOR(WS-I) )
           END-PERFORM.
           DIVIDE WS-K   BY 11 GIVING WS-L     REMAINDER WS-M
           IF WS-M           NOT EQUAL 0 THEN
              MOVE '9' TO RC
           END-IF
           EXIT.

       CALCULATe                SECTION.

           EVALUATE TRUE
             WHEN B5-OPTION-2
              COMPUTE WS-J          = 1900 + B3
             WHEN B5-OPTION-1
                IF B2        <= 57 THEN
                   COMPUTE WS-J          = 2000 + B3
                ELSE
                   COMPUTE WS-J          = 1800 + B3
                END-IF
             WHEN B5-OPTION-3
                IF B2        <= 36 THEN
                   COMPUTE WS-J          = 2000 + B3
                ELSE
                   COMPUTE WS-J          = 1900 + B3
                END-IF
           END-EVALUATE.

           COMPUTE LS-2   = WS-T1         - WS-J          - 1.
           IF WS-T2          > B2        OR
              (WS-T2          = B2        AND WS-T3        >= B1)
           THEN
              ADD 1 TO LS-2
           END-IF.

           DIVIDE A-TAB-DIGIT(10)  BY 2 GIVING WS-L     REMAINDER WS-I
           IF WS-I = 0 THEN
              MOVE 'F' TO LS-3
           ELSE
              MOVE 'M' TO LS-3
           END-IF.
       END PROGRAM CPRCHECO.