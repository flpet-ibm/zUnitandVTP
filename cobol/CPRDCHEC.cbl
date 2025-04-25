       ID DIVISION.
       PROGRAM-ID. CPRDCHEC.
      *    T
      *
      *
      *
      *    (C) 2024 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CPR         PIC X(10).
       01  WS-CPR-STRUKTUR   REDEFINES WS-CPR.
           05 CPR-DAG    PIC 99.
           05 CPR-MAANED  PIC 99.
           05 CPR-AAR     PIC 99.
           05 CPR-KONTROLCIFFER PIC 9999.
           05 CPR-AARHUND-CIFFER REDEFINES CPR-KONTROLCIFFER PIC 9.
                88 CPR-FODSEL-1800-2000 VALUE 5 6 7 8.
                88 CPR-FODSEL-1900      VALUE 0 1 2 3.
                88 CPR-FODSEL-1900-2000 VALUE 4 9.
       01  WS-CPR-CIFFER-TABEL  REDEFINES WS-CPR.
           05 WS-CPR-CIFFER PIC 9 OCCURS 10.
       01  WS-DIM-KONSTANTER.
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
       01  WS-DAG-I-MAANED-TABEL REDEFINES WS-DIM-KONSTANTER.
           05 WS-DAG-I-MAANED PIC S9(4) BINARY OCCURS 12.

       01  WS-FAKTOR-KONSTANT.
           05 FAKTOR-01       PIC S9(4) BINARY VALUE 4.
           05 FAKTOR-02       PIC S9(4) BINARY VALUE 3.
           05 FAKTOR-03       PIC S9(4) BINARY VALUE 2.
           05 FAKTOR-04       PIC S9(4) BINARY VALUE 7.
           05 FAKTOR-05       PIC S9(4) BINARY VALUE 6.
           05 FAKTOR-06       PIC S9(4) BINARY VALUE 5.
           05 FAKTOR-07       PIC S9(4) BINARY VALUE 4.
           05 FAKTOR-08       PIC S9(4) BINARY VALUE 3.
           05 FAKTOR-09       PIC S9(4) BINARY VALUE 2.
           05 FAKTOR-10       PIC S9(4) BINARY VALUE 1.
       01  WS-FAKTOR-TAB REDEFINES WS-FAKTOR-KONSTANT.
           05 WS-FAKTOR       PIC S9(4) BINARY OCCURS 10.
       01  WS-MODUL        PIC X(8).
       01  WS-IDAG.
           05 WS-IDAG-AAR     PIC 9999.
           05 WS-IDAG-MAANED  PIC 99.
           05 WS-IDAG-DAG    PIC 99.


       01  WS-I            PIC S9(8) BINARY.
       01  WS-FODSEL-AAR   PIC 9999.
       01  WS-SUM          PIC S9(8) BINARY.
       01  WS-DUMMY        PIC S9(8) BINARY.
       01  WS-CHECK-CIFFER PIC S9(8) BINARY.

       LINKAGE SECTION.

       01 LS-CPR-NR PIC X(10).

       01 LS-ALDER  PIC S9(4) BINARY.
       01 LS-KOEN         PIC X.
       01 RC        PIC X.

       PROCEDURE DIVISION USING LS-CPR-NR LS-ALDER LS-KOEN   RC.
      *
       MAIN SECTION.
       MAIN1.
           MOVE LS-CPR-NR to ws-cpr.
           MOVE '0' TO RC.

           MOVE 'CPRTODAD' TO WS-MODUL .
           CALL WS-MODUL  USING WS-IDAG .
           DISPLAY 'TODAY IS ' WS-IDAG .

           PERFORM CHECK-AAR.
           IF RC = '0' THEN
              PERFORM CHECK-MAANED
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-DAG
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-CHECK-CIFFER
           END-IF.

           IF RC = '0' THEN
              PERFORM BEREGN-ALDER-OG-KOEN
           END-IF.
           EXIT PROGRAM.

       CHECK-DAG SECTION.
           IF CPR-DAG = 0 OR CPR-DAG > WS-DAG-I-MAANED(CPR-MAANED) THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK-MAANED SECTION.

           IF CPR-MAANED = 0 OR CPR-MAANED > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

       CHECK-AAR SECTION.

           IF CPR-AAR IS NOT NUMERIC THEN
              MOVE '3' TO RC
           END-IF.

           EXIT.

        CHECK-CHECK-CIFFER SECTION.

           COMPUTE WS-SUM = 0.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
              COMPUTE WS-SUM = WS-SUM +
                     (WS-CPR-CIFFER(WS-I) * WS-FAKTOR(WS-I) )
           END-PERFORM.
           DIVIDE WS-SUM BY 11 GIVING WS-DUMMY REMAINDER WS-CHECK-CIFFER

           IF WS-CHECK-CIFFER NOT EQUAL 0 THEN
              MOVE '9' TO RC
           END-IF
           EXIT.

       BEREGN-ALDER-OG-KOEN SECTION.

           EVALUATE TRUE
             WHEN CPR-FODSEL-1900
              COMPUTE WS-FODSEL-AAR = 1900 + CPR-AAR
             WHEN CPR-FODSEL-1800-2000
                IF CPR-MAANED <= 57 THEN
                   COMPUTE WS-FODSEL-AAR = 2000 + CPR-AAR
                ELSE
                   COMPUTE WS-FODSEL-AAR = 1800 + CPR-AAR
                END-IF
             WHEN CPR-FODSEL-1900-2000
                IF CPR-MAANED <= 36 THEN
                   COMPUTE WS-FODSEL-AAR = 2000 + CPR-AAR
                ELSE
                   COMPUTE WS-FODSEL-AAR = 1900 + CPR-AAR
                END-IF
           END-EVALUATE.

           COMPUTE LS-ALDER = WS-IDAG-AAR - WS-FODSEL-AAR - 1.
           IF WS-IDAG-MAANED > CPR-MAANED OR
              (WS-IDAG-MAANED = CPR-MAANED AND WS-IDAG-DAG >= CPR-DAG)
           THEN
              ADD 1 TO LS-ALDER
           END-IF.

           DIVIDE WS-CPR-CIFFER(10) BY 2 GIVING WS-DUMMY REMAINDER WS-I
           IF WS-I = 0 THEN
              MOVE 'F' TO LS-KOEN
           ELSE
              MOVE 'M' TO LS-KOEN
           END-IF.
       END PROGRAM CPRDCHEC.