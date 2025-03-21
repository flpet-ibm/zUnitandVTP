       ID DIVISION.
       PROGRAM-ID. CPRDCHE3.
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
       01  A              PIC X(10).
       01  B                 REDEFINES A     .
           05 CPR-DAG    PIC 99.
           05 CPR-MAANED  PIC 99.
           05 CPR-AAR     PIC 99.
           05 CPR-KONTROLCIFFER PIC 9999.
       01  WS-CPR-CIFFER-TABEL  REDEFINES A     .
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
       01  WS-IDAG         PIC 9(8).

       01  WS-I            PIC S9(8) BINARY.
       01  WS-SUM          PIC S9(8) BINARY.
       01  WS-DUMMY        PIC S9(8) BINARY.
       01  WS-CHECK-CIFFER PIC S9(8) BINARY.
       01  WS-CPR9         PIC 9(10).
       01  ws-cprx redefines ws-cpr9 pic x(10) .

       LINKAGE SECTION.

       01 LS-1      PIC X(10).

       01 LS-ALDER  PIC S9(4) BINARY.
       01 LS-KOEN         PIC X.
       01 RC        PIC X.

       PROCEDURE DIVISION USING LS-1      LS-ALDER LS-KOEN   RC.
      *
       MAIN SECTION.
       MAIN1.
           MOVE LS-1      to A     .
           MOVE '0' TO RC.

           MOVE 'CPRTODAD' TO WS-MODUL .
           CALL WS-MODUL  USING WS-IDAG .
           DISPLAY 'TODAY IS ' WS-IDAG .

           PERFORM CHECK-char-3-4.
           IF RC = '0' THEN
              PERFORM CHECK-char1-2
           END-IF.

           IF RC = '0' THEN
              PERFORM CHECK-char-10
           END-IF.

           EXIT PROGRAM.

       CHECK-char1-2 SECTION.
           IF CPR-DAG = 0 OR CPR-DAG > WS-DAG-I-MAANED(CPR-MAANED)
                                                                  THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK-char-3-4 SECTION.

           IF CPR-MAANED = 0 OR CPR-MAANED > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

        CHECK-char-10      SECTION.

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

       END PROGRAM CPRDCHE3.
