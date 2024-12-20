       ID DIVISION.
       PROGRAM-ID. CPRDCHE5.
      *    T
      *    used the explain of CPRDCHE4 to use the help there to
      *    rename 2 variables to more meanful names
      *
      *    (C) 2024 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  A              PIC X(10).
       01  B                 REDEFINES A     .
           05 WS-day     PIC 99.
           05 WS-MAANED   PIC 99.
           05 e           PIC 99.
           05 f                 PIC 9999.
       01  g                    REDEFINES A     .
           05 H             PIC 9 OCCURS 10.
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
       01  i                     REDEFINES WS-DIM-KONSTANTER.
           05 j               PIC S9(4) BINARY OCCURS 12.

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
       01  k               PIC S9(8) BINARY.
       01  l               PIC 9(10).
       01  m       redefines l       pic x(10) .

       LINKAGE SECTION.

       01 LS-1      PIC X(10).

       01 ls-2      PIC S9(4) BINARY.
       01 ls-3            PIC X.
       01 RC        PIC X.

       PROCEDURE DIVISION USING LS-1      ls-2     ls-3      RC.
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
           IF WS-day  = 0 OR WS-day  > j(WS-MAANED)
                                                                  THEN
              MOVE '1' TO RC
           END-IF.

           EXIT.


       CHECK-char-3-4 SECTION.

           IF WS-MAANED  = 0 OR WS-MAANED  > 12 THEN
              MOVE '2' TO RC
           END-IF.

           EXIT.

        CHECK-char-10      SECTION.

           COMPUTE WS-SUM = 0.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
              COMPUTE WS-SUM = WS-SUM +
                     (H(WS-I)             * WS-FAKTOR(WS-I) )
           END-PERFORM.
           DIVIDE WS-SUM BY 11 GIVING WS-DUMMY REMAINDER k

           IF k               NOT EQUAL 0 THEN
              MOVE '9' TO RC
           END-IF
           EXIT.

       END PROGRAM CPRDCHE5.