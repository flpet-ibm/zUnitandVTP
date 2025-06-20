       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGDB2AGE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-TODAY.
           05 WS-TODAY-YEAR   PIC 9999.
           05 WS-TODAY-MONTH  PIC 99.
           05 WS-TODAY-DAY    PIC 99.

       LINKAGE SECTION.
       01 DATEOFBIRTH  PIC X(10).
       01 FILLER REDEFINES DATEOFBIRTH.
           05 DOB-YEAR    PIC 9999.
           05 DOB-MONTH   PIC 99.
           05 DOB-DAY     PIC 99.

       01 AGE          PIC S9(5)V99.

       PROCEDURE DIVISION USING DATEOFBIRTH AGE.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
       MAIN.

           ACCEPT WS-TODAY FROM DATE YYYYMMDD.
           COMPUTE AGE = WS-TODAY-YEAR - DOB-YEAR.
           IF (WS-TODAY-MONTH = DOB-MONTH AND
               WS-TODAY-DAY > DOB-DAY)
              OR (WS-TODAY-MONTH > DOB-MONTH)
           THEN
              ADD 1 TO AGE
           END-IF
           GOBACK.


       END PROGRAM LGDB2AGE.
