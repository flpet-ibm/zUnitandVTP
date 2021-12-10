       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FILLER01.
      ******************************************************************
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
      ****************************************************************
      * WORKING-STORAGE SECTION
      ****************************************************************
       WORKING-STORAGE SECTION.
      *
       01 FILLER                   PIC X(32)  VALUE
                           'XXX MODUL FILLER01 START WSS XXX'.
       01 dato pic x(10).
      ******************************************************************
      *    KONSTANTER (KK)
      ******************************************************************
       01  FILLER                   PIC X(16)  VALUE 'KONSTANTERXXXXXX'.
      **
      * DATUM/TIDAREA
       01  FILGG019.
           COPY FILGG019.
       LINKAGE SECTION.

       01 FILGG057.
         COPY FILGG057.
      ****************************************************************
      * PROCEDURE DIVISION
      ****************************************************************
       PROCEDURE DIVISION USING FILGG057.
      ****************************************************************
      *
      ****************************************************************

       A-MAIN SECTION.

           INITIALIZE            FILGG019-PARAMETRAR
           CALL 'FILG0190' USING FILGG019-PARAMETRAR

           MOVE 0                          TO RETURN-CODE

           GOBACK
           .
       A999-SLUT.
           EXIT.