       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLEMSGCU.
      ******************************************************************
      *REMARKS.  BATCH COBOL PROGRAM.
      *          USE PROCESSOR COBNBL.
      *****************************************************************
      *******
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  LS-CUSTNO        PIC X(10).
       01  LS-CUSTOMERNAME  PIC X(30).
      ******************************************************************
       PROCEDURE DIVISION USING LS-CUSTNO LS-CUSTOMERNAME.

           move 'John Johnson' to LS-CUSTOMERNAME.

           GOBACK.