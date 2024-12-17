       ID DIVISION.
       PROGRAM-ID. ADDIPGM2.
      ***
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  MY-CUST.
           COPY ADDICPY1.

       01 MY-PGM PIC X(8).
       01  WS-ABSTIME                  PIC S9(8) COMP VALUE +0.


       LINKAGE SECTION.
       01  LS-CUST.
           COPY ADDICPY1.

       PROCEDURE DIVISION USING LS-CUST.
      *
       MAIN SECTION.
       MAIN1.

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC

           MOVE LS-CUST TO MY-CUST

           MOVE 'ADDIPGMS' TO MY-PGM
           CALL MY-PGM USING MY-CUST

           MOVE MY-CUST TO LS-CUST.

       END PROGRAM ADDIPGM2.