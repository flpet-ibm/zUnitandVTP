       ID DIVISION.
       PROGRAM-ID. ADDIPGM1.
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
       01 my-text  pic x(80).
       PROCEDURE DIVISION.
      *
       MAIN SECTION.
       MAIN1.

           MOVE 1 TO FSP-CUSTOMER-NUM IN MY-CUST

           MOVE 'ADDIPGM2' TO MY-PGM
           CALL MY-PGM USING DFHEIBLK DFHCOMMAREA MY-CUST

           DISPLAY FSP-FIRST-NAME IN FSP-CUSTOMER-REQUEST
           DISPLAY FSP-LAST-NAME IN FSP-CUSTOMER-REQUEST

           MOVE 2 TO FSP-CUSTOMER-NUM IN MY-CUST

           MOVE 'ADDIPGM4' TO MY-PGM
           CALL MY-PGM USING DFHEIBLK DFHCOMMAREA MY-CUST


           MOVE FSP-FIRST-NAME IN FSP-CUSTOMER-REQUEST TO
                MY-TEXT.
           DISPLAY MY-TEXT.
           MOVE FSP-LAST-NAME IN FSP-CUSTOMER-REQUEST TO
                MY-TEXT
           DISPLAY MY-TEXT.

           EXEC CICS RETURN END-EXEC.


       END PROGRAM ADDIPGM1.