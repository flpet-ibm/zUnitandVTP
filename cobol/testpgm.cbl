       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGDB2MAI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * SQLCA DB2 communications area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE CUSTOMER
           END-EXEC.


           EXEC SQL
             DECLARE C1 CURSOR FOR
               SELECT CUSTOMERNUMBER,
                     FIRSTNAME,
                     LASTNAME,
                     DATEOFBIRTH,
                     HOUSENAME,
                     HOUSENUMBER,
                     POSTCODE,
                     PHONEHOME,
                     PHONEMOBILE,
                     EMAILADDRESS
               FROM CUSTOMER
               order by customernumber
           END-EXEC.

       01 PROGRAMNAME  PIC X(8) VALUE 'LGDB2AGE'.
       01 AGE          PIC S9(5)V99.
       01 DISP-AGE     PIC Z999,99.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
       MAIN.

           PERFORM OPEN-CURSOR.
           PERFORM FETCH-CURSOR.
           PERFORM WITH TEST BEFORE UNTIL SQLCODE NOT = 0

      *            DISPLAY CUSTOMERNUMBER ' ' FIRSTNAME ',' LASTNAME
                   PERFORM PROCESS-CUSTOMER
                   PERFORM FETCH-CURSOR
           END-PERFORM
           PERFORM CLOSE-CURSOR.

           STOP RUN.

       OPEN-CURSOR.
           EXEC SQL OPEN C1 END-EXEC.
           IF SQLCODE NOT = 0 THEN
              PERFORM DISPLAY-SQL-ERROR
           END-IF.

       FETCH-CURSOR.

             EXEC SQL
                 FETCH C1
                 INTO :CUSTOMERNUMBER,
                       :FIRSTNAME,
                       :LASTNAME,
                       :DATEOFBIRTH,
                       :HOUSENAME,
                       :HOUSENUMBER,
                       :POSTCODE,
                       :PHONEHOME,
                       :PHONEMOBILE,
                       :EMAILADDRESS
             END-EXEC.
           IF SQLCODE NOT = 0 THEN
              PERFORM DISPLAY-SQL-ERROR
           END-IF.

       CLOSE-CURSOR.

            EXEC SQL CLOSE C1 END-EXEC.
           IF SQLCODE NOT = 0 THEN
              PERFORM DISPLAY-SQL-ERROR
           END-IF.

       PROCESS-CUSTOMER.

      *    MOVE 'LGDB2AGE' TO PROGRAMNAME.
           CALL PROGRAMNAME USING DATEOFBIRTH AGE.
           MOVE AGE TO DISP-AGE
           DISPLAY 'CUSTOMER ' FIRSTNAME ' USING BORN ON ' DATEOFBIRTH
                   ' AND IS ' DISP-AGE ' YEARS OLD'.

       DISPLAY-SQL-ERROR.

           DISPLAY 'SQLERROR'
           DISPLAY 'SQLCODE  ' SQLCODE
           DISPLAY 'SQLSTATE ' SQLSTATE.

       END PROGRAM LGDB2MAI.
