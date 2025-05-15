       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGDB2BAT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
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
           END-EXEC.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
       MAIN.

           PERFORM OPEN-CURSOR.
           PERFORM FETCH-CURSOR.
           PERFORM WITH TEST BEFORE UNTIL SQLCODE NOT = 0

                   DISPLAY CUSTOMERNUMBER ' ' FIRSTNAME ',' LASTNAME
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

       DISPLAY-SQL-ERROR.

           DISPLAY 'SQLERROR'
           DISPLAY 'SQLCODE  ' SQLCODE
           DISPLAY 'SQLSTATE ' SQLSTATE.

       END PROGRAM LGDB2BAT.
