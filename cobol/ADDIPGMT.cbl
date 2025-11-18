       ID DIVISION.
       PROGRAM-ID. ADDIPGMT.
      ***
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Host variables for input to DB2 integer types

       01  DB2-IN-INTEGERS.
           03 DB2-CUSTOMERNUMBER-INT   PIC S9(9) COMP.

           EXEC SQL INCLUDE SQLCA      END-EXEC.
           EXEC SQL INCLUDE CUSTOMER   END-EXEC.

       LINKAGE SECTION.
       01  MY-CUST.
           COPY ADDICPY2 REPLACING ==:DELIM:== by ==FSX-==.


       PROCEDURE DIVISION USING MY-CUST.
      *
       MAIN SECTION.
       MAIN1.

            MOVE FSX-CUSTOMER-NUM IN MY-CUST TO
                 DB2-CUSTOMERNUMBER-INT

            EXEC SQL
               SELECT FIRSTNAME,
                      LASTNAME,
                      DATEOFBIRTH,
                      HOUSENAME,
                      HOUSENUMBER,
                      POSTCODE,
                      PHONEMOBILE,
                      PHONEHOME,
                      EMAILADDRESS
               INTO  :FSX-FIRST-NAME,
                     :FSX-LAST-NAME,
                     :FSX-DOB,
                     :FSX-HOUSE-NAME,
                     :FSX-HOUSE-NUM,
                     :FSX-POSTCODE,
                     :FSX-PHONE-MOBILE,
                     :FSX-PHONE-HOME,
                     :FSX-EMAIL-ADDRESS
               FROM CUSTOMER
               WHERE CUSTOMERNUMBER = :DB2-CUSTOMERNUMBER-INT
           END-EXEC.

       END PROGRAM ADDIPGMT.