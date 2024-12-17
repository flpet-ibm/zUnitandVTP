       ID DIVISION.
       PROGRAM-ID. ADDIPGMI.
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
           COPY ADDICPY1.

       PROCEDURE DIVISION USING MY-CUST.
      *
       MAIN SECTION.
       MAIN1.

            MOVE FSP-CUSTOMER-NUM IN MY-CUST TO
                 DB2-CUSTOMERNUMBER-INT

             EXEC SQL
               INSERT INTO CUSTOMER
                         ( CUSTOMERNUMBER,
                           FIRSTNAME,
                           LASTNAME,
                           DATEOFBIRTH,
                           HOUSENAME,
                           HOUSENUMBER,
                           POSTCODE,
                           PHONEMOBILE,
                           PHONEHOME,
                           EMAILADDRESS )
                  VALUES ( :DB2-CUSTOMERNUMBER-INT,
                           :FSP-FIRST-NAME,
                           :FSP-LAST-NAME,
                           :FSP-DOB,
                           :FSP-HOUSE-NAME,
                           :FSP-HOUSE-NUM,
                           :FSP-POSTCODE,
                           :FSP-PHONE-MOBILE,
                           :FSP-PHONE-HOME,
                           :FSP-EMAIL-ADDRESS )
             END-EXEC.

       END PROGRAM ADDIPGMI.