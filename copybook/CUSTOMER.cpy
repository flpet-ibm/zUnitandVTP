      ******************************************************************
      * DCLGEN TABLE(GENASA1.CUSTOMER)                                 *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(CUSTOMER))                 *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CUSTOMER TABLE
           ( CUSTOMERNUMBER                 INTEGER NOT NULL,
             FIRSTNAME                      CHAR(10),
             LASTNAME                       CHAR(20),
             DATEOFBIRTH                    DATE,
             HOUSENAME                      CHAR(20),
             HOUSENUMBER                    CHAR(4),
             POSTCODE                       CHAR(8),
             PHONEHOME                      CHAR(20),
             PHONEMOBILE                    CHAR(20),
             EMAILADDRESS                   CHAR(100)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.CUSTOMER                   *
      ******************************************************************
       01  DCLCUSTOMER.
           10 CUSTOMERNUMBER       PIC S9(9) USAGE COMP-5.
           10 FIRSTNAME            PIC X(10).
           10 LASTNAME             PIC X(20).
           10 DATEOFBIRTH          PIC X(10).
           10 HOUSENAME            PIC X(20).
           10 HOUSENUMBER          PIC X(4).
           10 POSTCODE             PIC X(8).
           10 PHONEHOME            PIC X(20).
           10 PHONEMOBILE          PIC X(20).
           10 EMAILADDRESS         PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************