      ******************************************************************
      * DCLGEN TABLE(GENASA1.CUSTOMER_SECURE)                          *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(CUSTOMSE))                 *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CUSTOMER_SECURE TABLE
           ( CUSTOMERNUMBER                 INTEGER NOT NULL,
             CUSTOMERPASS                   CHAR(32),
             STATE_INDICATOR                CHAR(1),
             PASS_CHANGES                   INTEGER
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.CUSTOMER_SECURE            *
      ******************************************************************
       01  DCLCUSTOMER-SECURE.
           10 CUSTOMERNUMBER       PIC S9(9) USAGE COMP-5.
           10 CUSTOMERPASS         PIC X(32).
           10 STATE-INDICATOR      PIC X(1).
           10 PASS-CHANGES         PIC S9(9) USAGE COMP-5.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************