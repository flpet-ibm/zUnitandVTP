      ******************************************************************
      * DCLGEN TABLE(GENASA1.CLAIM)                                    *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(CLAIM))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CLAIM TABLE
           ( CLAIMNUMBER                    INTEGER NOT NULL,
             POLICYNUMBER                   INTEGER NOT NULL,
             CLAIMDATE                      DATE,
             PAID                           INTEGER,
             VALUE                          INTEGER,
             CAUSE                          CHAR(255),
             OBSERVATIONS                   CHAR(255)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.CLAIM                      *
      ******************************************************************
       01  DCLCLAIM.
           10 CLAIMNUMBER          PIC S9(9) USAGE COMP-5.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP-5.
           10 CLAIMDATE            PIC X(10).
           10 PAID                 PIC S9(9) USAGE COMP-5.
           10 WS-VALUE                PIC S9(9) USAGE COMP-5.
           10 CAUSE                PIC X(255).
           10 OBSERVATIONS         PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************