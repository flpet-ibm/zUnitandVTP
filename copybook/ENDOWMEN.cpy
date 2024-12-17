      ******************************************************************
      * DCLGEN TABLE(GENASA1.ENDOWMENT)                                *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(ENDOWMEN))                 *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE ENDOWMENT TABLE
           ( POLICYNUMBER                   INTEGER NOT NULL,
             EQUITIES                       CHAR(1),
             WITHPROFITS                    CHAR(1),
             MANAGEDFUND                    CHAR(1),
             FUNDNAME                       CHAR(10),
             TERM                           SMALLINT,
             SUMASSURED                     INTEGER,
             LIFEASSURED                    CHAR(31),
             PADDINGDATA                    VARCHAR(32606)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.ENDOWMENT                  *
      ******************************************************************
       01  DCLENDOWMENT.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP-5.
           10 EQUITIES             PIC X(1).
           10 WITHPROFITS          PIC X(1).
           10 MANAGEDFUND          PIC X(1).
           10 FUNDNAME             PIC X(10).
           10 TERM                 PIC S9(4) USAGE COMP-5.
           10 SUMASSURED           PIC S9(9) USAGE COMP-5.
           10 LIFEASSURED          PIC X(31).
           10 PADDINGDATA.
              49 PADDINGDATA-LEN   PIC S9(4) USAGE COMP-5.
              49 PADDINGDATA-TEXT
                 PIC X(32606).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************