      ******************************************************************
      * DCLGEN TABLE(GENASA1.MOTOR)                                    *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(MOTOR))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MOTOR TABLE
           ( POLICYNUMBER                   INTEGER NOT NULL,
             MAKE                           CHAR(15),
             MODEL                          CHAR(15),
             VALUE                          INTEGER,
             REGNUMBER                      CHAR(7),
             COLOUR                         CHAR(8),
             CC                             SMALLINT,
             YEAROFMANUFACTURE              DATE,
             PREMIUM                        INTEGER,
             ACCIDENTS                      INTEGER
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.MOTOR                      *
      ******************************************************************
       01  DCLMOTOR.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP-5.
           10 MAKE                 PIC X(15).
           10 MODEL                PIC X(15).
           10 WS-VALUE                PIC S9(9) USAGE COMP-5.
           10 REGNUMBER            PIC X(7).
           10 COLOUR               PIC X(8).
           10 CC                   PIC S9(4) USAGE COMP-5.
           10 YEAROFMANUFACTURE    PIC X(10).
           10 PREMIUM              PIC S9(9) USAGE COMP-5.
           10 ACCIDENTS            PIC S9(9) USAGE COMP-5.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************